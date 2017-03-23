{-# LANGUAGE RecordWildCards #-}

module Thesis.LaTeX
    ( LaTeXRulesSpec(..)
    , LaTeXRunConfig(..)
    , wantLaTeX
    , pdfOutFiles
    , simpleLaTeXRules
    ) where

import Import

import qualified System.FilePath as FP

import Development.Shake
import Development.Shake.Path

import Thesis.GitInfo
import Thesis.Utils

data LaTeXRulesSpec = LaTeXRulesSpec
    { latexTopDir :: Path Rel Dir
    , latexRunConfigs :: [LaTeXRunConfig]
    , latexMainTexFileName :: Path Rel File
    } deriving (Show, Eq)

data LaTeXRunConfig = LaTeXRunConfig
    { latexPdfOutFile :: Path Rel File
    , latexInternalFlags :: [String]
    } deriving (Show, Eq)

wantLaTeX :: LaTeXRulesSpec -> Rules ()
wantLaTeX = wantP . pdfOutFiles

pdfOutFiles :: LaTeXRulesSpec -> [Path Rel File]
pdfOutFiles LaTeXRulesSpec {..} =
    map ((outDir </>) . latexPdfOutFile) latexRunConfigs

simpleLaTeXRules :: LaTeXRulesSpec -> Rules ()
simpleLaTeXRules LaTeXRulesSpec {..} = do
    here <- liftIO getCurrentDir
    let docDir :: Path Rel Dir
        docDir = latexTopDir
    forM_ latexRunConfigs $ \LaTeXRunConfig {..} -> do
        let outFile = latexPdfOutFile
        let outPath = outDir </> outFile
        let tmpOut :: Path Rel File
            tmpOut = tmpDir </> outFile
        expandedFile <-
            liftIO $ (tmpDir </>) <$> setFileExtension ".tex" outFile
        expandedFile $%> do
            fs <-
                fmap (filter (not . hidden)) $
                liftIO $ snd <$> listDirRecur docDir
            needP fs
            needP
                [ gitHashFile
                , gitBranchFile
                , gitCommitCountFile
                , gitCommitDateFile
                ]
            forM_ fs $ \f ->
                case stripDir (here </> docDir) f of
                    Nothing -> fail "Should not happen."
                    Just fn -> do
                        let res = here </> tmpDir </> fn
                        ensureDir $ parent res
                        copyFile f $ tmpDir </> fn
            putLoud $
                unwords
                    [ "Expanding"
                    , toFilePath latexMainTexFileName
                    , "to"
                    , toFilePath expandedFile
                    ]
            cmd
                "m4"
                (Cwd $ toFilePath tmpDir)
                (FileStdout $ toFilePath expandedFile)
                (map (\v -> "--define=" ++ v) latexInternalFlags)
                (toFilePath latexMainTexFileName)
        tmpOut $%> do
            needP [expandedFile]
            cmd
                (Cwd $ toFilePath tmpDir)
                "latexmk"
                "-pdf"
                "-shell-escape"
                "-halt-on-error"
                (toFilePath $ filename expandedFile) -- Download latexmk if necessary, also download pdflatex if necessary.
                ["-jobname=" ++ FP.dropExtensions (toFilePath outFile)]
        outPath `byCopying` tmpOut

byCopying :: Path r File -> Path s File -> Rules ()
byCopying to from =
    to $%> do
        needP [from]
        putLoud $ unwords ["Copying", toFilePath from, "to", toFilePath to]
        copyFile from to

hidden :: Path r File -> Bool
hidden f = ".swp" `isSuffixOf` toFilePath f
