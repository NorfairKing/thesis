{-# LANGUAGE RecordWildCards #-}

module Thesis.LaTeX
    ( LaTeXRulesSpec(..)
    , wantLaTeX
    , pdfOutFile
    , simpleLaTeXRules
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.Constants
import Thesis.Utils

data LaTeXRulesSpec = LaTeXRulesSpec
    { latexTopDir :: Path Rel Dir
    , latexPdfOutFile :: Path Rel File
    , latexMainTexFileName :: Path Rel File
    } deriving (Show, Eq)

wantLaTeX :: LaTeXRulesSpec -> Rules ()
wantLaTeX = wantP . (: []) . pdfOutFile

pdfOutFile :: LaTeXRulesSpec -> Path Rel File
pdfOutFile LaTeXRulesSpec {..} = outDir </> latexPdfOutFile

simpleLaTeXRules :: LaTeXRulesSpec -> Rules ()
simpleLaTeXRules spec@LaTeXRulesSpec {..} = do
    here <- liftIO getCurrentDir
    let docDir :: Path Rel Dir
        docDir = latexTopDir
        outFile :: Path Rel File
        outFile = pdfOutFile spec
    tmpOutFile <- liftIO $ latexMainTexFileName <.> pdfExt
    let tmpOut :: Path Rel File
        tmpOut = tmpDir </> tmpOutFile
    tmpOut $%> do
        fs <- liftIO $ snd <$> listDirRecur docDir
        needP fs
        forM_ fs $ \f ->
            case stripDir (here </> docDir) f of
                Nothing -> pure ()
                Just fn -> copyFile f $ tmpDir </> fn
        cmd
            (Cwd $ toFilePath tmpDir)
            "latexmk"
            "-pdf"
            "-shell-escape"
            "-halt-on-error"
            (toFilePath latexMainTexFileName) -- Download latexmk if necessary, also download pdflatex if necessary.
    outFile `byCopying` tmpOut

byCopying :: Path r File -> Path s File -> Rules ()
byCopying to from =
    to $%> do
        needP [from]
        copyFile from to
