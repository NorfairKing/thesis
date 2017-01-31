{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.LaTeX
    ( LaTeXRulesSpec(..)
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

pdfOutFile :: LaTeXRulesSpec -> Path Rel File
pdfOutFile LaTeXRulesSpec {..} =
    latexTopDir </> $(mkRelDir "out") </> latexPdfOutFile

simpleLaTeXRules :: LaTeXRulesSpec -> Rules ()
simpleLaTeXRules spec@LaTeXRulesSpec {..} = do
    here <- liftIO getCurrentDir
    let dir :: Path Rel Dir
        dir = latexTopDir
        tmpDir :: Path Rel Dir
        tmpDir = dir </> $(mkRelDir "tmp")
        docDir :: Path Rel Dir
        docDir = dir </> $(mkRelDir "doc")
        outFile :: Path Rel File
        outFile = pdfOutFile spec
    tmpOutFile <- liftIO $ latexMainTexFileName <.> pdfExt
    let tmpOut :: Path Rel File
        tmpOut = tmpDir </> tmpOutFile
    mainSrcFile <- liftIO $ latexMainTexFileName <.> texExt
    let tmpMainSource :: Path Rel File
        tmpMainSource = tmpDir </> mainSrcFile
        mainSource :: Path Rel File
        mainSource = docDir </> mainSrcFile
    mainBibFile <- liftIO $ latexMainTexFileName <.> bibExt
    let tmpMainBibliography :: Path Rel File
        tmpMainBibliography = tmpDir </> mainBibFile
        mainBibliography :: Path Rel File
        mainBibliography = docDir </> mainBibFile
    tmpMainSource `byCopying` mainSource
    tmpMainBibliography `byCopying` mainBibliography
    tmpOut $%> do
        fs <- liftIO $ snd <$> listDirRecur docDir
        needP $ fs ++ map (here </>) [tmpMainSource, tmpMainBibliography]
        forM_ fs $ \f ->
            case stripDir (here </> docDir) f of
                Nothing -> pure ()
                Just fn -> copyFile f $ tmpDir </> fn
        cmd
            (Cwd $ toFilePath tmpDir)
            "latexmk"
            "-pdf"
            "-shell-escape"
            "-halt-on-error" -- Download latexmk if necessary, also download pdflatex if necessary.
    outFile `byCopying` tmpOut

byCopying :: Path r File -> Path s File -> Rules ()
byCopying to from =
    to $%> do
        needP [from]
        copyFile from to
