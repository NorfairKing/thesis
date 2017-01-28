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
    tmpMainSource $%> copyFile mainSource tmpMainSource
    tmpMainBibliography $%> copyFile mainBibliography tmpMainBibliography
    tmpOut $%> do
        needP [tmpMainSource, tmpMainBibliography]
        cmd (Cwd $ toFilePath tmpDir) "latexmk" "-pdf" "-shell-escape" -- Download latexmk if necessary, also download pdflatex if necessary.
    outFile $%> copyFile tmpOut outFile
