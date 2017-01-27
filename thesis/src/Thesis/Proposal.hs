module Thesis.Proposal where

import Import

import Development.Shake
import Development.Shake.FilePath

proposalRules :: Rules ()
proposalRules = do
    proposalTmpMainSource %> \out -> copyFile' proposalMainSource out
    proposalTmpMainBibliography %> \out ->
        copyFile' proposalMainBibliography out
    proposalTmpOut %> \_ -> do
        need [proposalTmpMainSource, proposalTmpMainBibliography]
        cmd (Cwd proposalTmpDir) "latexmk" "-pdf" "-shell-escape" -- Download latexmk if necessary, also download pdflatex if necessary.
    proposalOut %> \out -> copyFile' proposalTmpOut out

proposalDir :: FilePath
proposalDir = "proposal"

proposalOutDir :: FilePath
proposalOutDir = proposalDir </> "out"

proposalTmpDir :: FilePath
proposalTmpDir = proposalDir </> "tmp"

proposalDocDir :: FilePath
proposalDocDir = proposalDir </> "doc"

proposalOut :: FilePath
proposalOut = proposalOutDir </> "thesis-proposal-tom-sydney-kerckhove.pdf"

proposalMainFileName :: FilePath
proposalMainFileName = "proposal"

proposalTmpOutFile :: FilePath
proposalTmpOutFile = proposalMainFileName <.> "pdf"

proposalTmpOut :: FilePath
proposalTmpOut = proposalTmpDir </> proposalTmpOutFile

proposalMainSrcFile :: FilePath
proposalMainSrcFile = proposalMainFileName <.> "tex"

proposalTmpMainSource :: FilePath
proposalTmpMainSource = proposalTmpDir </> proposalMainSrcFile

proposalMainSource :: FilePath
proposalMainSource = proposalDocDir </> proposalMainSrcFile

proposalMainBibFile :: FilePath
proposalMainBibFile = proposalMainFileName <.> "bib"

proposalTmpMainBibliography :: FilePath
proposalTmpMainBibliography = proposalTmpDir </> proposalMainBibFile

proposalMainBibliography :: FilePath
proposalMainBibliography = proposalDocDir </> proposalMainBibFile
