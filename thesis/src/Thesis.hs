module Thesis where

import Import

import Development.Shake
import Development.Shake.FilePath

thesis :: IO ()
thesis =
    shakeArgs shakeOptions $ do
        let proposalDir = "proposal"
            proposalOutDir = proposalDir </> "out"
            proposalTmpDir = proposalDir </> "tmp"
            proposalDocDir = proposalDir </> "doc"
            proposalOut =
                proposalOutDir </> "thesis-proposal-tom-sydney-kerckhove.pdf"
            proposalMainFileName = "proposal"
            proposalTmpOutFile = proposalMainFileName <.> "pdf"
            proposalTmpOut = proposalTmpDir </> proposalTmpOutFile
            proposalMainSrcFile = "proposal" <.> "tex"
            proposalTmpMainSource = proposalTmpDir </> proposalMainSrcFile
            proposalMainSource = proposalDocDir </> proposalMainSrcFile
        proposalOut %> \out -> copyFile' proposalTmpOut out
        proposalTmpOut %> \_ -> do
            need [proposalTmpMainSource]
            cmd (Cwd proposalTmpDir) "latexmk" "-pdf" -- Download latexmk if necessary, also download pdflatex if necessary.
        proposalTmpMainSource %> \out -> copyFile' proposalMainSource out
        want [proposalOut]
