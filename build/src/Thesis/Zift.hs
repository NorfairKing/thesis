module Thesis.Zift
    ( thesisZiftScript
    ) where

import Import

import Development.Shake

import Zifter

thesisZiftScript :: ZiftScript ()
thesisZiftScript =
    checker $ do
        rd <- getRootDir
        Stdout out1 <-
            liftIO $ cmd (Cwd $ toFilePath rd) "stack" "install" ":thesis"
        printZift out1
        -- This automatically runs it in the weird tmp dir, so we make sure that it works anywhere.
        Stdout out2 <- liftIO $ cmd "thesis" "build"
        printZift out2
