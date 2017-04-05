#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-git
    --package zifter-hindent
    --package zifter-stack
    --package zifter-hlint
    --package path
    --package shake
-}
import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

import Development.Shake
import Path

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor $ ziftP [hindentZift, cabalFormatZift]
        prechecker gitAddAllZift
        checker $ do
            do rd <- getRootDir
               Stdout out1 <-
                   liftIO $
                   cmd (Cwd $ toFilePath rd) "stack" "install" ":thesis"
               printZift out1
                -- This automatically runs it in the weird tmp dir, so we make sure that it works anywhere.
               Stdout out2 <- liftIO $ cmd "thesis" "build"
               printZift out2
            hlintZift
            stackBuildZift
