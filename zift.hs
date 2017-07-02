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
        preprocessor $ ziftP [hindentZift, cabalFormatZift]
        prechecker gitAddAllZift
        checker $ do
            do rd <- getRootDir
               Stdout out1 <-
                   liftIO $
                   cmd (Cwd $ toFilePath rd) "stack" "build" "thesis:exe:thesis"
               printZift out1
               Stdout out2 <-
                   liftIO $
                   cmd
                       (Cwd $ toFilePath rd)
                       "stack"
                       "path"
                       "--local-install-root"
               printZift out2
               Stdout out3 <-
                   liftIO $
                   cmd
                       (Cwd $ toFilePath rd)
                       (init out2 ++ "/bin/thesis")
                       "build"
               printZift out3
            hlintZift
            stackBuildZift
