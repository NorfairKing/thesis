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
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

import Thesis.Zift

main :: IO ()
main =
    ziftWith $ do
        thesisZiftScript
        recursiveZift
        preprocessor $ ziftP [hindentZift, cabalFormatZift]
        prechecker gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
