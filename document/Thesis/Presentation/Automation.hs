{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Automation
    ( automation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies

automation :: Thesis
automation = do
    section "Definitions" $ do
        f "" $ huge $ center $ raw "Step 0: Definitions"
        comment
            "This was not step 0 in our research, but for explanations, it helps."
        g "Definitions: Property" $ do
            vfill
            " Example:"
            hask "reverse (reverse ls) = ls"
            vfill
            " Short for:"
            hask "(\\ls -> reverse (reverse ls)) = (\\ls -> ls)"
            vfill
            " In general:"
            hask $
                T.unlines
                    [ "(f :: A -> Bool) = (g :: A -> Bool)"
                    , "for some A with"
                    , "instance Arbitrary A"
                    ]
            vfill
        g "Definitions: Property of a function" $ do
            "Functions:"
            hask $ T.unlines ["f = (* 2)", "g = (* 3)", "z = 0"]
            vfill
            "Properties of " <> haskInline "f" <> ":"
            hask $ T.unlines ["f (g x) = g (f x)", "f z = z"]
            "Not properties of " <> haskInline "f" <> ":"
            hask $ T.unlines ["g z = z"]
        g "Definitions: Relevant function" $ do
            "Functions:"
            hask $ T.unlines ["f = (* 2)", "g = (* 3)", "z = 0", "h = id"]
            "Properties:"
            hask $
                T.unlines
                    ["f (g x) = g (f x)", "f z = z", "g z = z", "id x = x"]
            vfill
            haskInline "g" <> " and " <> haskInline "z" <> " are relevant to " <>
                haskInline "f" <>
                " but " <>
                "h" <>
                " is not."
    section "Automation" $ do
        f "" $ huge $ center $ raw "Step 1: Automation"
        g "Signatures" $ tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")
        g "A QuickSpec Signature" $ do
            hask $
                T.unlines
                    [ "data Signature ="
                    , "  Signature {"
                    , "    constants           :: [Constant],"
                    , "    instances           :: [[Instance]],"
                    , "    -- [...]"
                    , "    background          :: [Prop],"
                    , "  }"
                    ]
            hask $ "quickSpec :: Signature -> IO Signature"
            comment
                "QuickSpec finds all properties of all functions in the signature"
        g "Automatic Monomorphisation" $ do
            hask "filter :: (a -> Bool) -> [a] -> [a]"
            center "becomes"
            hask "filter :: (A -> Bool) -> [A] -> [A]"
            vfill
            hask "sort :: Ord a => [a] -> [a]"
            center "becomes"
            hask "sort :: Dict (Ord A) => [A] -> [A]"
        g "Signature Expression Generation" $ do
            enumerate $ do
                item "Find all functions in scope"
                hask "sort :: Ord a => [a] -> [a]"
                item "Make them Monomorphic"
                hask "sort :: Dict (Ord A) => [A] -> [A]"
                item "Make expressions for QuickSpec"
                hask $
                    T.unlines
                        [ "constant \"sort\""
                        , "  (mkDict sort :: Dict (Ord A) -> [A] -> [A])"
                        ]
                item "Make a signature expression"
        g "Current situation" $ do
            mintedTextInline "$ cat Reverse.hs"
            haskFile $(embedAsset "Reverse.hs")
            vfill
            mintedTextInline "$ easyspec discover Reverse.hs"
            mintedText $
                T.unlines
                    ["reverse (reverse xs) = xs", "sort (reverse xs) = sort xs"]
