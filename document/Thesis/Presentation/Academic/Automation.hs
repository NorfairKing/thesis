{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Academic.Automation
    ( automation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets

automation :: Thesis
automation = do
    f "" $ center $ huge "Property Discovery with EasySpec"
    section "Automation" $ do
        f "" $ huge $ center $ raw "Step 1: Automation"
        g "Signatures" $ do
            only [OneSlide 2] $ raw "\\setminted{highlightlines={16-26}}"
            tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")
            pause
        g "A QuickSpec Signature" $ do
            hask $
                T.unlines
                    [ "data Signature ="
                    , "  Signature {"
                    , "    functions           :: [Function],"
                    , "    [...]"
                    , "    background          :: [Prop],"
                    , "    [...]"
                    , "  }"
                    ]
            hask "quickSpec :: Signature -> IO Signature"
            comment
                "QuickSpec finds all properties of all functions in the signature"
        g "Signature Expression Generation" $ do
            pause
            hask "filter :: (a -> Bool) -> [a] -> [a]"
            pause
            hask "filter :: (A -> Bool) -> [A] -> [A]"
            pause
            hask $
                T.unlines
                    [ "function \"filter\""
                    , "  (filter :: (A -> Bool) -> [A] -> [A])"
                    ]
            pause
            hask "signature { constants = [...] }"
        g "Current situation" $ do
            mintedTextInline "$ cat Reverse.hs"
            haskFile $(embedAsset "Reverse.hs")
            vfill
            pause
            mintedTextInline "$ easyspec discover Reverse.hs"
            mintedText $
                T.unlines
                    ["reverse (reverse xs) = xs", "sort (reverse xs) = sort xs"]
