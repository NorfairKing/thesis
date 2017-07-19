{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Automation
    ( automation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Presentation.Utils

automation :: Thesis
automation = do
    pauseSlide 1
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
                    , "    constants           :: [Constant],"
                    , "    instances           :: [[Instance]],"
                    , "    [...]"
                    , "    background          :: [Prop],"
                    , "    [...]"
                    , "  }"
                    ]
            hask $ "quickSpec :: Signature -> IO Signature"
            comment
                "QuickSpec finds all properties of all functions in the signature"
        g "Automatic Monomorphisation" $ do
            hask "filter :: (a -> Bool) -> [a] -> [a]"
            center "becomes"
            hask "filter :: (A -> Bool) -> [A] -> [A]"
            pause
            vfill
            hask "sort :: Ord a => [a] -> [a]"
            center "becomes"
            hask "sort :: Dict (Ord A) -> [A] -> [A]"
        g "Signature Expression Generation" $ do
            pause
            hask "sort :: Ord a => [a] -> [a]"
            pause
            hask "sort :: Dict (Ord A) => [A] -> [A]"
            pause
            hask $
                T.unlines
                    [ "constant \"sort\""
                    , "  (mkDict sort :: Dict (Ord A) -> [A] -> [A])"
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
