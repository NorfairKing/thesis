{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.PropertyDiscovery
    ( propertyDiscovery
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies

propertyDiscovery :: Thesis
propertyDiscovery =
    section "Property Discovery" $ do
        g "Property Discovery" $ do
            only [OneSlide 1] $ raw "\\setminted{highlightlines=4}"
            hask $
                T.unlines
                    [ "  forAll"
                    , "    arbitrary"
                    , "      $ \\ls ->"
                    , "        sort ls == ls"
                    ]
        f "" $ center $ huge "Property Discovery with QuickSpec"
        g "Example code" $ do
            raw "\\setminted{highlightlines={3, 12}}"
            haskFile $(embedAsset "MySort.hs")
        g "Property discovery using QuickSpec" $ do
            raw "\\setminted{highlightlines={12-17}}"
            footnotesize $
                verbatimFile $(embedAsset "MySortQuickSpecOutput.txt")
        g "QuickSpec Code" $ tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")
        g "Problems with QuickSpec: Monomorphisation" $ do
            "Only for monomorphic functions"
            hask $
                T.unlines
                    [ "constant \"<\""
                    , "  (mkDict (<) :: Dict (Ord A) -> A -> A -> Bool)"
                    ]
        f "Problems with QuickSpec: Code" $ do
            "Programmer has to write code for all functions of interest"
            lnbk
            withRegisteredAsset $(embedAsset "MySort.hs") $ \fp -> do
                contents <- liftIO $ readFile fp
                l
                    [ raw $ fromString $ show $ length $ lines contents
                    , "lines of subject code"
                    ]
            lnbk
            withRegisteredAsset $(embedAsset "MySortQuickSpec.hs") $ \fp -> do
                contents <- liftIO $ readFile fp
                l
                    [ raw $ fromString $ show $ length $ lines contents
                    , "lines of QuickSpec code"
                    ]
        f "Problems with QuickSpec: Speed" $ do
            "Dumb version of the QuickSpec approach:"
            enumerate $ do
                item "Generate all possible terms"
                item "Generate all possible equations (tuples) of terms"
                item "Type check them to make sure the equation makes sense"
                item
                    "Check that the input can be generated and the output compared for equality"
                item "Run QuickCheck to see if the equation holds"
