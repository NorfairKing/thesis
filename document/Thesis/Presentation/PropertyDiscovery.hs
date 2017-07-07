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
            only [OneSlide 1] $ raw "\\setminted{highlightlines=8}"
            hask $
                T.unlines
                    [ "runMyTests :: IO ()"
                    , "runMyTests ="
                    , "  forAll"
                    , "    arbitrary"
                    , "      $ \\ls ->"
                    , "        sort ls"
                    , "          `shouldSatisfy`"
                    , "            isSorted"
                    ]
        g "Example code" $ haskFile $(embedAsset "MySort.hs")
        g "Properties" $
            footnotesize $
            verbatimFile $(embedAsset "MySortQuickSpecOutput.txt")
        g "QuickSpec" $ tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")
        g "Problems with QuickSpec: monomorphisation" $ do
            "Only for monomorphic functions"
            hask $
                T.unlines
                    [ "constant \"<\" (mkDict (<) :: Dict (Ord A) -> A -> A -> Bool)"
                    ]
        f
            "Problems with QuickSpec: code"
            "Programmer has to write code for all functions of interest"
        f "Problems with QuickSpec: speed" $ do
            "QuickSpec is slow"
            pause
            "Dumb version of the approach:"
            enumerate $ do
                item "Generate all possible terms"
                item "Generate all possible equations (tuples) of terms"
                item "Type check them to make sure the equation makes sense"
                item
                    "Check that the input can be generated and the output compared for equality"
                item "Run QuickCheck to see if the equation holds"
        f "Problems with QuickSpec: speed" $
            center $
            withRegisteredAsset assetRuntimePlot $ \fp ->
                includegraphics
                    [ KeepAspectRatio True
                    , IGWidth $ CustomMeasure $ "0.64" <> textwidth
                    ]
                    fp

haskFile :: Asset -> Thesis
haskFile = mintedFile "haskell"

verbatimFile :: Asset -> Thesis
verbatimFile = mintedFile "text"

mintedFile :: Text -> Asset -> Thesis
mintedFile lang asset =
    withRegisteredAsset asset $ \fp ->
        comm2 "inputminted" (raw lang) $ raw $ fromString fp
