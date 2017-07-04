{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.PropertyDiscovery
    ( propertyDiscovery
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets

propertyDiscovery :: Thesis
propertyDiscovery =
    section "Property Discovery" $ do
        g "Property Discovery" $ do
            only [OneSlide 1] $ raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "runMyTests :: IO ()"
                    , "runMyTests ="
                    , "  forall genInput $ \\input ->"
                    , "    result <- runMyCode input"
                    , "    itWorked <- didItWork validInput result"
                    , "    assertTrue itWorked"
                    ]
        g "Example code" $ haskFile $(embedAsset "MySort.hs")
        g "Properties" $ verbatimFile $(embedAsset "MySortQuickSpecOutput.txt")
        g "QuickSpec" $
            tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")

haskFile :: Asset -> Thesis
haskFile = mintedFile "haskell"

verbatimFile = mintedFile "text"

mintedFile :: Text -> Asset -> Thesis
mintedFile lang asset =
    withRegisteredAsset asset $ \fp ->
        comm2 "inputminted" (raw lang) $ raw $ fromString fp
