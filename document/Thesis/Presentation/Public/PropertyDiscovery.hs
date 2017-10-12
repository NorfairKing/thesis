{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Public.PropertyDiscovery
    ( propertyDiscovery
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets

propertyDiscovery :: Thesis
propertyDiscovery =
    section "Property Discovery" $ do
        g "Property Discovery" $ do
            only [OneSlide 1] $ raw "\\setminted{highlightlines={3,4}}"
            hask $
                T.unlines
                    [ "  forAll"
                    , "    arbitrary"
                    , "      $ \\ls ->"
                    , "        isSorted (sort ls)"
                    ]
        f "" $ center $ huge "Property Discovery with QuickSpec"
        g "Example Code" $ do
            only [OneSlide 2] $ raw "\\setminted{highlightlines={3, 12}}"
            haskFile $(embedAsset "MySort.hs")
        g "Property Discovery using QuickSpec" $ do
            footnotesize $
                mintedText $
                T.unlines
                    [ "== Signature =="
                    , "      True :: Bool"
                    , "      (<=) :: Ord a => a -> a -> Bool"
                    , "       (:) :: a -> [a] -> [a]"
                    , "    mySort :: Ord a => [a] -> [a]"
                    , "myIsSorted :: Ord a => [a] -> Bool"
                    ]
            pause
            only [OneSlide 3] $ raw "\\setminted{highlightlines={5-10}}"
            footnotesize $
                mintedText $
                T.unlines
                    [ "== Laws =="
                    , "  1. y <= y = True"
                    , "  2. y <= True = True"
                    , "  3. True <= x = x"
                    , "  4. myIsSorted (mySort xs) = True"
                    , "  5. mySort (mySort xs) = mySort xs"
                    , "  6. xs <= mySort xs = myIsSorted xs"
                    , "  7. mySort xs <= xs = True"
                    , "  8. myIsSorted (y : (y : xs)) = myIsSorted (y : xs)"
                    , "  9. mySort (y : mySort xs) = mySort (y : xs)"
                    ]
        g "QuickSpec Code" $ tiny $ haskFile $(embedAsset "MySortQuickSpec.hs")
        g "Problems with QuickSpec: Monomorphisation" $ do
            "Only for monomorphic functions"
            hask $
                T.unlines
                    [ "constant \"filter\""
                    , "  (filter :: (A -> Bool) -> [A] -> [A])"
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
