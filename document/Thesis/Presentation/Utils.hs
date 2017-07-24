{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Utils where

import PresImport

import qualified Data.Text as T
import System.FilePath

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Beamer as X

import Thesis.Document.Assets

pauseSlide :: Int -> Thesis
pauseSlide i =
    g "Pause slide with a joke" $
    hask $
    case i of
        1 -> T.unlines ["strictId :: a -> a", "strictId !x = x"]
        2 ->
            T.unlines
                [ "safePerformIO :: IO a -> IO a"
                , "safePerformIO ioa = ioa >>= return"
                ]
        3 -> T.unlines ["safeCoerce :: a ~ b => a -> b", "safeCoerce x = x"]

lightbulbslide :: Thesis
lightbulbslide =
    f "Idea" $
    figure (Just Center) $
    withRegisteredAsset $(embedAsset "lightbulb.png") $ \fp ->
        includegraphics
            [KeepAspectRatio True, IGWidth $ CustomMeasure $ "0.3" <> textwidth]
            fp
