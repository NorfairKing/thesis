{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Thesis.Document.Dependencies
    ( assetRuntimePlot
    ) where

import EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Evaluate.Evaluator

import Thesis.Document.Assets
import Thesis.Document.Dependencies.TH

$(makeDependencyAssets
      [ ( "assetRuntimePlot"
        , "runtime-plot.png"
        , plotFileFor
              barsPerGroupEvaluatorsPlotter
              ("runtime", UnorderedDistinct scopeSizeEvaluator runtimeEvaluator))
      ])
