{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Thesis.Document.Dependencies
    ( assetRuntimePlot
    ) where

import EasySpec.Discover.SignatureInference

import EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Evaluate.Evaluator

import Thesis.Document.Assets
import Thesis.Document.Dependencies.TH

$(makeDependencyAssets
      [ ( "assetRuntimePlot"
        , "runtime-plot.png"
        , plotFileFor
              barsPerGroupEvaluatorsStrategyPlotter
              ( "runtime"
              , inferFullBackground
              , UnorderedDistinct scopeSizeEvaluator runtimeEvaluator))
      ])
