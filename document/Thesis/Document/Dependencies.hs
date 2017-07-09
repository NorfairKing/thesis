{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Thesis.Document.Dependencies
    ( assetRuntimeFullBackgroundPlot
    , assetNrDifferentFunctionsPlot
    , assetRuntimeFullBreakthroughFullBreakthroughPlot
    , assetRelevantEquationsFullBreakthroughFullBreakthroughPlot
    , assetEquationsFullBreakthroughFullBreakthroughPlot
    ) where

import EasySpec.Discover.SignatureInference

import EasySpec.Discover.SignatureInference.FullBreakthrough
import EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Types

import Thesis.Document.Assets
import Thesis.Document.Dependencies.TH

$(makeDependencyAssets
      [ ( "assetRuntimeFullBackgroundPlot"
        , "runtime-plot.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategyPlotter
              ( "runtime"
              , inferFullBackground
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)))
      , ( "assetNrDifferentFunctionsPlot"
        , "evaluation-nr-different-functions.pdf"
        , plotFileFor
              (dfrgCartPlotter
                   @(GroupName, SignatureInferenceStrategy)
                   dfrgNrDifferentFunctions)
              ("evaluation", inferFullBackground))
      , ( "assetRuntimeFullBreakthroughFullBreakthroughPlot"
        , "runtime-plot-full-background-full-breakthrough.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferFullBackground (inferFullBreakthrough 1))))
      , ( "assetRelevantEquationsFullBreakthroughFullBreakthroughPlot"
        , "relevant-equations-plot-full-background-full-breakthrough.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground (inferFullBreakthrough 1))))
      , ( "assetEquationsFullBreakthroughFullBreakthroughPlot"
        , "equations-plot-full-background-full-breakthrough.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , equationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground (inferFullBreakthrough 1))))
      ])
