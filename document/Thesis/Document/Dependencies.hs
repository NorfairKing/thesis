{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Thesis.Document.Dependencies
    ( assetRuntimeFullBackgroundPlot
    , assetRuntimeFullBreakthroughEmptyBackgroundPlot
    , assetRelevantEquationsFullBackgroundEmptyBackgroundPlot
    , assetRuntimeFullBreakthroughSyntacticSimilarityNamePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot
    , assetRuntimeFullBreakthroughSyntacticSimilaritySymbolsPlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot
    , assetRuntimeFullBreakthroughSyntacticSimilarityTypePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot
    , assetNrDifferentFunctionsPlot
    , assetRuntimeFullBreakthroughFullBreakthroughPlot
    , assetRelevantEquationsFullBreakthroughFullBreakthroughPlot
    , assetEquationsFullBreakthroughFullBreakthroughPlot
    ) where

import EasySpec.Discover.SignatureInference
import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType

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
      , ( "assetRuntimeFullBreakthroughEmptyBackgroundPlot"
        , "runtime-plot-full-background-empty-background.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferEmptyBackground)))
      , ( "assetRelevantEquationsFullBackgroundEmptyBackgroundPlot"
        , "relevant-equations-plot-full-background-empty-background.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferEmptyBackground)))
      , ( "assetRuntimeFullBreakthroughSyntacticSimilarityNamePlot"
        , "runtime-plot-full-background-syntactic-similarity-symbols-name.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferSyntacticSimilarityName inferFullBackground )))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-name.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferSyntacticSimilarityName inferFullBackground )))
      , ( "assetRuntimeFullBreakthroughSyntacticSimilaritySymbolsPlot"
        , "runtime-plot-full-background-syntactic-similarity-symbols.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferFullBackground inferSyntacticSimilaritySymbols)))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-symbols.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground inferSyntacticSimilaritySymbols)))
      , ( "assetRuntimeFullBreakthroughSyntacticSimilarityTypePlot"
        , "runtime-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferFullBackground inferSyntacticSimilarityType)))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground inferSyntacticSimilarityType)))
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
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetRelevantEquationsFullBreakthroughFullBreakthroughPlot"
        , "relevant-equations-plot-full-background-full-breakthrough.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetEquationsFullBreakthroughFullBreakthroughPlot"
        , "equations-plot-full-background-full-breakthrough.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , equationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      ])
