{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Thesis.Document.Dependencies
    ( assetRuntimeFullBackgroundPlot
    , assetRuntimeFullBackgroundEmptyBackgroundPlot
    , assetRelevantEquationsFullBackgroundEmptyBackgroundPlot
    , assetRuntimeChunksSyntacticSimilarityNamePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot
    , assetRuntimeChunksSyntacticSimilaritySymbolsPlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot
    , assetRuntimeChunksSyntacticSimilarityTypePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot
    , assetNrDifferentFunctionsPlot
    , assetRuntimeFullBackgroundChunksPlot
    , assetRelevantEquationsFullBackgroundChunksPlot
    , assetEquationsFullBackgroundChunksPlot
    , assetRuntimeFullBackgroundChunksPlusPlot
    , assetRelevantEquationsFullBackgroundChunksPlusPlot
    , assetEquationsFullBackgroundChunksPlusPlot
    ) where

import EasySpec.Discover.SignatureInference
import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.ChunksPlus
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
      , ( "assetRuntimeFullBackgroundEmptyBackgroundPlot"
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
      , ( "assetRuntimeChunksSyntacticSimilarityNamePlot"
        , "runtime-plot-full-background-syntactic-similarity-symbols-name.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferSyntacticSimilarityName inferFullBackground)))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-name.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferSyntacticSimilarityName inferFullBackground)))
      , ( "assetRuntimeChunksSyntacticSimilaritySymbolsPlot"
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
      , ( "assetRuntimeChunksSyntacticSimilarityTypePlot"
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
      , ( "assetRuntimeFullBackgroundChunksPlot"
        , "runtime-plot-full-background-chunks.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetRelevantEquationsFullBackgroundChunksPlot"
        , "relevant-equations-plot-full-background-chunks.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetEquationsFullBackgroundChunksPlot"
        , "equations-plot-full-background-chunks.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , equationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetRuntimeFullBackgroundChunksPlusPlot"
        , "runtime-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( "runtime"
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      , ( "assetRelevantEquationsFullBackgroundChunksPlusPlot"
        , "relevant-equations-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      , ( "assetEquationsFullBackgroundChunksPlusPlot"
        , "equations-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( "evaluation"
              , equationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      ])
