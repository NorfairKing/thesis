{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Thesis.Document.Dependencies
    ( assetRuntimeFullBackgroundPlot
    , assetRuntimeFullBackgroundEmptyBackgroundPlot
    , assetRelevantEquationsFullBackgroundEmptyBackgroundPlot
    , assetRelevantEquationsFullBackgroundSyntacticalSimilarityPlot
    , assetRuntimeFullBackgroundSyntacticalSimilarityPlot
    , assetRuntimeChunksSyntacticSimilarityNamePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot
    , assetRuntimeChunksSyntacticSimilaritySymbolsPlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot
    , assetRuntimeChunksSyntacticSimilarityTypePlot
    , assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot
    , assetRuntimeTypeReachabilityPlot
    , assetRelevantEquationsFullBackgroundTypeReachabilityPlot
    , assetRelevantEquationsFullBackgroundReducingPlot
    , assetNrDifferentFunctionsPlot
    , assetRuntimeFullBackgroundChunksPlot
    , assetRelevantEquationsFullBackgroundChunksPlot
    , assetEquationsFullBackgroundChunksPlot
    , assetRuntimeFullBackgroundChunksPlusPlot
    , assetRelevantEquationsFullBackgroundChunksPlusPlot
    , assetEquationsFullBackgroundChunksPlusPlot
    , assetRuntimeAll
    , assetRelevantEquationsAll
    ) where

import EasySpec.Discover.SignatureInference
import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.ChunksPlus
import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.TypeReachability

import EasySpec.Evaluate.Analyse.Data.Common
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
              ( runtimeGroup
              , inferFullBackground
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)))
      , ( "assetRuntimeFullBackgroundEmptyBackgroundPlot"
        , "runtime-plot-full-background-empty-background.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferEmptyBackground)))
      , ( "assetRelevantEquationsFullBackgroundEmptyBackgroundPlot"
        , "relevant-equations-plot-full-background-empty-background.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferEmptyBackground)))
      , ( "assetRelevantEquationsFullBackgroundSyntacticalSimilarityPlot"
        , "relevant-equations-plot-full-background-syntactical-similarity.pdf"
        , plotFileFor
              boxPlotterPerGroupEvaluatorOnDemand
              ( evaluationGroup
              , relevantEquationsEvaluator
              , [ inferFullBackground
                , inferSyntacticSimilarityName 5
                , inferSyntacticSimilaritySymbols 5
                , inferSyntacticSimilarityType 5
                ]))
      , ( "assetRuntimeFullBackgroundSyntacticalSimilarityPlot"
        , "runtime-plot-full-background-syntactical-similarity.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotterOnDemand
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , [ inferFullBackground
                , inferSyntacticSimilarityName 5
                , inferSyntacticSimilaritySymbols 5
                , inferSyntacticSimilarityType 5
                ]))
      , ( "assetRuntimeChunksSyntacticSimilarityNamePlot"
        , "runtime-plot-full-background-syntactic-similarity-symbols-name.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair (inferSyntacticSimilarityName 5) inferFullBackground)))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-name.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair (inferSyntacticSimilarityName 5) inferFullBackground)))
      , ( "assetRuntimeChunksSyntacticSimilaritySymbolsPlot"
        , "runtime-plot-full-background-syntactic-similarity-symbols.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair
                         inferFullBackground
                         (inferSyntacticSimilaritySymbols 5))))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-symbols.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair
                         inferFullBackground
                         (inferSyntacticSimilaritySymbols 5))))
      , ( "assetRuntimeChunksSyntacticSimilarityTypePlot"
        , "runtime-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferFullBackground (inferSyntacticSimilarityType 5))))
      , ( "assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground (inferSyntacticSimilarityType 5))))
      , ( "assetRuntimeTypeReachabilityPlot"
        , "runtime-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct
                    (Pair inferFullBackground (inferTypeReachability 7))))
      , ( "assetRelevantEquationsFullBackgroundTypeReachabilityPlot"
        , "relevant-equations-plot-full-background-syntactic-similarity-type.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct
                    (Pair inferFullBackground (inferTypeReachability 7))))
      , ( "assetRelevantEquationsFullBackgroundReducingPlot"
        , "relevant-equations-plot-full-background-Reducing.pdf"
        , plotFileFor
              boxPlotterPerGroupEvaluatorOnDemand
              ( evaluationGroup
              , relevantEquationsEvaluator
              , [ inferFullBackground
                , inferSyntacticSimilarityName 5
                , inferSyntacticSimilaritySymbols 5
                , inferSyntacticSimilarityType 5
                , inferTypeReachability 7
                ]))
      , ( "assetNrDifferentFunctionsPlot"
        , "evaluation-nr-different-functions.pdf"
        , plotFileFor
              (dfrgCartPlotter
                   @(GroupName, SignatureInferenceStrategy)
                   dfrgNrDifferentFunctions)
              (evaluationGroup, inferFullBackground))
      , ( "assetRuntimeFullBackgroundChunksPlot"
        , "runtime-plot-full-background-chunks.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetRelevantEquationsFullBackgroundChunksPlot"
        , "relevant-equations-plot-full-background-chunks.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetEquationsFullBackgroundChunksPlot"
        , "equations-plot-full-background-chunks.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , equationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunks)))
      , ( "assetRuntimeFullBackgroundChunksPlusPlot"
        , "runtime-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsStrategiesPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      , ( "assetRelevantEquationsFullBackgroundChunksPlusPlot"
        , "relevant-equations-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , relevantEquationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      , ( "assetEquationsFullBackgroundChunksPlusPlot"
        , "equations-plot-full-background-chunks-plus.pdf"
        , plotFileFor
              boxPlotterPerEvaluatorStrategies
              ( evaluationGroup
              , equationsEvaluator
              , OrderedDistinct (Pair inferFullBackground inferChunksPlus)))
      , ( "assetRuntimeAll"
        , "runtime-all.pdf"
        , plotFileFor
              barsPerGroupEvaluatorsPlotter
              ( runtimeGroup
              , IndepDepPairEvaluator (Pair scopeSizeEvaluator runtimeEvaluator)))
      , ( "assetRelevantEquationsAll"
        , "relevant-equations-all.pdf"
        , plotFileFor
              boxPlotterPerEvaluator
              (evaluationGroup, relevantEquationsEvaluator))
      ])
