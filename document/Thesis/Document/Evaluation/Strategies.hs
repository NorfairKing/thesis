{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation.Strategies
    ( thesisEvaluationStrategies
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies

thesisEvaluationStrategies :: Thesis
thesisEvaluationStrategies =
    subsection "Strategies" $ do
        subsubsection "Empty Background" $ do
            let assetRuntimeFullBackgroundEmptyBackgroundPlotLabel =
                    "fig:runtime-full-background-empty-background"
            let assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel =
                    "fig:relevant-equations-full-background-empty-background"
            l
                [ "In Figure"
                , ref assetRuntimeFullBackgroundEmptyBackgroundPlotLabel <>
                  ", the runtime of"
                , emptyBackground
                , "is plotted compared to the runtime of"
                , fullBackground
                ]
            l
                [ "Because the signature that QuickSpec is run on in"
                , emptyBackground
                , "is of constant size,"
                , emptyBackground
                , "will always run in constant time"
                ]
            l
                [ "This means that"
                , emptyBackground
                , "is a practical signature inference strategy to use"
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRuntimeFullBackgroundEmptyBackgroundPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ l ["Runtime of", emptyBackground]
                lab assetRuntimeFullBackgroundEmptyBackgroundPlotLabel
            l
                [ "However, when we look at Figure"
                , ref assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel <>
                  ", we see that"
                , emptyBackground
                , "finds almost no relevant equations"
                ]
            l
                [ "Figure"
                , ref assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel
                , "shows a box plot of the number of relevant equations found, comparing"
                , emptyBackground
                , "and"
                , fullBackground
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundEmptyBackgroundPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel
                caption $ l ["Relevant equations of", emptyBackground]
            l
                [ "Because the"
                , emptyBackground
                , "signature inference strategy only finds equations that only concern functions in focus, it misses out on most of the relevant equations"
                ]
        subsubsection "Syntactic Similarity" $ do
            let assetRuntimeFullBackgroundSyntacticSimilarityPlotLabel =
                    "fig:runtime-full-background-syntactic-similarity"
            let assetRelevantEquationsFullBackgroundSyntacticSimilarityPlotLabel =
                    "fig:relevant-equations-full-background-syntactic-similarity"
            let i_ = "i"
            l
                [ "All distance based signature inference strategies have a parameter"
                , m i_
                , "that determines the maximum size of the signature to select"
                ]
            l
                [ "We chose to fix this parameter to"
                , m 5
                , "such that the runtime of each of these signature inference strategies would remain constant as well"
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRuntimeFullBackgroundSyntacticalSimilarityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $
                    sequence_
                        [ "Runtime of the syntactic similarity signature inference strategies:"
                        , syntacticSimilarityName
                        , ", "
                        , syntacticSimilaritySymbols
                        , " and "
                        , syntacticSimilarityType
                        ]
                lab assetRuntimeFullBackgroundSyntacticSimilarityPlotLabel
            l
                [ "In Figure"
                , ref assetRuntimeFullBackgroundSyntacticSimilarityPlotLabel <>
                  ", we find the runtimes of"
                , fullBackground <> ","
                , syntacticSimilarityName <> ","
                , syntacticSimilaritySymbols
                , "and"
                , syntacticSimilarityType
                ]
            s
                "As is to be expected, all of these signature inference strategies run in constant time and are practical as such."
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticalSimilarityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityPlotLabel
                caption $
                    sequence_
                        [ "Relevant equations of the syntactic similarity signature inference strategies:"
                        , syntacticSimilarityName
                        , ", "
                        , syntacticSimilaritySymbols
                        , " and "
                        , syntacticSimilarityType
                        ]
            l
                [ "In Figure"
                , ref assetRelevantEquationsFullBackgroundSyntacticSimilarityPlotLabel <>
                  ", we find a box plot of the number of relevant equations that each of these strategies discover"
                ]
            s
                "As it turns out, these strategies already find a good number of equations."
            l
                [ "However, the way we have set up our experiments may have skewed these numbers because the number we fixed for the parameter"
                , m i_ <> ": " <> m 5
                , "is already a significant fraction of the size of the scope"
                ]
            s "This would not be the case in practice."
            s
                "It seems that choosing smaller signatures to run QuickSpec on is a good idea, but that these strategies are not ideal in deciding which functions to put in the smaller signature."
        subsubsection "Type Reachability" $ do
            l
                [ typeReachability
                , "is different from the distance based signature inference strategies because it does not guarantee that the reduced scope is any smaller than the original scope"
                ]
            l
                [ "As such, it is not guaranteed to be any faster than"
                , fullBackground
                ]
            l
                [ "This makes"
                , typeReachability
                , "infeasible for use in practical situations, but it may still be useful as a building block for better strategies"
                ]
            let assetRuntimeFullBackgroundTypeReachabilityPlotLabel =
                    "fig:runtime-full-background-type-reachability"
            hereFigure $ do
                withRegisteredAsset assetRuntimeTypeReachabilityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "The runtime of " <> typeReachability
                lab assetRuntimeFullBackgroundTypeReachabilityPlotLabel
            l
                [ "In Figure"
                , ref assetRuntimeFullBackgroundTypeReachabilityPlotLabel <>
                  ", we see that in practice"
                , typeReachability
                , "seems to reduce the scope to a sufficiently small subset that the runtime is subsequently small enough"
                ]
            let assetRelevantEquationsFullBackgroundTypeReachabilityPlotLabel =
                    "fig:relevant-equations-full-background-type-reachability"
            l
                [ "As for the discovered equations, in Figure"
                , ref assetRelevantEquationsFullBackgroundTypeReachabilityPlotLabel <>
                  ", we find that"
                , typeReachability
                , "is not better than"
                , fullBackground
                , ", but it is at least as promising as the distance based signature inference strategies"
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundReducingPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption
                    "The number of relevant equations of the reducing signature inference strategies"
                lab
                    assetRelevantEquationsFullBackgroundTypeReachabilityPlotLabel
        subsubsection "Chunks" $ do
            l [chunks, "runs QuickCheck on many signatures of constant size"]
            let s_ = "S"
                f_ = "F"
            l
                [ "To be precise,"
                , chunks
                , "runs QuickSpec"
                , "on exactly"
                , m s_
                , "signatures of size"
                , m 2
                , "and one more signature of size"
                , m f_
                ]
            l
                [ "Here,"
                , m s_
                , "is the size of the scope, and"
                , m f_
                , "is the size of the focus"
                ]
            l
                [ "As a result, we expect"
                , chunks
                , "to run in linear time with respect to the size of the scope"
                ]
            let assetRuntimeFullBackgroundChunksPlotLabel =
                    "fig:runtime-full-background-chunks"
            hereFigure $ do
                withRegisteredAsset assetRuntimeFullBackgroundChunksPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "The number of relevant equations of " <> chunks
                lab assetRuntimeFullBackgroundChunksPlotLabel
            l
                [ "When we look at Figure"
                , ref assetRuntimeFullBackgroundChunksPlotLabel <>
                  ", we see that this looks plausible"
                ]
            l
                [ "For an interactive use case,"
                , chunks
                , "may not be practical, but for a nightly use case, it may very well be"
                ]
            l
                [ "Moreover, the"
                , chunks
                , "signature inference strategy may still be a useful building block for developing better signature inference strategies"
                ]
            newline
            l
                [ "For the"
                , chunks
                , "signature inference strategy, we expected to find the equations that"
                , fullBackground
                , "finds, but only the ones that mention two or fewer distinct functions"
                ]
            let assetRelevantEquationsFullBackgroundChunksPlotLabel =
                    "fig:relevant-equations-full-background-chunks"
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundChunksPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "The number of relevant equations of " <> chunks
                lab assetRelevantEquationsFullBackgroundChunksPlotLabel
            l
                [ "However, when we look at Figure"
                , ref assetRelevantEquationsFullBackgroundChunksPlotLabel <>
                  ", we find that"
                , chunks
                , "often even finds more relevant properties than"
                , fullBackground
                , "does"
                ]
            s
                "This may seem curious at first, but it explained by the fact that QuickSpec outputs only the most general form of the properties it discovers."
            s
                "With more context, properties are more likely to have a more general property that will be discovered."
            l
                [ "If this context is omitted, such as in the case of"
                , chunks <>
                  ", QuickSpec is more likely to find multiple different relevant equations that could generalise to fewer equations if QuickSpec had more context"
                ]
            question
                "How detailed do we want the explanation as to why this happens to be?"
        subsubsection "Chunks Plus" $ do
            l
                [ "We expect the run time of"
                , chunksPlus
                , "to be quadratic in complexity"
                ]
            l
                [ "As such,"
                , chunksPlus
                , "is most likely not a practical signature inference strategy to use by itself in the use cases that we had in mind"
                ]
            l
                [ "However, as a building block,"
                , chunksPlus
                , "may still be useful"
                ]
            let assetRuntimeFullBackgroundChunksPlusPlotLabel =
                    "fig:runtime-full-background-chunks-plus"
            hereFigure $ do
                withRegisteredAsset assetRuntimeDrillingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "The number of relevant equations of " <> chunksPlus
                lab assetRuntimeFullBackgroundChunksPlusPlotLabel
            l
                [ "In Figure"
                , ref assetRuntimeFullBackgroundChunksPlusPlotLabel <>
                  ", we see that"
                , chunksPlus
                , "seems to have the complexity that we expected"
                ]
            l
                [ "This plot also confirms that"
                , chunksPlus
                , "would be impractical to use by itself"
                ]
            l
                [ "As for the relevant equations that"
                , chunksPlus
                , "finds, we expect that it finds at least as many as"
                , chunks
                , "does"
                ]
            let assetRelevantEquationsFullBackgroundChunksPlusPlotLabel =
                    "fig:relevant-equations-full-background-chunks-plus"
            hereFigure $ do
                withRegisteredAsset assetRelevantEquationsDrillingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "The number of relevant equations of " <> chunksPlus
                lab assetRelevantEquationsFullBackgroundChunksPlusPlotLabel
            l
                [ "In Figure"
                , ref assetRelevantEquationsFullBackgroundChunksPlusPlotLabel <>
                  ", we see that this is indeed the case"
                ]
            l
                [ "While the runtime of"
                , chunksPlus
                , "prohibits it from being used in practice, its time complexity is still twelve factors of the scope size faster than"
                , fullBackground
                , "is, and it regularly finds more relevant equations"
                ]
        subsubsection "Compositions" $ do
            hereFigure $
                withRegisteredAsset assetRuntimeFullChunksPlusReachabilityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            hereFigure $
                withRegisteredAsset
                    assetRelevantEquationsChunksPlusReachabilityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        subsubsection "Overview" $
            hereFigure $
            withRegisteredAsset assetRelevantEquationsAll $ \fp ->
                includegraphics
                    [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
                    fp
