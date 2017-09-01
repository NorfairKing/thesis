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
                caption $ "Relevant equations of " <> emptyBackground
                lab assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel
            l
                [ "Because the"
                , emptyBackground
                , "signature inference strategy only finds equations that only consist of functions in focus, it misses out on most of the relevant equations"
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
                caption $
                    sequence_
                        [ "Relevant equations of the syntactic similarity signature inference strategies:"
                        , syntacticSimilarityName
                        , ", "
                        , syntacticSimilaritySymbols
                        , " and "
                        , syntacticSimilarityType
                        ]
                lab
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityPlotLabel
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
            let assetRuntimeFullBackgroundTypeReachabilityPlotLabel =
                    "fig:runtime-full-background-type-reachability"
            l
                [ "The"
                , typeReachability
                , "signature inference strategy is different from the distance based signature inference strategies because it does not guarantee that the reduced scope is any smaller than the original scope"
                ]
            l
                [ "As such, it is not guaranteed to be any faster than"
                , fullBackground
                ]
            l
                [ "This could make"
                , typeReachability
                , "infeasible for use in certain situations, but as we can see in Figure"
                , ref assetRuntimeFullBackgroundTypeReachabilityPlotLabel <>
                  ", the experiments that we used do not cause"
                , typeReachability
                , "to exhibit this problem"
                ]
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
                [ "we see that in practice"
                , typeReachability
                , "reduces the scope to a sufficiently small subset such that the runtime is subsequently small enough to be practically feasible"
                ]
            let assetRelevantEquationsFullBackgroundTypeReachabilityPlotLabel =
                    "fig:relevant-equations-full-background-type-reachability"
            l
                [ "As for the discovered equations, in Figure"
                , ref assetRelevantEquationsFullBackgroundTypeReachabilityPlotLabel <>
                  ", we find that"
                , typeReachability
                , "is not better than"
                , fullBackground <> ","
                , "but it is at least as promising as the distance based signature inference strategies"
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
            l
                [ "The"
                , chunks
                , "signature inference strategy runs QuickCheck on many signatures of constant size"
                ]
            let s_ = "S"
                f_ = "F"
            l
                [ "To be precise,"
                , chunks
                , "runs QuickSpec"
                , "on exactly"
                , m $ s_ * f_
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
                , "to have a linear discovery complexity with respect to the size of the scope"
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
                [ "Figure"
                , ref assetRuntimeFullBackgroundChunksPlotLabel <>
                  "confirms this expectation"
                ]
            l
                [ "For the interactive use case,"
                , chunks
                , "may not be practical, but for the nightly use case, it could be"
                ]
            s
                "A user would have to evaluate whether the strategy is fast enough for their use case."
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
                [ "However, Figure"
                , ref assetRelevantEquationsFullBackgroundChunksPlotLabel <>
                  ", shows that"
                , chunks
                , "often even finds even more relevant properties than"
                , fullBackground
                , "does"
                ]
            s
                "This may seem curious at first, but it is explained by the fact that QuickSpec outputs only the most general properties it discovers."
            s
                "With a larger scope, general properties that subsume others are more likely to be discovered."
            s
                "With more scope, properties are more likely to have a more general property that will be discovered."
            l
                [ "If the context is reduced, such as in the case of"
                , chunks <>
                  ", QuickSpec is more likely to find multiple different relevant equations that could generalise to fewer equations if QuickSpec had more context"
                ]
            l ["As an example, consider the following scope"]
            haskL
                [ "a :: Int -> Int"
                , "a = (+1)"
                , "b :: Int -> Int"
                , "b = (+2)"
                , "c :: Int -> Int"
                , "c = (+3)"
                , "d :: Int -> Int"
                , "d = (+4)"
                ]
            l
                [ "When we run"
                , fullBackground
                , "on this scope, we find the following equations"
                ]
            haskL ["a (a x) = b x", "a (b x) = c x", "a (c x) = d x"]
            l
                [ "If we chose"
                , haskInline "d"
                , "as the focus, only the last equation would be considered relevant"
                ]
            l
                [ "This means that"
                , fullBackground
                , "only finds one relevant equations"
                ]
            l
                [ "When we run"
                , chunks
                , "on this scope with focus"
                , haskInline "d"
                , "we find the following relevant equations"
                ]
            haskL ["b (b x) = d x", "a (a (a (a x))) = d x"]
            l
                [ "Note that neither of these equations are found in the"
                , fullBackground
                , "results, because they are more specific than some of the equations that"
                , fullBackground
                , "finds"
                ]
            l
                [ "The reduction of the scope caused"
                , chunks
                , "to find more relevant equations than"
                , fullBackground
                , "did"
                ]
        subsubsection "Chunks Plus" $ do
            l
                [ "We expect"
                , chunksPlus
                , "to have a quadratic discovery complexity"
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
                , "has the complexity that we expected"
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
            let chunksReducingsRuntimeLabel = "fig:chunks-reducings-runtime"
            l
                [ "In Figure"
                , ref chunksReducingsRuntimeLabel <>
                  ", we see that all the compositions of a reducing with"
                , chunks
                , "yield a signature inference strategies that runs in a practically feasible amount of time, even"
                , chunksTypeReachability
                ]
            hereFigure $ do
                withRegisteredAsset assetRuntimeChunksReducingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksReducingsRuntimeLabel
            let chunksReducingsRelevantEquationsLabel =
                    "fig:chunks-reducings-relevant-equations"
            l
                [ "In Figure"
                , lab chunksReducingsRelevantEquationsLabel <>
                  ", we see that these composed strategies all approach"
                , fullBackground
                , "in terms of how many relevant equations they find"
                ]
            hereFigure $ do
                withRegisteredAsset assetRelevantEquationsChunksReducingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksReducingsRelevantEquationsLabel
            let chunksPlusReducingsRuntimeLabel =
                    "fig:chunks-plus-reducings-runtime"
            l
                [ "As we can see in Figure"
                , ref chunksPlusReducingsRuntimeLabel <>
                  ", composing the same reducings with a more intensive drilling:"
                , chunksPlus
                , "produces signature inference strategies that still run in a practical amount of time, but still a greater amount of time, even"
                , chunksPlusTypeReachability
                ]
            hereFigure $ do
                withRegisteredAsset assetRuntimeChunksPlusReducingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksPlusReducingsRuntimeLabel
            let chunksPlusReducingsRelevantEquationsLabel =
                    "fig:chunks-plus-reducings-relevant-equations"
            l
                [ "In Figure"
                , ref chunksPlusReducingsRelevantEquationsLabel
                , "we find that these these compositions regularly outperform"
                , fullBackground
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsChunksPlusReducingsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksPlusReducingsRelevantEquationsLabel
            s
                "The compositions of two reducings with a drilling produced less successful signature inference strategies than expected."
            let chunksPlusReachabilitiesRuntimeLabel =
                    "fig:chunks-plus-reachabilities-runtime"
            l
                [ "As we can see in Figure"
                , ref chunksPlusReachabilitiesRuntimeLabel <>
                  ", these strategies still ran in a practical amount of time"
                ]
            hereFigure $ do
                withRegisteredAsset assetRuntimeFullChunksPlusReachabilityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksPlusReachabilitiesRuntimeLabel
            let chunksPlusReachabilitiesRelevantEquationsLabel =
                    "fig:chunks-plus-reachabilities-relevant-equations"
            l
                [ "However, if we look at Figure"
                , ref chunksPlusReachabilitiesRelevantEquationsLabel <>
                  ", we see that these more complex compositions do not outperform their simpler variants"
                ]
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsChunksPlusReachabilityPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab chunksPlusReachabilitiesRelevantEquationsLabel
        subsubsection "Overview" $ do
            let overviewRelevantEquationsLabel =
                    "fig:overview-relevant-equations"
            l
                [ "In Figure"
                , ref overviewRelevantEquationsLabel <>
                  ", there is an overview of the performance, with respect to the number of relevant equations found, of all the signature inference strategies that we studied"
                ]
            hereFigure $ do
                withRegisteredAsset assetRelevantEquationsAll $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab overviewRelevantEquationsLabel
            l ["The best performing signature inference strategy is clearly", chunksPlus, "and, while it is still significantly faster than", fullBackground <> ", it may not run in a practically feasible amount of time"]
            l ["Of the signature inference strategies that run in a feasible amount of time,", chunksPlusTypeReachability, "finds the most relevant equations"]
            l ["If one needs a constant time guarantee, then", chunksPlusSimilarityType,"or", chunksPlusSimilaritySymbols, "are viable alternatives"]
