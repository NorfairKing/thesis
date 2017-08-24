{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation
    ( thesisEvaluation
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies

thesisEvaluation :: Thesis
thesisEvaluation =
    section "Evaluation" $ do
        s
            "In this section I will evaluate the different signature inference strategies that were introduced in the previous section."
        subsection "Evaluators" $ do
            s
                "It is hard to quantify which of two inference strategies is better."
            l
                [ "In fact, it is so hard to define what"
                , quoted "better"
                , "means when it comes to inference strategies, that the concept of an evaluator was developed"
                ]
            s
                "For every run of EasySpec, the evaluation framework remembers the input to EasySpec, the equations that were discovered, and how long the run took."
            l
                [ "An evaluator has a name and a way to create a"
                , haskInline "Maybe Double"
                , ", given this information about a run of EasySpec"
                ]
            hask "type Evaluator = EvaluationInput -> Maybe Double"
            s "We define the following evaluators."
            itemize $ do
                item $
                    mintedTextInline "equations" <>
                    ": The number of equations that were found"
                item $
                    mintedTextInline "runtime" <>
                    ": The amount of time that the run took"
                item $
                    mintedTextInline "relevant-equations" <>
                    ": The number of relevant equations that were found"
                item $
                    mintedTextInline "relevant-functions" <>
                    ": The number of relevant functions that were found"
                item $
                    mintedTextInline "equations-minus-relevant-equations" <>
                    ": The number of irrelevant equations that were found"
                item $
                    mintedTextInline "relevant-equations-divided-by-runtime" <>
                    ": The number of relevant equations found per unit of time"
            s
                "In practice, what we are looking for is a strategy that runs in a practical amount of time."
            s "What we mean by practical depends on the use case."
            s "We had two use cases in mind."
            newline
            s
                "In the first use case, EasySpec would be run at night, to find gaps in a test suite."
            s
                "In this use case, we envision that a linear time complexity would be feasible, but anything worse than that would be impractical."
            newline
            s
                "In the second use case, EasySpec would be an interactive assistant that can suggest tests for code that is being written as it is being written."
            s
                "For this purpose, anything slower than a logarithmic time complexity is out of the question, and even a logarithmic time complexity may be infeasible for large code bases."
            newline
            s
                "Obviously constant factors matter as well, in discussions of complexity."
            newline
            s
                "A signature inference strategy should not just be fast, but it should also find relevant properties."
            s
                "In practice we only care about relevant properties, but it is important to note that properties take time to discover."
            s
                "This means that we look for a strategy that find many relevant properties, and ideally few irrelevant properties and therefore few properties beyond the relevant properties."
            newline
            l
                [ "While a measure like"
                , relevantEquationsDividedByRuntime
                , "can be a useful tool for evaluation, there is no single evaluator to rank signature inference strategies by"
                ]
            l
                [ "The best way to evaluate signature inference strategies that we have found is to first decide if the runtime is practical, and among practical signature inference strategies, choose the signature inference strategy with highest score for"
                , relevantEquations
                ]
        subsection "Experiments" $ do
            s
                "To evaluate EasySpec and the different signature inference strategies, we performed multiple experiments."
            s
                "In a perfect world, we would run EasySpec on real world Haskell code."
            s
                "However, Haskell is a giant language syntax wise, and the GHC compiler extensions make the language bigger."
            s
                "Due to time constraints, EasySpec could not be readied for use on real world code."
            s
                "As a result, experiments had to be performed on subject code that was limited to the part of the language that is currently supported."
            l
                [ "We put together example code with as large a scope as"
                , fullBackground
                , "could handle with a diverse mixture of functions and types"
                ]
            s
                "For each of these examples, for each signature inference strategy and for each function in the module, EasySpec is run using that function as the focus."
            question "How much detail is needed in this section?"
        subsection "Strategies" $ do
            subsubsection "Empty Background" $ do
                let assetRuntimeFullBackgroundEmptyBackgroundPlotLabel =
                        "fig:runtime-full-background-empty-background"
                let assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel =
                        "fig:relevant-equations-full-background-empty-background"
                l
                    [ "In figure"
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
                    [ "However, when we look at figure"
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
                    lab
                        assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel
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
                    [ "In figure"
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
                    [ "In figure"
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
                hereFigure $ do
                    withRegisteredAsset assetRuntimeTypeReachabilityPlot $ \fp ->
                        includegraphics
                            [ KeepAspectRatio True
                            , IGWidth $ CustomMeasure textwidth
                            ]
                            fp
                todo "continue here"
                hereFigure $ do
                    withRegisteredAsset
                        assetRelevantEquationsFullBackgroundTypeReachabilityPlot $ \fp ->
                        includegraphics
                            [ KeepAspectRatio True
                            , IGWidth $ CustomMeasure textwidth
                            ]
                            fp
            subsubsection "Chunks" $ do
                l
                    [ chunks
                    , "runs QuickCheck on many signatures of constant size"
                    ]
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
                    [ "When we look at figure"
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
                    [ "However, when we look at figure"
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
                    withRegisteredAsset assetRuntimeFullBackgroundChunksPlusPlot $ \fp ->
                        includegraphics
                            [ KeepAspectRatio True
                            , IGWidth $ CustomMeasure textwidth
                            ]
                            fp
                    caption $
                        "The number of relevant equations of " <> chunksPlus
                    lab assetRuntimeFullBackgroundChunksPlusPlotLabel
                l
                    [ "In figure"
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
                    withRegisteredAsset
                        assetRelevantEquationsFullBackgroundChunksPlusPlot $ \fp ->
                        includegraphics
                            [ KeepAspectRatio True
                            , IGWidth $ CustomMeasure textwidth
                            ]
                            fp
                    caption $
                        "The number of relevant equations of " <> chunksPlus
                    lab assetRelevantEquationsFullBackgroundChunksPlusPlotLabel
                l
                    [ "In figure"
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

-- equations :: Thesis
-- equations = mintedTextInline "equations"
-- runtime :: Thesis
-- runtime = mintedTextInline "runtime"
relevantEquations :: Thesis
relevantEquations = mintedTextInline "relevant-equations"

-- relevantFunctions :: Thesis
-- relevantFunctions = mintedTextInline "relevant-functions"
-- equationsMinusRelevantEquations :: Thesis
-- equationsMinusRelevantEquations =
--     mintedTextInline "equations-minus-relevant-equations"
relevantEquationsDividedByRuntime :: Thesis
relevantEquationsDividedByRuntime =
    mintedTextInline "relevant-equations-divided-by-runtime"
