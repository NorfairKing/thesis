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
            "In this section I will evaluate different signature inference strategies."
        subsection "Empty background" $ do
            l
                [ "The simplest reducing signature inference strategy is called"
                , emptyBackground
                ]
            s "Its entire implementation can be written as follows."
            haskL
                [ "emptyBackground :: SignatureInferenceStrategy"
                , "emptyBackground focus scope = focus"
                ]
            let assetRuntimeFullBackgroundEmptyBackgroundPlotLabel =
                    "fig:runtime-full-background-empty-background"
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
            let assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel =
                    "fig:relevant-equations-full-background-empty-background"
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
            let assetRuntimeFullBackgroundSyntacticSimilarityNamePlotLabel =
                    "fig:runtime-full-background-syntactic-similarity-name"
            hereFigure $ do
                withRegisteredAsset
                    assetRuntimeChunksSyntacticSimilarityNamePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ l ["Runtime of", syntacticSimilarityName]
                lab assetRuntimeFullBackgroundSyntacticSimilarityNamePlotLabel
            let assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlotLabel =
                    "fig:relevant-equations-full-background-syntactic-similarity-name"
            hereFigure $ do
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                lab
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlotLabel
                caption $ l ["Relevant equations of", syntacticSimilarityName]
            s
                "It is hard to quantify which of two inference strategies is better."
            l
                [ "In fact, it is so hard to define what"
                , quoted "better"
                , "means when it comes to inference strategies, that the concept of evaluators was developed"
                ]
            s
                "For every run of EasySpec, the evaluation framework remembers the input to EasySpec, the equations that were discovered, and how long the run took."
            l
                [ "An evaluator has a name and a way to create a"
                , haskInline "Maybe Double"
                , ", given this information about a run of EasySpec"
                ]
            haskL
                [ "data Evaluator = Evaluator"
                , "    { name :: String"
                , "    , evaluator :: EvaluationInput -> Maybe Double"
                , "    }"
                ]
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
