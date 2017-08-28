{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation.Evaluators
    ( thesisEvaluationEvaluators
    ) where

import DocImport

thesisEvaluationEvaluators :: Thesis
thesisEvaluationEvaluators =
    subsection "Evaluators" $ do
        s "It is hard to quantify which of two inference strategies is better."
        l
            [ "To define what"
            , quoted "better"
            , "means when it comes to inference strategies, we developed the the concept of an evaluator"
            ]
        s
            "We developed an evaluation framework that, for every run of EasySpec, remembers the input, the equations that were discovered, and how long the run took."
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
