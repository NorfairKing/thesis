{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation.Experiments
    ( thesisEvaluationExperiments
    ) where

import DocImport

thesisEvaluationExperiments :: Thesis
thesisEvaluationExperiments =
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
