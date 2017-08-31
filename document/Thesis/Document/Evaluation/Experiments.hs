{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation.Experiments
    ( thesisEvaluationExperiments
    ) where

import DocImport

import Thesis.Document.References

thesisEvaluationExperiments :: Thesis
thesisEvaluationExperiments =
    subsection "Experiments" $ do
        s
            "To evaluate EasySpec and the different signature inference strategies, we performed multiple experiments."
        s
            "In a perfect world, we would run EasySpec on real world Haskell code."
        s
            "However, Haskell is a large language when it comes to syntax, and the GHC compiler extensions make the language bigger."
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
        l
            [ "The code samples can be found in the EasySpec repository,"
            , cite easyspecGithubRef
            , "and consist of the following major groups"
            ]
        itemize $ do
            item $
                l
                    [ mintedTextInline "Bools.hs"
                    , "consists of Boolean functions, operators, and list functions that use Boolean filter functions"
                    ]
            item $
                l
                    [ mintedTextInline "Monoid.hs"
                    , "consists of integer functions, operators and constants"
                    ]
            item $
                l
                    [ mintedTextInline "DNA.hs"
                    , "consists of functions that deal with strings characters"
                    ]
        s
            "These examples are together comprehensive in the sense that they contain all the types that QuickSpec supports by default."
        s
            "We made a separate group of code samples to evaluate the runtime aspect of signature inference strategies."
        l
            [ "These files consist of three locally defined functions that resemble"
            , haskInline "id" <> ","
            , haskInline "++"
            , "and"
            , haskInline "reverse"
            , "and an increasing scope of prelude functions around them"
            ]
