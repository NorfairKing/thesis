{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Premise
    ( thesisSignatureInferencePremise
    ) where

import DocImport

thesisSignatureInferencePremise :: Thesis
thesisSignatureInferencePremise =
    subsection "Premise" $ do
        s
            "QuickSpec requires a programmer to specify all of the context that they are interested in."
        s
            "If one were to automate the process of generating QuickSpec code from a code base, the result would discover all properties that relate any properties in that code base."
        s
            "This work, however, asserts that usually a programmer is not necessarily interested in all the equations relating the functions in the entire codebase."
        s
            "The assumption is that a programmer is more interested in the properties that involve a very small number of functions, say one."
        l ["We call these functions the", emph "focus functions"]
        s
            "The new goal is to find the properties that relate the focus functions to the rest of the codebase."
        l
            [ "We will call these"
            , emph "relevant equations"
            , "or"
            , emph "relevant properties"
            ]
        s "With respect to this new goal, QuickSpec has at least two problems."
        itemize $ do
            item $
                s
                    "QuickSpec will most likely find (a lot) of properties that do not relate the focus functions with the rest of the code base at all."
            item $
                s
                    "QuickSpec will take an immense amount of time to run, with respect to the size of the signature it is given."
        l
            [ "The question that this work tries to answer is"
            , dquoted
                  "How do we choose the inputs to give to QuickSpec intelligently, such that it will find properties of that one focus function in a practically feasible amount of time?"
            ]
        "Ideally the inputs will be chosen such that:"
        itemize $ do
            item $
                s
                    "QuickSpec finds all of the properties that relate the focus function with the rest of the codebase."
            item $
                s
                    "QuickSpec does not waste any time finding the properties of the codebase that do not involve the focus function."
