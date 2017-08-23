{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Reducing
    ( thesisSignatureInferenceReducing
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Document.References

thesisSignatureInferenceReducing :: Thesis
thesisSignatureInferenceReducing =
    subsection "Reducing Signature Inference Strategies" $ do
        s
            "The first attempt at speeding up the above process is to shrink the signature that is given to QuickSpec by omitting functions."
        s
            "This means that we looked for ways to select a smaller subset of the scope."
        l
            [ "The first definition of a"
            , haskInline "SignatureInferenceStrategy"
            , "looks as follows"
            ]
        haskL
            [ "type SignatureInferenceStrategy"
            , "    = [Function] -> [Function] -> [Function]"
            ]
        s
            "The first argument to a signature inference strategy is the list of focus functions, and the second argument is a list of all the functions in scope."
        s
            "The result is supposed to be a list of elements of the scope, that is smaller than the entire scope."
        s
            "This kind of signature inference strategy is sometimes called a reducing signature inference strategy."
        subsubsection "Full Background" $ do
            s
                "Running QuickSpec in an automated manner can be described as a trivial reducing signature inference strategy as follows."
            haskL
                [ "emptyBackground :: SignatureInferenceStrategy"
                , "emptyBackground focus scope = scope"
                ]
        subsubsection "Empty Background" $ do
            l
                [ "The simplest reducing signature inference strategy is called"
                , emptyBackground
                ]
            s "Its entire implementation can be written as follows."
            haskL
                [ "emptyBackground :: SignatureInferenceStrategy"
                , "emptyBackground focus scope = focus"
                ]
            l
                [ "The"
                , emptyBackground
                , "strategy will only run QuickSpec on the focus functions"
                ]
            s "As a result, it will find only relevant equations."
            s
                "However, it will find only equations that only relate the focus functions."
            s "Examples of such equations are idempotency and involution."
            question "Should I explain what idempotency and involution mean?"
