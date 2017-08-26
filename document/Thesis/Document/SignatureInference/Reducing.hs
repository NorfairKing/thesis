{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Reducing
    ( thesisSignatureInferenceReducing
    ) where

import DocImport

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
        lnbk
        s
            "Running QuickSpec on the entire scope of functions can be described as a trivial reducing signature inference strategy as follows."
        haskL
            [ "fullBackground :: SignatureInferenceStrategy"
            , "fullBackground focus scope = scope"
            ]
        l
            [ "The simplest reducing signature inference strategy is called"
            , emptyBackground
            ]
        s
            "It consists of reducing the scope to the focus and its entire implementation can be written as follows."
        haskL
            [ "emptyBackground :: SignatureInferenceStrategy"
            , "emptyBackground focus scope = focus"
            ]
        l
            [ "The"
            , emptyBackground
            , "signature inference strategy will only run QuickSpec on the focus functions"
            ]
        s "As a result, it will find only relevant equations."
        s
            "However, it will find only equations that only relate the focus functions."
        s "Examples of such equations are idempotency and involution."
        haskL
            [ "reverse (reverse x) = x -- Involution"
            , "sort (sort x) = sort x  -- Idempotency"
            ]
