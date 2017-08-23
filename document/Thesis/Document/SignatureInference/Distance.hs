{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Distance
    ( thesisSignatureInferenceDistance
    ) where

import DocImport

thesisSignatureInferenceDistance :: Thesis
thesisSignatureInferenceDistance =
    subsection "Distance-based Reducing Signature Inference Strategies" $ do
        s
            "Given a general distance between functions, we can construct a reducing signature inference strategy."
        s
            "The idea is that we may be able to use a distance to predict whether functions will be relevant to each other."
        s
            "If this is true, then the functions that are closest to the focus functions should be chosen to run QuickSpec on."
        s
            "A general distance based signature inference strategy has one parameter, namely the number of functions to choose around the focus functions."
        let i_ = "i"
        l ["We will call this parameter", m i_]
        l
            [ "For each function in scope, the distance to the focus function is calculated and the"
            , m i_
            , "closed functions in scope are put together in the signature"
            ]
        l
            [ "The following piece of code exhibits this principle"
            , footnote $
              s
                  "This piece of code assumes that there is only a single focus function. In practice this is a valid assumption, but this code could also be extended to work on larger foci using a summation."
            ]
        hereFigure $ do
            haskL
                [ "distanceBased"
                , "    :: (Function -> Function -> Double)"
                , "    -> Int -> SignatureInference"
                , "distanceBased dist i [focus] scope"
                , "    = take i"
                , "    $ sortOn"
                , "        (\\sf -> dist focus sf)"
                , "        scope"
                ]
            caption "General distance based signature inference strategy"
        subsubsection "Syntactic Similarity Name" $ do
            l
                [ "The first distance based reducing signature inference strategy is called"
                , syntacticSimilarityName
                ]
            s
                "It is based on the assumption that mutual relevancy of functions can be predicted by the similarity of their names."
            l
                [ "For example, the functions"
                , haskInline "isPrime :: Int -> Bool"
                , "and"
                , haskInline "primeAtIndex :: Int -> Int"
                , "are most likely relevant to each other, and we may be able to predict this fact because the names both mention the word"
                , dquoted "prime"
                ]
            s
                "Because EasySpec has access to compile time information about code, it can introspect the name of functions."
            s
                "The following is pseudo code to define this signature inference strategy."
            hereFigure $ do
                haskL
                    [ "inferSyntacticSimilarityName"
                    , "    :: Int -> SignatureInference"
                    , "inferSyntacticSimilarityName"
                    , "    = distanceBased"
                    , "        (\\ff sf ->"
                    , "            hammingDistance (name ff) (name sf))"
                    ]
                caption syntacticSimilarityName
        subsubsection "Syntactic Similarity Symbols" $ do
            l
                [ "A second distance based reducing signature inference strategy is called"
                , syntacticSimilaritySymbols
                ]
            s
                "This strategy looks at the implementation of functions to determine a distance."
            s
                "It is based on the assumption that mutually relevant functions will have a similar looking implementation when it comes to the functions that they use."
            l
                [ "To use the same example,"
                , haskInline "isPrime :: Int -> Bool"
                , "and"
                , haskInline "primeAtIndex :: Int -> Int"
                , "both probably use"
                , haskInline "div :: Int -> Int -> Int"
                , "and"
                , haskInline "mod :: Int -> Int -> Int"
                , "and should therefore be judged to be close to each other"
                ]
            l
                [ syntacticSimilaritySymbols
                , "defines the distance between two functions as the hamming distance between the symbol vectors of these functions"
                ]
            hereFigure $ do
                haskL
                    [ "inferSyntacticSimilaritySymbols"
                    , "    :: Int -> SignatureInference"
                    , "inferSyntacticSimilaritySymbols"
                    , "    = distanceBased"
                    , "        (\\ff sf ->"
                    , "            hammingDistance (symbols ff) (symbols sf))"
                    ]
                caption syntacticSimilaritySymbols
        subsubsection "Syntactic Similarity Type" $ do
            l
                [ "A final second distance based reducing signature inference strategy is called"
                , syntacticSimilarityType
                ]
            s
                "This strategy is based on the assumption that functions that are relevant to each other will have similar types."
            l
                [ "In the example of"
                , haskInline "isPrime :: Int -> Bool"
                , "and"
                , haskInline "primeAtIndex :: Int -> Int"
                , "both of the types of these functions mention the type"
                , haskInline "Int"
                ]
            s "For each type, we define a multiset of parts of that type."
            l
                [ "For example, the type"
                , haskInline "[a] -> [a]"
                , "has the following parts multiset:"
                ]
            hereFigure $
                mintedTextL
                    [ "[a] -> [a] : 1"
                    , "([a] ->)   : 1"
                    , "(-> [a])   : 1"
                    , "(->)       : 2"
                    , "[a]        : 2"
                    , "a          : 2"
                    ]
            s
                "This multiset is interpreted as a vector in an infinitely dimensional vector space."
            s
                "In this space, the hamming distance between two vectors is used as the distance between two functions."
            s "The resulting signature inference strategy looks as follows."
            hereFigure $ do
                haskL
                    [ "inferSyntacticSimilarityType"
                    , "    :: Int -> SignatureInference"
                    , "inferSyntacticSimilarityType"
                    , "    = distanceBased"
                    , "        (\\ff sf ->"
                    , "            hammingDistance (typeParts ff) (typeParts sf))"
                    ]
                caption syntacticSimilarityType
