{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.TypeReachability
    ( thesisSignatureInferenceTypeReachability
    ) where

import DocImport

thesisSignatureInferenceTypeReachability :: Thesis
thesisSignatureInferenceTypeReachability =
    subsection "Type Reachability" $ do
        s
            "A more sophisticated signature inference strategy is based on how functions can be composed."
        l
            [ "This signature inference strategy is called"
            , typeReachability
            , "and it is a reducing signature inference strategy"
            ]
        s
            "It uses type information to try to rule out parts of the scope from being relevant to the focus."
        let typeReachabilityExampleLabel = "fig:type-type-reachability-example"
        subsubsection "Motivation" $ do
            s
                "The idea is to determine which functions could possibly be composed with the focus functions to produce an equation."
            s "Consider the following example of a scope."
            hereFigure $ do
                haskL
                    [ "f :: Int -> Char"
                    , "g :: () -> Int"
                    , "h :: Char -> Double"
                    , "i :: Bool -> Bool"
                    , "j :: Double -> Double"
                    ]
                lab typeReachabilityExampleLabel
                caption $
                    "An example scope in which " <> typeReachability <>
                    " would work well"
            l
                [ "If the focus consists of the single function"
                , haskInline "f"
                , "then, without looking at their implementation, we can already say that"
                , haskInline "i"
                , "can never occur in an equation together with"
                , haskInline "f"
                ]
            l
                [ "This is because there is no way to make terms on the left and right hand side of the equation that both mention"
                , haskInline "f"
                , "and have a sub term of type"
                , haskInline "Bool"
                ]
            l
                [ "The intention of the"
                , typeReachability
                , "signature inference strategy is to eliminate functions from the scope that cannot occur together in an equation with any focus functions"
                ]
            s
                "To see why this situation occurs, we have to explore a concept that we have called type reachability."
        subsubsection "Definition" $ do
            s "Type reachability is defined recursively as follows."
            enumerate $ do
                item $
                    s
                        "Every function is type reachable from itself in zero steps."
                item $ do
                    s
                        "If two functions can make up an equation by each occurring on either side of the equation, then they are type reachable from each other in one step."
                    lnbk
                    s
                        "This means that the types of these two functions must be unifiable."
                    l
                        [ "For example, the functions"
                        , haskInline "id :: a -> a"
                        , "and"
                        , haskInline "reverse :: [a] -> [a]"
                        , "are type reachable from each other because the equation"
                        , haskInline "id xs = reverse xs"
                        , "typechecks"
                        ]
                    s "Note that this equation does not have to hold."
                item $ do
                    s
                        "If the output of one function can be used as an input to a second function, then these functions are type reachable from each other in one step."
                    l
                        [ "For example, the functions"
                        , haskInline "(+) :: Num a => a -> a -> a"
                        , "and"
                        , haskInline "take :: Int -> [a] -> [a]"
                        , "are type reachable from each other because they can be composed as"
                        , haskInline "take (a + b) xs"
                        ]
                    s
                        "Note that there can be multiple possible input and output types of a function."
                    l
                        [ "The function"
                        , haskInline "(+)"
                        , "can have both"
                        , haskInline "a"
                        , "and"
                        , haskInline "a -> a"
                        , "as an output type due to currying"
                        ]
                    l
                        [ "The function"
                        , haskInline "take"
                        , "can have both"
                        , haskInline "Int"
                        , "and"
                        , haskInline "a"
                        , "as an input type due to the fact that it has multiple arguments"
                        ]
                item $ do
                    let a = "a"
                        b = "b"
                    l
                        [ "If a function"
                        , haskInline "f"
                        , "is type reachable from a function"
                        , haskInline "g"
                        , "in"
                        , m a
                        , "steps, and a third function"
                        , haskInline "h"
                        , "is reachable from"
                        , haskInline "g"
                        , "in"
                        , m b
                        , "steps, then we say that"
                        , haskInline "f"
                        , "is type reachable from"
                        , haskInline "h"
                        , "in"
                        , m $ a + b
                        , "steps"
                        ]
            l
                [ "Consider the example in Figure"
                , ref typeReachabilityExampleLabel
                , "again"
                ]
            l
                [ "From the function"
                , haskInline "f" <> ", the functions"
                , haskInline "g"
                , "and"
                , haskInline "h"
                , "are type reachable in one step"
                ]
            l
                [ "The function"
                , haskInline "j"
                , "is type reachable in two steps, and"
                , haskInline "i"
                , "is not type reachable from"
                , haskInline "f"
                , "at all"
                ]
        subsubsection "The Type Reachability Strategy" $ do
            let i = "i"
            l
                [ "The idea of"
                , typeReachability
                , "is to find all functions in scope that are type reachable from the focus functions in less than"
                , m i
                , "steps, where"
                , m i
                , "is a parameter of the strategy"
                ]
            s
                "It is important to note that we only use an under approximation for real type reachability."
            s
                "This underapproximation only deals with monomorphic types, and only considers the first argument and the last output of functions."
            l
                [ "This limitation allows us to implement the under approximation function"
                , haskInline "typeReachableInOneStep"
                , "without integrating with any type checker"
                ]
            hereFigure $ do
                haskL
                    [ "inferTypeReachability :: Int -> SignatureInferenceStrategy"
                    , "inferTypeReachability i focus scope = go i focus"
                    , "  where"
                    , "    go 0 acc = acc"
                    , "    go n acc = go (n - 1) $"
                    , "      concatMap"
                    , "        (\\af -> filter (typeReachableInOneStep af) focus)"
                    , "        acc"
                    ]
                caption typeReachability
            l
                [ "In this work, we investigate this strategy with"
                , m 7
                , "as the parameter"
                , m i <>
                  ", because this is the maximum size of properties that we use"
                ]
