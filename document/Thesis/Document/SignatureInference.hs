{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

import qualified Data.Text as T

thesisSignatureInference :: Thesis
thesisSignatureInference = do
    section "Signature Inference" $ do
        s "In this section I will use the concept of signature inference."
        subsection "Premise" $ do
            s
                "Signature inference is best introduced by explaining the thought process that lead to its definition."
            s
                "QuickSpec requires a set of functions, and discovers all the equations that relate them."
            s
                "This work, however, asserts that usually a programmer is not necessarily in all the equations relating all the functions in the entire codebase."
            s
                "The assumption is that a programmer is more interested in the properties that relate a very small number of functions, say one."
            s "This function is called the focus function."
            s
                "The new goal is to find the properties that relate the focus functions to the rest of the codebase."
            s
                "With respect to this new goal, QuickSpec has at least two problems."
            itemize $ do
                item $
                    s
                        "QuickSpec will most likely find (a lot) of properties that do not relate the focus functions at all."
                item $
                    s
                        "QuickSpec will take an immense amount of time to run, with respect to the size of the signature it is given."
            l
                [ "The question that this work tries to answer is"
                , dquoted
                      "How do we choose the inputs to give to QuickSpec intelligently, such that it will find properties of that one focus function in a reasonable amount of time?"
                ]
            "Ideally the inputs will be chosen such that:"
            itemize $ do
                item $
                    s
                        "QuickSpec finds all of the properties that relate the focus function with the rest of the codebase."
                item $
                    s
                        "QuickSpec does not waste any time finding the properties of the codebase that do not involve the focus function."
        subsection "Signature inference strategies" $ do
            s
                "A signature inference strategy is the general data type that will drive signature inference."
            s
                "It is defined to contain a function that infers signatures from two pieces of data, The focus functions and the complete scope of functions that are available."
            s
                "The type signature of a signature inference strategy looks a bit like the following."
            hask "[Id] -> [Id] -> InferredSignature"
            l
                [ "Here,"
                , haskInline "Id"
                , "is type of which the values represent Haskell functions"
                ]
            l
                [ "In particular, an"
                , haskInline "Id"
                , "contains the name and type of the corresponding Haskell function"
                ]
            l
                [ "A value of type"
                , haskInline "InferredSignature"
                , "is not a signature for QuickSpec"
                ]
            l
                [ "In fact, an"
                , haskInline "InferredSignature"
                , "more closely resembles a directed acyclic graph where nodes represent signatures"
                ]
        subsection "Running QuickSpec on an inferred signature" $ do
            l
                [ "Given an"
                , haskInline "InferredSignature" <> ","
                , "QuickSpec is run as follows"
                ]
            s "First, the signatures are sorted topologically."
            s "Next, QuickSpec is run on each node."
            s
                "In every node of the graph, the equations that are discovered by QuickSpec at the nodes that the node has edges to, are added to the signature in that node as background properties."
            s
                "After running QuickSpec on each node like that, the equations from all nodes without any incoming edges are combined into the final output."
    section "Basic inference strategies" $ do
        subsection "Empty background" $ do
            l
                [ "The simplest inference strategy, is the"
                , mintedTextInline "empty-background"
                , "strategy"
                ]
            s
                "It infers a DAG with one node, and that one node only contains the focus functions."
            s
                "This means that this strategy will only find properties that relate the focus functions."
            s "It will completely ignore the rest of the scope."
        subsection "Full background" $ do
            l
                [ "The next simplest strategy is the"
                , mintedTextInline "full-background"
                , "strategy"
                ]
            s "This strategy resembles the workings of QuickSpec the best."
            s
                "It also infers a DAG with one node, but this time that node contains the focus functions and also the entire scope."
            s
                "It will find all of the properties that relate any of the functions in scope."
    section "Evaluation of inference strategies" $ do
        s
            "In this section I will explain how difference inference strategies can be evaluated objectively."
        s "It is hard to quantify which of two inference strategies is better."
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
        hask $
            T.unlines
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
