{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies

thesisSignatureInference :: Thesis
thesisSignatureInference =
    section "Signature Inference" $ do
        s "In this section I will use the concept of signature inference."
        subsection "Premise" $ do
            s
                "QuickSpec requires a programmer to specify all of the context that they are interested in."
            s
                "If one were to automate the process of generating QuickSpec code from a code base, the result would discover all properties that relate any properties in that code base."
            s
                "This work, however, asserts that usually a programmer is not necessarily in all the equations relating the functions in the entire codebase."
            s
                "The assumption is that a programmer is more interested in the properties that relate a very small number of functions, say one."
            s "These functions are called the focus functions."
            s
                "The new goal is to find the properties that relate the focus functions to the rest of the codebase."
            s "We will call these relevant equations."
            s
                "With respect to this new goal, QuickSpec has at least two problems."
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
                      "How do we choose the inputs to give to QuickSpec intelligently, such that it will find properties of that one focus function in a practical amount of time?"
                ]
            "Ideally the inputs will be chosen such that:"
            itemize $ do
                item $
                    s
                        "QuickSpec finds all of the properties that relate the focus function with the rest of the codebase."
                item $
                    s
                        "QuickSpec does not waste any time finding the properties of the codebase that do not involve the focus function."
        subsection "Solving the input problem with automation" $ do
            s
                "The first step in making property discovery feasibly for practical use, is ensuring that a programmer never has to write extra code to discover properties of their code."
            l
                [ "It involves inspecting the subject code and generating a"
                , haskInline "Signature"
                , "to run"
                , haskInline "quickSpec"
                , "on using an automated interactive evaluator"
                ]
            newline
            l
                [ "By hooking into the GHC API"
                , citationNeeded' "GHC API"
                , "one can find all functions that are in scope in a given module"
                ]
            l
                [ "Given all functions in scope, we can try to generate a"
                , haskInline "Signature"
                , "that contains all of these functions, but first they need to be monomorphised"
                ]
            s
                "Monomorphisation consists of getting rid of any type variables in the type of a function."
            subsubsection "Parameters without constraints" $ do
                l
                    [ "For type parameters of kind"
                    , haskInline "*"
                    , "in types without any constraints on the type parameters, QuickSpec has support in the form of placeholders."
                    ]
                l
                    [ "QuickSpec exposes five types"
                    , haskInline "A, B, C, D, E"
                    , "that represent type parameters"
                    ]
                l
                    [ "In reality these placeholders are just"
                    , haskInline "newtype" <> "s"
                    , "over"
                    , haskInline "Integer"
                    ]
                s
                    "This means that monomorphising such a type is as simple as turning the type parameters into the QuickSpec placeholders."
                s
                    "The following translation is an example of such a monomorphisation."
                haskL
                    [ "map :: (a -> b) -> [a] -> [b]"
                    , "       -- becomes --"
                    , "map :: (A -> B) -> [A] -> [B]"
                    ]
            subsubsection "Parameters with constraints" $ do
                s
                    "Monomorphisation is a bit more complicated in types that have type class constraints."
                s "Consider the following type."
                hask "sort :: Ord a => [a] -> [a]"
                s
                    "Given type class constraints, the previous translation would be unsound."
                s "Consider the translation as follows."
                hask "sort :: Ord A => [A] -> [A]"
                l
                    [ "There is in fact no guarantee that"
                    , haskInline "A"
                    , "is in fact in the"
                    , haskInline "Ord"
                    , "type class"
                    ]
                s
                    "The way QuickSpec solves this problem is by translating type class constraints to argument evidence dictionaries."
                s "The previous type would be translated to the following."
                hask "mkDict sort :: Dict (Ord A) -> [A] -> [A]"
                l
                    [ "Here"
                    , haskInline "Dict (Ord A)"
                    , "is the evidence dictionary for the"
                    , haskInline "Ord A"
                    , "constraint, and"
                    , haskInline "mkDict"
                    , "the function that can turn a type class constraint into an evidence dictionary argument"
                    ]
            todo "Higher kinded type parameters?"
            subsubsection "Complexity" $ do
                s
                    "An automated version of QuickSpec offers many benefits, but it does not solve the problem of computational complexity that QuickSpec exhibits."
                let automationComplexityLabel = "fig:automationComplexity"
                hereFigure $ do
                    withRegisteredAsset assetRuntimeFullBackgroundPlot $ \fp ->
                        includegraphics
                            [ KeepAspectRatio True
                            , IGWidth $ CustomMeasure textwidth
                            ]
                            fp
                    caption "Complexity of Automated QuickSpec"
                    lab automationComplexityLabel
                l
                    [ "In figure"
                    , ref automationComplexityLabel
                    , "we see the runtime of the automated version of QuickSpec with respect to the size of the scope that it was run on"
                    ]
                s
                    "The maximum size of the discovered properties was set to the same default as QuickSpec."
                let n_ = raw "N"
                    p_ = raw "P"
                l
                    [ "In fact, the complexity is of the order"
                    , m $ "O" <> pars (n_ ^: (2 <> p_))
                    , "where"
                    , m n_
                    , "is the size of the scope, and"
                    , m p_
                    , "is the maximum size of the discovered properties"
                    ]
                l
                    [ "This is still polynomial time, since we fix"
                    , m p_
                    , "to be"
                    , m 7 <> ","
                    , "but it is not practical for real code bases"
                    ]
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
            subsubsection "Empty background" $ do
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
                    lab
                        assetRelevantEquationsFullBackgroundEmptyBackgroundPlotLabel
                    caption $ l ["Relevant equations of", emptyBackground]
{-
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
                , emptyBackground
                , "strategy"
                ]
            s
                "It infers a DAG with one node, and that one node only contains the focus functions."
            s
                "This means that this strategy will only find properties that relate the focus functions."
            s "It will completely ignore the rest of the scope."
        subsection "Full background" $ do
            l ["The next simplest strategy is the", fullBackground, "strategy"]
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
-}
