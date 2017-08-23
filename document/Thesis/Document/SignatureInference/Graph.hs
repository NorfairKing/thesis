{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Graph
    ( thesisSignatureInferenceGraph
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies

thesisSignatureInferenceGraph :: Thesis
thesisSignatureInferenceGraph =
    subsection "Graph Signature Inference Strategies" $ do
        subsubsection "Motivation" $ do
            s
                "During experimentation, we tried looking for clues to build better strategies."
            s
                "One of the measures we looked at, was the number of different functions that occur in any given property."
            l
                [ "We ran"
                , fullBackground
                , "on many examples, and gathered the following data"
                ]
            let differentFunctionsPerPropertyLabel =
                    "fig:different-functions-per-property"
            hereFigure $ do
                withRegisteredAsset assetNrDifferentFunctionsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption "Number of different functions that occur in a property"
                lab differentFunctionsPerPropertyLabel
            l
                [ "In figure"
                , ref differentFunctionsPerPropertyLabel
                , "there is a histogram of the number of different functions in an equation"
                ]
            l
                [ "We notice that"
                , m 60
                , "and"
                , m 70
                , "per cent of all properties that were discovered by"
                , fullBackground
                , "only talk about two or fewer functions"
                ]
            l
                [ "Similarly, we find that almost"
                , m 90
                , "per cent of all properties only talk about three or fewer functions"
                ]
            s
                "This means that it does not make sense to run QuickSpec on large signatures."
            s
                "The complexity of running QuickSpec suggests that it is cheaper to run QuickSpec multiple times with different small signatures than to run QuickSpec on a large signature."
            s
                "This is when we realised that we should try running QuickSpec more than once."
        subsubsection "Definition" $ do
            s
                "The definition of a signature inference strategy would have to be changed to allow for multiple runs of QuickSpec."
            s
                "The first attempt involved defining a signature inference strategy as a reducing signature inference strategy that produces a list of signatures instead of a single signature."
            haskL
                [ "type SignatureInferenceStrategy"
                , "    = [Function] -> [Function] -> InferredSignature"
                , ""
                , "type InferredSignature = [Signature]"
                ]
            s
                "A list signature inference strategy, as these were named, could be run very similarly to a regular reducing signature inference strategy."
            s
                "QuickSpec is run on each of the different signatures that is inferred, and the resulting equations are collected as the result."
            s
                "Note that any reducing signature inference strategy can be trivially converted to a reducing signature inference strategy."
            newline
            s
                "In the next iteration of this idea, we recognised that QuickSpec has a feature that allows it to learn from previous discoveries."
            todo $
                s
                    "Ensure that this is described properly, either here or in the QuickSpec section."
            s
                "We adapted our definition to allow for dependencies between signatures by arranging the resulting signatures in a forest."
            hask "type InferredSignature = Forest Signature"
            s
                "Now we could describe the idea that every signature had to be run before its parent could be run, and QuickSpec would do the appropriate optimisation."
            s
                "Again, it is trivial to convert a list signature inference strategy into a forest signature inference strategy."
            s
                "Next, we noticed that we may as well allow signature inference strategies to share children in the forest."
            s
                "That's why we adapted our definition again, this time to arrange signatures in a directed acyclic graph."
            hask "type InferredSignature = DAG Signature"
            s
                "Note that any tree is a directed acyclic graph as well, so translations were trivial again."
            s
                "At this point, signature inference strategies were strictly more expressive, which allowed for more intricate signature inference strategies."
        subsubsection "Chunks" $ do
            s
                "Using the newfound knowledge that properties usually contain very few different functions, we set out to create a signature inference strategy that takes advantage of this fact."
            s
                "It sets out to find the properties that contain two or fewer different functions."
            s
                "As such, it first creates a node that only contains the focus functions."
            s
                "Next, it creates a signature for every tuple of one focus function and one scope function and adds the initial node as a dependency."
            s
                "The resulting directed acyclic graph of signatures is star shaped and only contains signatures with two or fewer functions."
            todo $
                raw
                    "A nice illustration of chunks, a graphviz graph or something."
