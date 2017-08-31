{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document.SignatureInference.Graph
    ( thesisSignatureInferenceGraph
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Document.References
import Thesis.Document.Sections

thesisSignatureInferenceGraph :: Thesis
thesisSignatureInferenceGraph =
    subsection "Graph Signature Inference Strategies" $ do
        s
            "During experimentation, we searched for clues to build better strategies."
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
                    [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
                    fp
            caption "Number of different functions that occur in a property"
            lab differentFunctionsPerPropertyLabel
        l
            [ "In Figure"
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
            "This means that it may not always be worthwhile to run QuickSpec on large signatures."
        s
            "The runtime of running QuickSpec suggests that it is cheaper to run QuickSpec multiple times with different small signatures than to run QuickSpec on a large signature."
        subsubsection "Definition" $ do
            s
                "To allow for multiple runs of QuickSpec, the definition of a signature inference strategy needs to be changed."
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
            l
                [ "In the next iteration of this idea, we observed that QuickSpec has a feature that allows it to learn from previous discoveries as alluded to in section"
                , ref propertyDiscoveryBackgroundSection
                ]
            lab strategyWithBackgroundSection
            l
                [ "Recall that a signature contains a field called"
                , haskInline "background"
                , "that allows a user to specify previously discovered properties"
                ]
            s
                "The result of running QuickSpec is a signature in which this field has been filled with the properties that were discovered in that run."
            s
                "In subsequent runs, the user can then specify these discovered properties in the signatures to run QuickSpec on, and QuickSpec will take these properties into account for optimisation of further discovery."
            l
                [ "For more details, refer to the second QuickSpec paper"
                , cite quickspec2Ref
                ]
            lnbk
            s
                "We adapted our definition to allow for dependencies between signatures by arranging the resulting signatures in a forest."
            hask "type InferredSignature = Forest Signature"
            s
                "Now we could describe the idea that every signature had to be run before its parent could be run, and QuickSpec would perform the appropriate optimisation."
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
            lab chunksSection
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
            let chunksExampleLabel = "fig:chunks-example"
            l
                [ "As an example, consider the scope in figure"
                , ref chunksExampleLabel <> ", and choose"
                , haskInline "sort"
                , "as the focus"
                ]
            hereFigure $ do
                haskL
                    [ "sort :: Ord a => [a] -> [a]"
                    , "reverse :: [a] -> [a]"
                    , "id :: a -> a"
                    , "not :: Bool -> Bool"
                    ]
                caption "An example scope"
                lab chunksExampleLabel
            let chunksExampleGraphLabel = "fig:chunks-example-graph"
            l
                [ "The"
                , chunks
                , "signature inference strategy will first run QuickSpec on a signature that only contains"
                , haskInline "sort" <>
                  ", and then on the signatures with two functions as depicted in figure"
                , ref chunksExampleGraphLabel
                ]
            hereFigure $ do
                withDotAsset $(embedAsset "chunks.dot") $ \fp ->
                    center $
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $
                    sequence_
                        ["The graph that ", chunks, "builds for the example"]
                lab chunksExampleGraphLabel
