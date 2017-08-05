{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Document.References

thesisSignatureInference :: Thesis
thesisSignatureInference =
    section "Signature Inference" $ do
        s "In this section I will introduce the concept of signature inference."
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
                , cite ghcAPIRef
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
                    , footnote
                          "Note that this is sound because values of a parametric type cannot be inspected at all."
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
            subsubsection "Full background" $ do
                s
                    "Running QuickSpec in an automated manner can be described as a trivial reducing signature inference strategy as follows."
                haskL
                    [ "emptyBackground :: SignatureInferenceStrategy"
                    , "emptyBackground focus scope = scope"
                    ]
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
                l
                    [ "The"
                    , emptyBackground
                    , "strategy will only run QuickSpec on the focus functions"
                    ]
                s "As a result, it will find only relevant equations."
                s
                    "However, it will find only equations that only relate the focus functions."
                s "Examples of such equations are idempotency and involution."
                question
                    "Should I explain what idempotency and involution mean?"
            subsection "Distance-based reducing signature inference strategies" $ do
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
                    , footnote
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
                    caption
                        "General distance based signature inference strategy"
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
                    s
                        "For each type, we define a multiset of parts of that type."
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
                    s
                        "The resulting signature inference strategy looks as follows."
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
                    caption
                        "Number of different functions that occur in a property"
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
                todo
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
            subsubsection "Chunks" $
                s
                    "Using the newfound knowledge that properties usually contain very few different functions, we set out to create a signature inference strategy that takes advantage of this fact."
