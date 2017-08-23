{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Automation
    ( thesisSignatureInferenceAutomation
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Document.References

thesisSignatureInferenceAutomation :: Thesis
thesisSignatureInferenceAutomation =
    subsection "Automation" $ do
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
        subsubsection "Parameters without Constraints" $ do
            l
                [ "For type parameters of kind"
                , haskInline "*"
                , "in types without any constraints on the type parameters, QuickSpec has support in the form of placeholders"
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
                , footnote $
                  s
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
        subsubsection "Parameters with Constraints" $ do
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
