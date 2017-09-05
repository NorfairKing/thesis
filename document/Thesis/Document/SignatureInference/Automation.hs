{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.Automation
    ( thesisSignatureInferenceAutomation
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Document.References
import Thesis.Document.Sections

thesisSignatureInferenceAutomation :: Thesis
thesisSignatureInferenceAutomation =
    subsection "Automation" $ do
        s
            "The first step in making property discovery feasible for practical use, is ensuring that a programmer never has to write extra code to discover properties of their code."
        l
            [ "This automation involves inspecting the subject code and generating a"
            , haskInline "Signature"
            , "to run"
            , haskInline "quickSpec"
            , "on using an automated interactive evaluator"
            ]
        subsubsection "Generating a signature" $ do
            l
                [ "By interfacing with the GHC API"
                , cite ghcAPIRef
                , "one can find all functions that are in scope in a given module"
                ]
            l
                [ "We will refer to these functions as values of type"
                , haskInline "Function"
                , "for easy reference, because these values have several names in the GHC API"
                ]
            l
                [ "To automatically supply QuickSpec with a"
                , haskInline "Signature"
                , "we need to generate a Haskell expression that describes a"
                , haskInline "Signature"
                , "and interactively evaluate it"
                ]
            l
                [ "Recall that a signature contains a list of"
                , haskInline "Constant"
                , "values"
                ]
            l
                [ "To generate an expression that evaluates to a"
                , haskInline "Signature" <>
                  ", we need to generate subexpressions that each evaluate to a"
                , haskInline "Constant"
                ]
            l
                [ "This involves monomorphising the functions in scope, and generating a"
                , haskInline "Constant"
                , "expression for each of the"
                , haskInline "Function"
                , "values by giving each function a name, and specifying their type explicitly"
                ]
            haskL
                [ "not :: Bool -> Bool"
                , "   -- becomes --"
                , "constant \"not\" (not :: Bool -> Bool)"
                ]
            s
                "Next, we need to generate an expression that describes the type class instances that are in scope."
            s
                "QuickSpec already knows about some instances by default, and these were comprehensive enough to perform our research, so the full instance resolution work has not been implemented in EasySpec."
            lnbk
            s
                "Lastly, we need to generate an expression that describes the properties that we already know about."
            l
                [ "Previously discovered properties usually come from previous runs of QuickSpec, so we leave this field empty for now and get back to it in section"
                , ref strategyWithBackgroundSection
                ]
            l
                [ "We will call this initial automation"
                , fullBackground
                , "for reasons that will become clear in section"
                , ref fullBackgroundSection
                ]
        subsubsection "Monomorphisation" $ do
            s
                "Monomorphisation consists of instantiating any type variables in the type of a function with monomorphic types so that no type variables remain."
            newline
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
                , "type class even though the parameter that it represents should be in the"
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
            newline
            s
                "Haskell supports type parameters that cannot be instantiated with a type, but rather with a type constructor."
            s "These type parameters are called higher kinded."
            l
                [ "An example of a higher kinded type parameter is"
                , haskInline "t"
                , "in the length function"
                ]
            hask "length :: Foldable t => t a -> Int"
            s
                "Monomorphisation of higher kinded types is not trivial in general, but could be performed manually for type constructors in scope, given that type constraints can be resolved."
            s "This transformation has not been implemented in EasySpec yet."
        subsubsection "Complexity" $ do
            l
                [ "The"
                , fullBackground
                , "automation already offers many benefits, but it does not solve the problem of computational complexity that QuickSpec exhibits"
                ]
            let automationComplexityLabel = "fig:automationComplexity"
            hereFigure $ do
                withRegisteredAsset assetRuntimeFullBackgroundPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $ "Runtime of" <> fullBackground
                lab automationComplexityLabel
            l
                [ "In Figure"
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
                , "but it is not practical for real codebases"
                ]
            l
                [ "For a more detailed breakdown of this complexity, we refer to section"
                , ref discoveryComplexitySection
                ]
