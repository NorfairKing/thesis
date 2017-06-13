{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background.PropertyTesting
    ( thesisBackgroundPropertyTesting
    ) where

import DocImport

import Thesis.Document.References

thesisBackgroundPropertyTesting :: Thesis
thesisBackgroundPropertyTesting =
    subsection "Property testing" $ do
        s
            "QuickCheck was the first implementation of property testing and it is written in Haskell."
        s "The concept is simple."
        s
            "Instead of requiring a programmer to specify a specific example of the working of his code, the programmer now gets to specify a general property of his code."
        s
            "A property, in this case, is very specifically defined as any function of the following shape."
        hask "f :: A -> Bool"
        l
            [ "Here,"
            , haskInline "A"
            , "must be a type for which QuickCheck can generate random values"
            ]
        l
            [ "QuickCheck does this by requiring"
            , haskInline "A"
            , "to be in the"
            , haskInline "Arbitrary"
            , "type class"
            ]
        l
            [ "When instructed to do so using the"
            , haskInline "quickCheck"
            , "function, the QuickCheck testing framework will generate random inputs to the supplied function"
            ]
        l
            [ "For each of the inputs, the function will evaluate to a Boolean value which QuickCheck asserts to be"
            , haskInline "True"
            ]
        l
            [ "If all of the output values are"
            , haskInline "True"
            , "then the function passes the property test"
            ]
        l
            [ "If the function evaluates any of the input values to"
            , haskInline "False"
            , "then the test fails"
            ]
        s
            "When a function fails a property test a shrinking process is started."
        s
            "In this process, the input value for which the property does not hold is inspected further in order to find a smaller value that still fails the property."
        s
            "The shrinking process is not relevant to the work described in this thesis."
        l
            [ "For further details, please refer to the original QuickCheck paper"
            , cite quickcheckRef
            , ", and the QuickCheck package on Hackage"
            , cite quickcheckHackageRef
            ]
