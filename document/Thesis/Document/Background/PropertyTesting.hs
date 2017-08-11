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
            "QuickCheck is an implementation of the concept of property testing and it is written in Haskell."
        s "It is a particularly elegant example of a use case for type classes."
        s "The concept is simple."
        s
            "Instead of requiring a programmer to specify a specific example of the working of his code, the programmer now gets to specify a general property of his code."
        s
            "To perform property tests, a programmer needs to provide two pieces of code: A generator, and a function that takes the output of the generator and produces a Boolean value."
        subsubsection "Generators and the Arbitrary Type Class" $ do
            s
                "Generators are a central component of QuickCheck and property test in general."
            s
                "In essence, a generator is a pure function that can use a pseudo random generator to produce values of a certain type."
            l
                [ "For example, a value of type"
                , haskInline "Gen Int"
                , "represents provides evidence that it is possible to generate pseudo random integers given a pseudo random generator of bits"
                ]
            s
                "Users can write generators for their own data types such that values of those types can also be generated."
            lnbk
            l
                [ "Elements of the"
                , haskInline "Arbitrary"
                , "type class provide a generator called"
                , haskInline "arbitrary"
                , "that can generate random values of that type"
                ]
            haskL ["class Arbitrary a where", "    arbitrary :: Gen a"]
            lnbk
            l
                [ "QuickCheck will use the"
                , haskInline "Arbitrary"
                , "type class by default, but users can also specify which generator to use in the random testing"
                ]
        subsubsection "Properties" $ do
            s
                "A property, in this case, is loosely defined as anything that can produce a Boolean value from supplied pseudo randomness."
            s "A typical example is a function as follows."
            hask "f :: A -> Bool"
            s
                "This function is also usually called the property, even though it is pure."
            s
                "To produce random Boolean values using this function, QuickCheck will require a generator of the input type."
        subsubsection "Running property tests" $ do
            l
                [ "When instructed to do so using the"
                , haskInline "quickCheck"
                , "function, the QuickCheck testing framework will generate random inputs to the supplied property"
                ]
            l
                [ "For each of the inputs, QuickCheck asserts that the resulting Boolean value is"
                , haskInline "True"
                ]
            l
                [ "If all of the output values are"
                , haskInline "True"
                , "then we say that the property passes the property test"
                ]
            l
                [ "If the property evaluates any of the input values to"
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
