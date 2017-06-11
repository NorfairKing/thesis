{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background.PropertyDiscovery
    ( thesisBackgroundPropertyDiscovery
    ) where

import DocImport

import Thesis.Document.References

thesisBackgroundPropertyDiscovery :: Thesis
thesisBackgroundPropertyDiscovery =
    subsection "QuickSpec" $ do
        s
            "QuickSpec was written because writing property tests can be even more difficult than writing unit tests."
        s
            "The idea was that properties could be discovered using a combination of intelligently looking at types, and using properties to validate the properties using QuickCheck."
        s
            "To run QuickSpec, a programmer is required to write a piece of code that calls QuickSpec with a record that is called a signature."
        newline
        s
            "A signature mainly consists of a set of functions, confusingly also sometimes called the signature."
        l
            [ "Each of the functions in a signature must be named, and its type must be in the"
            , haskInline "Typeable"
            , "typeclass"
            ]
        l
            [ "Specifically, a type with a"
            , haskInline "Typeable"
            , "instance allows a concrete representation of a type to be calculated"
            , cite typeableHackageRef
            ]
        l
            [ "A"
            , haskInline "Typeable"
            , "instance can automatically be generated with the"
            , haskInline "DeriveDataTypeable"
            , "GHC language extension or written manually, but only if the type in question is monomorphic."
            ]
        l
            [ "For example, the type"
            , haskInline "Int -> Bool"
            , "is in"
            , haskInline "Typeable"
            , "but"
            , haskInline "a -> Double"
            , "is not, because the latter has a type variable:"
            , haskInline "a"
            ]
        newline
        s
            "A signature further also contains a set of type class instances, specifically the evidence dictionaries."
        l
            [ "For example, if a signature is said to contain the instance"
            , haskInline "Eq Int"
            , "then it contains a record with the following two functions"
            ]
        hask $ do
            "(==) :: Int -> Int -> Int"
            raw "\n"
            "(/=) :: Int -> Int -> Int"
        s
            "QuickSpec only knows about the instances that it gets supplied with via the signature."
        newline
        s
            "The last important part of a signature is a set of background properties."
        s
            "These are properties that the signature somehow knows about already, and will use in its discovery."
        s
            "One way for a signature to contain background properties, is by using the properties that a previous run of QuickSpec discovered."
        newline
        s
            "Using a signature, QuickSpec will discover all properties that relate the functions in that signature, up to a given size."
        s
            "A property, in QuickSpec, is defined as an equation of the following form."
        mintedText "leftTerm = rightTerm"
        l
            [ "Here,"
            , haskInline "leftTerm"
            , "and"
            , haskInline "rightTerm"
            , "must be of the same type, and that type must be of the following form"
            ]
        hask "A -> B"
        l
            [ "In this type,"
            , haskInline "A"
            , "must be in"
            , haskInline "Arbitrary"
            , "such that arbitrary values can be generated as input"
            ]
        l
            [ "Furthermore,"
            , haskInline "B"
            , "must be in"
            , haskInline "Eq"
            , "such that they results of the respective functions may be compared for equality"
            ]
        s
            "Note that the shape of these properties is not a limitation with respect to their expressiveness."
        l
            [ "Indeed, any general property"
            , haskInline "p :: A -> Bool"
            , "can be expressed in the above form as follows"
            ]
        hask $ raw "p = \\_ -> True"
        l
            [ "For further details, please refer to the QuickSpec papers"
            , cite quickspecRef
            , cite quickspec2Ref
            , ", and the QuickCheck package on hackage"
            , cite quickspecHackageRef
            ]
