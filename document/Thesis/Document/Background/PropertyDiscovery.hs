{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background.PropertyDiscovery
    ( thesisBackgroundPropertyDiscovery
    ) where

import DocImport

import qualified Data.Text as T

import Thesis.Document.References

thesisBackgroundPropertyDiscovery :: Thesis
thesisBackgroundPropertyDiscovery =
    subsection "Property Discovery with QuickSpec" $ do
        l
            [ "QuickSpec is a recently developed tool for property discovery"
            , cite quickspecRef <> ","
            , cite quickspec2Ref
            ]
        s "QuickSpec aims to simplify the process of writing property tests."
        s
            "It is based on the idea is that properties could be discovered using a combination of intelligently looking at types, and using tests to validate the properties using QuickCheck."
        s "QuickSpec exposes the following function as its entry point."
        hask "quickSpec :: Signature -> IO Signature"
        s
            "To run QuickSpec, a programmer is required to write a piece of code that calls this function in order to discover any properties."
        l
            [ "To call the"
            , haskInline "quickSpec"
            , "function, the programmer has to define a value of the type"
            , haskInline "Signature"
            ]
        newline
        l
            [ "The following is a simplified definition of"
            , haskInline "Signature"
            ]
        haskL
            [ "data Signature ="
            , "  Signature {"
            , "    constants           :: [Constant],"
            , "    instances           :: [[Instance]],"
            , "    background          :: [Property]"
            , "  }"
            ]
        l
            [ "A signature mainly consists of a list of functions that are called constants because they must be in the"
            , haskInline "Typeable"
            , "type class"
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
            , "GHC language extension or written manually, but only if the type in question is monomorphic"
            ]
        l
            [ "For example, the type"
            , haskInline "Int -> Bool"
            , "is in"
            , haskInline "Typeable"
            , "but"
            , haskInline "a -> Double"
            , "is not, because the latter is polymorphic in a type variable:"
            , haskInline "a"
            ]
        s
            "It is important to note that generating values of a type with type parameters is impractical."
        l
            [ "A value of type"
            , haskInline "Constant"
            , "also contains a given name for the function that it represents"
            ]
        s
            "This allows QuickSpec to output the discovered properties in a human readable manner."
        newline
        s
            "A signature further also contains a set of type class instances, specifically the evidence dictionaries or information about how to construct them."
        l
            [ "For example, if a signature is said to contain the instance"
            , haskInline "Eq Int"
            , "then it contains a record with the following two functions"
            ]
        hask $
            T.unlines ["(==) :: Int -> Int -> Int", "(/=) :: Int -> Int -> Int"]
        s
            "QuickSpec only knows about those instances that it gets supplied with via the signature."
        newline
        s
            "The last part of a signature that is important for this work is a set of background properties."
        s
            "These are properties that the signature somehow knows about already, and will use in its discovery."
        s
            "One way for a signature to contain background properties, is by using the properties that a previous run of QuickSpec discovered."
        newline
        s
            "A signature also contains several configuration settings such as the maximum size of the discovered properties."
        l
            [ "The default value for this setting sets the maximum size of discovered properties to be"
            , m 7
            , "and we do not change it throughout this work"
            ]
        newline
        s
            "Using a signature, QuickSpec will enumerate all properties that relate the functions in that signature, up to a given size."
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
        s
            "Note that it is not a restriction to only allow properties where the input types of the both sides are the same."
        s
            "Indeed, for any property where the input types of the sides are different, there exists a property where this is not the case that expresses the same equation."
        haskL
            [ "\\a      -> f a = \\b      -> g b -- different input type"
            , "\\(a, b) -> f a = \\(a, b) -> g b -- same input type"
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
        hask "\\x -> p x = \\_ -> True"
        s "Also note that QuickSpec does not show properties in this form."
        s
            "In fact, QuickSpec leaves out the left side of the lambda expression, so it would show the above equation as follows."
        hask "      p x =       True"
        l
            [ "When QuickSpec is run using the"
            , haskInline "quickSpec"
            , "function, the discovered properties are contained in the"
            , haskInline "background"
            , "field of the resulting"
            , haskInline "Signature"
            ]
        l
            [ "In fact, this resulting"
            , haskInline "Signature"
            , "is equal to the input"
            , haskInline "Signature"
            , "in all other respects"
            ]
        l
            [ "For further details, please refer to the QuickSpec papers"
            , cite quickspecRef <> ","
            , cite quickspec2Ref
            , "and the QuickSpec repository on GitHub"
            , cite quickspecGithubRef
            ]
