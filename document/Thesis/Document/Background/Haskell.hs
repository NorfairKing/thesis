{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background.Haskell
    ( thesisBackgroundHaskell
    ) where

import DocImport

import Thesis.Document.References

thesisBackgroundHaskell :: Thesis
thesisBackgroundHaskell =
    subsection "Haskell" $ do
        s
            "The code in this work is written in the Haskell programming language."
        s
            "Some familiarity is assumed, but the important details will be revisited here."
        lnbk
        s
            "Haskell is a statically typed, purely functional language that evaluates lazily by default."
        l
            [ "The language lends itself well to concurrent programming and it uses type inference to ease programming"
            , cite haskellOrgRef
            ]
        subsubsection "Type classes" $ do
            s "Haskell famously has support for type classes."
            s
                "Type classes are a way to support ad hoc polymorphism, as opposed to the parametric polymorphism that type parameters provide."
            s
                "Using type classes, the programmer can add constraints to type parameters."
            s "As an example, consider equality."
            s
                "Without type classes, the programmer would have to write a different equality function for each type."
            l
                [ haskInline "eqInt :: Int -> Int -> Bool"
                , "for integers,"
                , haskInline "eqFloat :: Float -> Float -> Bool"
                , "for floating point numbers, etc.."
                ]
            l
                [ "The"
                , haskInline "Eq"
                , "type class solves this problem by allowing the programmer to define what equality means for a type"
                ]
            hask $ do
                "class Eq a where"
                raw "\n"
                "    (==) :: a -> a -> Bool"
            l
                [ "To define equality is to instantiate the"
                , haskInline "Eq"
                , "type class using a type class instance"
                ]
            l
                [ "In this instance, the programmer implements the evidence that the given type indeed belongs to the"
                , haskInline "Eq"
                , "type class"
                ]
            hask $ do
                "instance Eq Int where"
                raw "\n"
                "    (==) :: Int -> Int -> Bool"
                raw "\n"
                "    i == j = eqInt i j"
            l
                [ "Once equality is defined, the programmer can use the"
                , haskInline "=="
                , "function to test for equality, and the type checker will infer which equality will be used"
                ]
            s
                "Now that an equality can be defined parametrically in which type of values are tested, the programmer can write functions that are parametric in a type, so long as that type supports equality."
            l
                [ "For example, the"
                , haskInline "elem"
                , "function can test if a list contains a given element, as long as the elements of the list support equality"
                ]
            hask "elem :: Eq a => a -> [a] -> Bool"
            l
                [ "When a type supports equality, we say that this type is in the type class"
                , haskInline "Eq"
                ]
