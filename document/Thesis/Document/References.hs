{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.References
    ( quickcheckRef
    , quickspecRef
    ) where

import DocImport

quickcheckRef :: Reference
quickcheckRef =
    Reference
        inproceedings
        "QuickCheck"
        [ "author" .= "Claessen, Koen and Hughes, John"
        , "title" .=
          "QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs"
        , "booktitle" .=
          "Proceedings of the Fifth ACM SIGPLAN International Conference on Functional Programming"
        , "series" .= "ICFP '00"
        , "year" .= "2000"
        , "isbn" .= "1-58113-202-6"
        , "pages" .= "268--279"
        , "numpages" .= "12"
        , "url" .= "http://doi.acm.org/10.1145/351240.351266"
        , "doi" .= "10.1145/351240.351266"
        , "acmid" .= "351266"
        , "publisher" .= "ACM"
        , "address" .= "New York, NY, USA"
        ]

quickspecRef :: Reference
quickspecRef =
    Reference
        inproceedings
        "QuickSpec"
        [ "title" .= "QuickSpec: Guessing Formal Specifications Using Testing"
        , "author" .= "Koen Claessen and Nicholas Smallbone and John Hughes"
        , "booktitle" .= "TAP"
        , "year" .= "2010"
        ]

inproceedings :: ReferenceType
inproceedings = "inproceedings"

(.=) :: a -> b -> (a, b)
(.=) = (,)
