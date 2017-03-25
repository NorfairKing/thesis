{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.References
    ( quickcheckRef
    , quickspecRef
    , theHumbleProgrammerRef
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

theHumbleProgrammerRef :: Reference
theHumbleProgrammerRef =
    Reference
        article
        "TheHumbleProgrammer"
        [ "author" .= "Dijkstra, Edsger W."
        , "title" .= "The Humble Programmer"
        , "journal" .= "Commun. ACM"
        , "issue_date" .= "Oct. 1972"
        , "volume" .= "15"
        , "number" .= "10"
        , "month" .= "oct"
        , "year" .= "1972"
        , "issn" .= "0001-0782"
        , "pages" .= "859--866"
        , "numpages" .= "8"
        , "url" .= "http://doi.acm.org/10.1145/355604.361591"
        , "doi" .= "10.1145/355604.361591"
        , "acmid" .= "361591"
        , "publisher" .= "ACM"
        , "address" .= "New York, NY, USA"
        ]

(.=) :: a -> b -> (a, b)
(.=) = (,)
