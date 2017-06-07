{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.References
    ( ghcRef
    , quickcheckRef
    , quickcheckHackageRef
    , quickspecRef
    , quickspec2Ref
    , quickspecHackageRef
    , hipspecRef
    , smartcheckRef
    , fitspecRef
    , smallcheckRef
    , theHumbleProgrammerRef
    , filteringRef
    , mash2Ref
    , typeableHackageRef
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

quickcheckHackageRef :: Reference
quickcheckHackageRef =
    Reference
        miscelaneous
        "claessen2016QuickCheck-2.9.2"
        [ "author" .= "Koen Claessen"
        , "title" .= "{QuickCheck: Automatic testing of Haskell programs}"
        , "howpublished" .=
          "\\url{http://hackage.haskell.org/package/QuickCheck-2.9.2}"
        , "year" .= "2016"
        , "month" .= "September"
        , "version" .= "2.9.2"
        , "keywords" .= "Haskell, Testing"
        , "subtype" .= "program"
        ]

miscelaneous :: ReferenceType
miscelaneous = "Misc"

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

quickspec2Ref :: Reference
quickspec2Ref =
    Reference
        article
        "QuickSpec2"
        [ "title" .= "Quick Specifications for the Busy Programmer"
        , "author" .=
          "Nicholas Smallbone, Moa Johansson, Koen Claesson and Maximilian Algehed"
        , "booktitle" .= "Journal of Functional Programming"
        , "year" .= "2016"
        ]

quickspecHackageRef :: Reference
quickspecHackageRef =
    Reference
        miscelaneous
        "smallbone2016QuickSpec-2"
        [ "author" .= "Nick Smallbone"
        , "title" .= "{QuickCheck: Automatic testing of Haskell programs}"
        , "howpublished" .=
          "\\url{https://github.com/nick8325/quickspec/tree/3c6e0105374bcf1ed0d4f8d2a1a1d2875764fa56}"
        , "year" .= "2016"
        , "version" .= "2"
        , "keywords" .= "Haskell, Testing"
        , "subtype" .= "program"
        ]

hipspecRef :: Reference
hipspecRef =
    Reference
        inbook
        "HipSpec"
        [ "author" .=
          "Claessen, Koen\nand Johansson, Moa\nand Ros{\'e}n, Dan\nand Smallbone, Nicholas"
        , "editor" .= "Bonacina, Maria Paola"
        , "title" .= "Automating Inductive Proofs Using Theory Exploration"
        , "bookTitle" .=
          "Automated Deduction -- CADE-24: 24th International Conference on Automated Deduction, Lake Placid, NY, USA, June 9-14, 2013. Proceedings"
        , "year" .= "2013"
        , "publisher" .= "Springer Berlin Heidelberg"
        , "address" .= "Berlin, Heidelberg"
        , "pages" .= "392--406"
        , "isbn" .= "978-3-642-38574-2"
        , "doi" .= "10.1007/978-3-642-38574-2_27"
        , "url" .= "http://dx.doi.org/10.1007/978-3-642-38574-2_27"
        ]

smartcheckRef :: Reference
smartcheckRef =
    Reference
        inproceedings
        "SmartCheck"
        [ "author" .= "Lee Pike"
        , "title" .=
          "{SmartCheck}: Automatic and Efficient Counterexample Reduction and Generalization"
        , "booktitle" .= "Proceedings of the Haskell Symposium"
        , "publisher" .= "ACM"
        , "year" .= "2014"
        , "note" .=
          "{Available at \\url{http://www.cs.indiana.edu/~lepike/pub_pages/smartcheck.html}}"
        ]

fitspecRef :: Reference
fitspecRef =
    Reference
        inproceedings
        "FitSpec"
        [ "author" .= "Braquehais, Rudy and Runciman, Colin"
        , "title" .= "FitSpec: Refining Property Sets for Functional Testing"
        , "booktitle" .=
          "Proceedings of the 9th International Symposium on Haskell"
        , "series" .= "Haskell 2016"
        , "year" .= "2016"
        , "isbn" .= "978-1-4503-4434-0"
        , "location" .= "Nara, Japan"
        , "pages" .= "1--12"
        , "numpages" .= "12"
        , "url" .= "http://doi.acm.org/10.1145/2976002.2976003"
        , "doi" .= "10.1145/2976002.2976003"
        , "acmid" .= "2976003"
        , "publisher" .= "ACM"
        , "address" .= "New York, NY, USA"
        , "keywords" .=
          "Haskell, formal specification, mutation testing, property-based testing, systematic testing"
        ]

smallcheckRef :: Reference
smallcheckRef =
    Reference
        inproceedings
        "SmallCheck"
        [ "author" .=
          "Runciman, Colin and Naylor, Matthew and Lindblad, Fredrik"
        , "title" .=
          "Smallcheck and Lazy Smallcheck: Automatic Exhaustive Testing for Small Values"
        , "booktitle" .=
          "Proceedings of the First ACM SIGPLAN Symposium on Haskell"
        , "series" .= "Haskell '08"
        , "year" .= "2008"
        , "isbn" .= "978-1-60558-064-7"
        , "location" .= "Victoria, BC, Canada"
        , "pages" .= "37--48"
        , "numpages" .= "12"
        , "url" .= "http://doi.acm.org/10.1145/1411286.1411292"
        , "doi" .= "10.1145/1411286.1411292"
        , "acmid" .= "1411292"
        , "publisher" .= "ACM"
        , "address" .= "New York, NY, USA"
        , "keywords" .=
          "embedded language, exhaustive search, lazy evaluation, property-based testing, type classes"
        ]

filteringRef :: Reference
filteringRef =
    Reference
        inproceedings
        "Filtering"
        [ "author" .= "Jia Meng and Lawrence C. Paulson"
        , "title" .=
          "Lightweight relevance filtering for machine-generated resolution problems"
        , "booktitle" .=
          "In ESCoR: Empirically Successful Computerized Reasoning"
        , "year" .= "2006"
        , "pages" .= "53--69"
        ]

mash2Ref :: Reference
mash2Ref =
    Reference
        article
        "Mash2"
        [ "author" .=
          "Jasmin Christian Blanchette and\nDavid Greenaway and\nCezary Kaliszyk and\nDaniel K{\"{u}}hlwein and\nJosef Urban"
        , "title" .= "A Learning-Based Fact Selector for Isabelle/HOL"
        , "journal" .= "J. Autom. Reasoning"
        , "volume" .= "57"
        , "number" .= "3"
        , "pages" .= "219--244"
        , "year" .= "2016"
        ]

inbook :: ReferenceType
inbook = "inbook"

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

ghcRef :: Reference
ghcRef =
    Reference
        "online"
        "GHC8"
        [ "author" .= "Ben Gamari"
        , "title" .= "Glasgow Haskell Compiler version 8.0.1"
        , "year" .= "2016"
        , "url" .=
          "https://mail.haskell.org/pipermail/ghc-devs/2016-May/012098.html"
        , "urldate" .= "2017-02-01"
        ]

typeableHackageRef :: Reference
typeableHackageRef =
    Reference
        miscelaneous
        "unknown2017base-4.9.1.0-typeable"
        [ "author" .= "unknown"
        , "title" .= "{Typeable @ base-4.9.1.0}"
        , "howpublished" .=
          "\\url{http://hackage.haskell.org/package/base-4.9.1.0/Data-Typeable.html#t:Typeable}"
        , "year" .= "2017"
        , "month" .= "January"
        , "version" .= "4.9.1.0"
        , "keywords" .= "Prelude"
        , "subtype" .= "program"
        ]

(.=) :: a -> b -> (a, b)
(.=) = (,)
