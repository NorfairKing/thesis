{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.References
    ( haskellOrgRef
    , haskellOrgClasses
    , haskellTypeClassesProposal
    , haskellTypeClasses
    , haskellTypeClassesTypeInference
    , haskellTypeClassesImplementation
    , ghcRef
    , ghcAPIRef
    , quickcheckRef
    , quickcheckHackageRef
    , quickspecRef
    , quickspec2Ref
    , quickspecGithubRef
    , hipspecRef
    , smartcheckRef
    , fitspecRef
    , smallcheckRef
    , leancheckRef
    , speculateRef
    , theHumbleProgrammerRef
    , filteringRef
    , mash2Ref
    , typeableHackageRef
    , catalanNumberRef
    ) where

import DocImport

haskellOrgRef :: Reference
haskellOrgRef =
    Reference
        "online"
        "HaskellOrg"
        [ "author" .= "{haskell.org}"
        , "title" .= "{Haskell Language Front Page}"
        , "url" .= "https://www.haskell.org/"
        , "urldate" .= "2017-06-11"
        ]

haskellOrgClasses :: Reference
haskellOrgClasses =
    Reference
        "online"
        "HaskellOrgTypeClasess"
        [ "title" .= "{Type Classes and Overloading}"
        , "url" .= "https://www.haskell.org/tutorial/classes.html"
        , "urldate" .= "2017-06-11"
        ]

haskellTypeClassesProposal :: Reference
haskellTypeClassesProposal =
    Reference
        miscelaneous
        "haskellTypeClassesProposal"
        [ "author" .= "{Philip Lee Wadler}"
        , "howpublished" .= "{Letter to Haskell working group}"
        , "year" .= "1988"
        , "month" .= "Februari"
        , "day" .= "27"
        , "url" .=
          "http://homepages.inf.ed.ac.uk/wadler/papers/class-letter/class-letter.txt"
        , "urldate" .= "2017-06-11"
        ]

haskellTypeClasses :: Reference
haskellTypeClasses =
    Reference
        inproceedings
        "haskellTypeClasses"
        [ "author" .= "{Philip Wadler and Stephen Blott}"
        , "title" .= "{How to Make ad-hoc Polymorphism Less ad-hoc}"
        , "booktitle" .=
          "{Conference Record of the Sixteenth Annual {ACM} Symposium on Principles of Programming Languages, Austin, Texas, USA, January 11-13, 1989}"
        , "pages" .= "{60--76}"
        , "year" .= "{1989}"
        , "url" .= "{http://doi.acm.org/10.1145/75277.75283}"
        , "doi" .= "{10.1145/75277.75283}"
        , "timestamp" .= "{Mon, 21 May 2012 16:19:51 +0200}"
        , "biburl" .= "{http://dblp.uni-trier.de/rec/bib/conf/popl/WadlerB89}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

haskellTypeClassesTypeInference :: Reference
haskellTypeClassesTypeInference =
    Reference
        article
        "hall1996type"
        [ "author" .=
          "{Cordelia V. Hall and Kevin Hammond and Simon L. Peyton Jones and Philip Wadler}"
        , "title" .= "{Type Classes in Haskell}"
        , "journal" .= "{{ACM} Trans. Program. Lang. Syst.}"
        , "volume" .= "{18}"
        , "number" .= "{2}"
        , "pages" .= "{109--138}"
        , "year" .= "{1996}"
        , "url" .= "{http://doi.acm.org/10.1145/227699.227700}"
        , "doi" .= "{10.1145/227699.227700}"
        , "timestamp" .= "{Mon, 03 Apr 2006 11:19:30 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/journals/toplas/HallHJW96}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

haskellTypeClassesImplementation :: Reference
haskellTypeClassesImplementation =
    Reference
        inproceedings
        "ImplementingTypeClasses"
        [ "author" .= "{John Peterson and Mark P. Jones}"
        , "title" .= "{Implementing Type Classes}"
        , "booktitle" .=
          "{Proceedings of the {ACM} SIGPLAN'93 Conference on Programming Language Design and Implementation (PLDI), Albuquerque, New Mexico, USA, June 23-25, 1993}"
        , "pages" .= "{227--236}"
        , "year" .= "{1993}"
        , "url" .= "{http://doi.acm.org/10.1145/155090.155112}"
        , "doi" .= "{10.1145/155090.155112}"
        , "timestamp" .= "{Mon, 21 May 2012 16:19:53 +0200}"
        , "biburl" .= "{http://dblp.uni-trier.de/rec/bib/conf/pldi/PetersonJ93}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

quickcheckRef :: Reference
quickcheckRef =
    Reference
        inproceedings
        "QuickCheck"
        [ "author" .= "{Koen Claessen and John Hughes}"
        , "title" .=
          "{QuickCheck: a lightweight tool for random testing of Haskell programs}"
        , "booktitle" .=
          "{Proceedings of the Fifth {ACM} {SIGPLAN} International Conference on Functional Programming {(ICFP} '00), Montreal, Canada, September 18-21, 2000.}"
        , "pages" .= "{268--279}"
        , "year" .= "{2000}"
        , "url" .= "{http://doi.acm.org/10.1145/351240.351266}"
        , "doi" .= "{10.1145/351240.351266}"
        , "timestamp" .= "{Tue, 11 Jun 2013 13:51:25 +0200}"
        , "biburl" .= "{http://dblp.uni-trier.de/rec/bib/conf/icfp/ClaessenH00}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

quickcheckHackageRef :: Reference
quickcheckHackageRef =
    Reference
        miscelaneous
        "claessen2016QuickCheck-2.9.2"
        [ "author" .= "{Koen Claessen}"
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
        [ "author" .= "{Koen Claessen and Nicholas Smallbone and John Hughes}"
        , "title" .= "{QuickSpec: Guessing Formal Specifications Using Testing}"
        , "booktitle" .=
          "{Tests and Proofs, 4th International Conference, {TAP} 2010, M{\\'{a}}laga, Spain, July 1-2, 2010. Proceedings}"
        , "pages" .= "{6--21}"
        , "year" .= "{2010}"
        , "url" .= "{https://doi.org/10.1007/978-3-642-13977-2_3}"
        , "doi" .= "{10.1007/978-3-642-13977-2_3}"
        , "timestamp" .= "{Fri, 19 May 2017 01:25:21 +0200}"
        , "biburl" .= "{http://dblp.uni-trier.de/rec/bib/conf/tap/ClaessenSH10}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

inproceedings :: ReferenceType
inproceedings = "inproceedings"

quickspec2Ref :: Reference
quickspec2Ref =
    Reference
        article
        "QuickSpec2"
        [ "author" .=
          "{Nicholas Smallbone and Moa Johansson and Koen Claessen and Maximilian Algehed}"
        , "title" .= "{Quick specifications for the busy programmer}"
        , "journal" .= "{J. Funct. Program.}"
        , "volume" .= "{27}"
        , "pages" .= "{e18}"
        , "year" .= "{2017}"
        , "url" .= "{https://doi.org/10.1017/S0956796817000090}"
        , "doi" .= "{10.1017/S0956796817000090}"
        , "timestamp" .= "{Mon, 17 Jul 2017 16:13:52 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/journals/jfp/SmallboneJCA17}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

quickspecGithubRef :: Reference
quickspecGithubRef =
    Reference
        miscelaneous
        "smallbone2016QuickSpec-2"
        [ "author" .= "{Nick Smallbone}"
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
          "{Koen Claessen and Moa Johansson and Dan Ros{\\'{e}}n and Nicholas Smallbone}"
        , "title" .= "{Automating Inductive Proofs Using Theory Exploration}"
        , "booktitle" .=
          "{Automated Deduction - {CADE-24} - 24th International Conference on Automated Deduction, Lake Placid, NY, USA, June 9-14, 2013. Proceedings}"
        , "pages" .= "{392--406}"
        , "year" .= "{2013}"
        , "url" .= "{https://doi.org/10.1007/978-3-642-38574-2_27}"
        , "doi" .= "{10.1007/978-3-642-38574-2_27}"
        , "timestamp" .= "{Sun, 21 May 2017 00:17:17 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/conf/cade/ClaessenJRS13}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

smartcheckRef :: Reference
smartcheckRef =
    Reference
        inproceedings
        "SmartCheck"
        [ "author" .= "{Lee Pike}"
        , "title" .=
          "{SmartCheck: automatic and efficient counterexample reduction and generalization}"
        , "booktitle" .=
          "{Proceedings of the 2014 {ACM} {SIGPLAN} symposium on Haskell, Gothenburg, Sweden, September 4-5, 2014}"
        , "pages" .= "{53--64}"
        , "year" .= "{2014}"
        , "url" .= "{http://doi.acm.org/10.1145/2633357.2633365}"
        , "doi" .= "{10.1145/2633357.2633365}"
        , "timestamp" .= "{Mon, 08 Sep 2014 16:12:17 +0200}"
        , "biburl" .= "{http://dblp.uni-trier.de/rec/bib/conf/haskell/Pike14}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

fitspecRef :: Reference
fitspecRef =
    Reference
        inproceedings
        "FitSpec"
        [ "author" .= "{Rudy Braquehais and Colin Runciman}"
        , "title" .= "{FitSpec: refining property sets for functional testing}"
        , "booktitle" .=
          "{Proceedings of the 9th International Symposium on Haskell, Haskell 2016, Nara, Japan, September 22-23, 2016}"
        , "pages" .= "{1--12}"
        , "year" .= "{2016}"
        , "url" .= "{http://doi.acm.org/10.1145/2976002.2976003}"
        , "doi" .= "{10.1145/2976002.2976003}"
        , "timestamp" .= "{Thu, 22 Sep 2016 13:56:24 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/conf/haskell/BraquehaisR16}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

smallcheckRef :: Reference
smallcheckRef =
    Reference
        inproceedings
        "SmallCheck"
        [ "author" .= "{Colin Runciman and Matthew Naylor and Fredrik Lindblad}"
        , "title" .=
          "{Smallcheck and lazy smallcheck: automatic exhaustive testing for small values}"
        , "booktitle" .=
          "{Proceedings of the 1st {ACM} {SIGPLAN} Symposium on Haskell, Haskell 2008, Victoria, BC, Canada, 25 September 2008}"
        , "pages" .= "{37--48}"
        , "year" .= "{2008}"
        , "url" .= "{http://doi.acm.org/10.1145/1411286.1411292}"
        , "doi" .= "{10.1145/1411286.1411292}"
        , "timestamp" .= "{Fri, 29 Jan 2010 14:44:59 +0100}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/conf/haskell/RuncimanNL08}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

leancheckRef :: Reference
leancheckRef =
    Reference
        miscelaneous
        "matela2017LeanCheck-0.6.2"
        [ "author" .= "{Rudy Matela}"
        , "title" .=
          "{LeanCheck: Cholesterol-free property-based testing for Haskell}"
        , "howpublished" .=
          "\\url{https://hackage.haskell.org/package/leancheck-0.6.2}"
        , "year" .= "2017"
        , "month" .= "March"
        , "version" .= "0.6.2"
        , "keywords" .= "Haskell, Testing"
        , "subtype" .= "library"
        ]

speculateRef :: Reference
speculateRef =
    Reference
        inproceedings
        "matela2017Speculate"
        [ "author" .= "{Rudy Braquehais and Colin Runciman}"
        , "title" .=
          "{Speculate: discovering conditional equations and inequalities about black-box functions by reasoning from test results}"
        , "booktitle" .= "{Haskell Symposium 2017}"
        , "year" .= "{2017}"
        , "publisher" .= "{ACM}"
        ]

filteringRef :: Reference
filteringRef =
    Reference
        inproceedings
        "Filtering"
        [ "author" .= "{Jia Meng and Lawrence C. Paulson}"
        , "title" .=
          "{Lightweight relevance filtering for machine-generated resolution problems}"
        , "journal" .= "{J. Applied Logic}"
        , "volume" .= "{7}"
        , "number" .= "{1}"
        , "pages" .= "{41--57}"
        , "year" .= "{2009}"
        , "url" .= "{https://doi.org/10.1016/j.jal.2007.07.004}"
        , "doi" .= "{10.1016/j.jal.2007.07.004}"
        , "timestamp" .= "{Tue, 06 Jun 2017 22:27:34 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/journals/japll/MengP09}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

mash2Ref :: Reference
mash2Ref =
    Reference
        article
        "Mash2"
        [ "author" .=
          "{Jasmin Christian Blanchette and David Greenaway and Cezary Kaliszyk and Daniel K{\\\"{u}}hlwein and Josef Urban}"
        , "title" .= "{A Learning-Based Fact Selector for Isabelle/HOL}"
        , "journal" .= "{J. Autom. Reasoning}"
        , "volume" .= "{57}"
        , "number" .= "{3}"
        , "pages" .= "{219--244}"
        , "year" .= "{2016}"
        , "url" .= "{https://doi.org/10.1007/s10817-016-9362-8}"
        , "doi" .= "{10.1007/s10817-016-9362-8}"
        , "timestamp" .= "{Thu, 15 Jun 2017 21:22:45 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/journals/jar/BlanchetteGKKU16}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

inbook :: ReferenceType
inbook = "inbook"

theHumbleProgrammerRef :: Reference
theHumbleProgrammerRef =
    Reference
        article
        "TheHumbleProgrammer"
        [ "author" .= "{Edsger W. Dijkstra}"
        , "title" .= "{The Humble Programmer}"
        , "journal" .= "{Commun. {ACM}}"
        , "volume" .= "{15}"
        , "number" .= "{10}"
        , "pages" .= "{859--866}"
        , "year" .= "{1972}"
        , "url" .= "{http://doi.acm.org/10.1145/355604.361591}"
        , "doi" .= "{10.1145/355604.361591}"
        , "timestamp" .= "{Tue, 07 Jun 2011 16:50:42 +0200}"
        , "biburl" .=
          "{http://dblp.uni-trier.de/rec/bib/journals/cacm/Dijkstra72}"
        , "bibsource" .= "{dblp computer science bibliography, http://dblp.org}"
        ]

ghcRef :: Reference
ghcRef =
    Reference
        "online"
        "GHC8"
        [ "author" .= "{GHC Developers}"
        , "title" .= "{Glasgow Haskell Compiler version 8.0.2}"
        , "year" .= "2016"
        , "url" .=
          "https://mail.haskell.org/pipermail/ghc-devs/2016-May/012098.html"
        , "urldate" .= "2017-02-01"
        ]

ghcAPIRef :: Reference
ghcAPIRef =
    Reference
        "online"
        "GHCAPI"
        [ "author" .= "{GHC Devs}"
        , "title" .= "{Glasgow Haskell Compiler API version 8.0.2}"
        , "year" .= "2017"
        , "url" .=
          "https://downloads.haskell.org/~ghc/8.0.2/docs/html/libraries/ghc-8.0.2/index.html"
        , "urldate" .= "2017-08-05"
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

catalanNumberRef :: Reference
catalanNumberRef =
    Reference
        "book"
        "TAOCP"
        [ "author" .= "{Knuth, Donald E.}"
        , "title" .=
          "{The Art of Computer Programming, Volume 1 (3rd Ed.): Fundamental Algorithms}"
        , "year" .= "{1997}"
        , "isbn" .= "{0-201-89683-4}"
        , "publisher" .= "{Addison Wesley Longman Publishing Co., Inc.}"
        , "address" .= "{Redwood City, CA, USA}"
        , "section" .= "{2.3.4.4}"
        ]

(.=) :: a -> b -> (a, b)
(.=) = (,)
