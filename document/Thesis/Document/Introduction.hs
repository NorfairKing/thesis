{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Introduction
    ( thesisIntroduction
    ) where

import DocImport

import Thesis.Document.References

thesisIntroduction :: Thesis
thesisIntroduction =
    section "Introduction" $ do
        s "Signature inference is the last piece of the puzzle of testing."
        s
            "To explain why, we must first sketch the problem, and motivate all the other parts of the puzzle."
        subsection "Motivation" $ do
            subsubsection "Testing" $ do
                s
                    "The romantic idea of automated testing is that programmers could tell if their code was faulty before running the code in production."
                s
                    "Alternatives to testing include formal verification, model checking static analysis, etc..."
                s
                    "When these formal methods are too expensive, testing is the most reliable way to ensure that a software system does what it should."
                s
                    "In practice, formal methods are usually too expensive and mostly not feasible."
                citationNeeded
            subsubsection "Unit Testing" $ do
                s "Unit testing is an initial approach to the testing problem."
                s
                    "A unit test consists of a piece of code, without arguments, and possibly some assertions about the results."
                s
                    "We say that a unit test passes if, when run, the code does not crash and no assertions fail."
                s "Unit tests have two main problems."
                paragraph "Coverage" $ do
                    l
                        [ "Edsger W. Dijkstra famously said"
                        , dquoted
                              "Program testing can be a very effective way to show the presence of bugs, but it is hopelessly inadequate for showing their absence."
                        , cite theHumbleProgrammerRef
                        ]
                    s
                        "Indeed, unit tests often fail to address all possible aspects of the code under test."
                    s
                        "One would have to write at least one unit test for every possible code path of the code under test to have a chance at covering all possible aspects of the code under test."
                    s "This brings us to the next problem."
                paragraph "Developer-time Costs" $ do
                    s
                        "Unit testing, but testing in general, is quite expensive when it comes to developer time."
                    s
                        "This problem is exacerbated by the sad reality that management often does not see the value in testing."
                    s
                        "The programmer has to think of as many different scenarios as possible, and implement the corresponding unit tests."
                    s
                        "Not only does this process take a lot of time, it is also a mentally taxing task since the process is repetitive and often unrewarding."
                    s
                        "As a result, automated tests are all-to-often omitted from a software project."
            subsubsection "Property Testing" $ do
                s
                    "Property testing is similar to unit testing but the concept differs in one key aspect."
                s
                    "Instead of specifying a single scenario with the test code, the test code now takes an argument as input."
                s
                    "The code now tests a property of this argument, namely the property that the test passes when given this input."
                s
                    "We say that the property test passes if it passes for a given number of arbitrarily chosen input values."
                lnbk
                s
                    "Property testing probabilistically solves the coverage problem of unit testing, but exacerbates the second problem of unit testing."
                s
                    "As the number of executions of a property test increases, the probability of covering all possible aspects of the code under test should tend to one."
                s
                    "This means that a programmer now only needs to write a limited number of property tests, instead of a large number of unit tests."
                s
                    "This sounds like it is easier on the programmer, and for some it is, but coming up with properties of code, as opposed to input-output examples is often much harder."
                s
                    "Consequently, the developer-time costs of property testing is even higher and, as a result, property testing is rarely done in practice."
            subsubsection "Property Discovery" $ do
                s
                    "Taking the human element out of the equation would solve the problem with developer-time costs."
                s
                    "Property discovery means that property tests can automatically be generated."
                s
                    "This process relieves the programmer from having to think of examples or properties and only requires them to select the properties that they think should hold."
                s
                    "While property discovery solves the problems with unit testing and property testing, it comes with a new problems."
                paragraph "Complexity" $ do
                    l
                        [ "Previous work"
                        , cite quickspecRef
                        , "has explored automatic discovery of equational properties, and has shown that property discovery is a complex problem"
                        ]
                    s
                        "The first attempt failed to discover large properties or properties of a large codebase in a reasonable amount of time."
                    l
                        [ "Subsequent research"
                        , cite quickspec2Ref
                        , "has improved upon these limitations, but remains unable to discover properties of realistic codebases"
                        ]
                paragraph "Signatures" $ do
                    l
                        [ "The input to the property discovery algorithm that is mentioned in"
                        , cite quickspecRef
                        , "and"
                        , cite quickspec2Ref
                        , "is a so-called"
                        , dquoted "signature"
                        , "of functions"
                        ]
                    s
                        "A signature is a set of functions that are defined to be relevant in property discovery."
                    s
                        "All functions that are not in the signature are completely ignored."
                    s
                        "This means that, to discover the properties of a single function, the programmer has to specify which functions are relevant."
                    s
                        "The process of figuring out which functions are relevant in property discover is often only marginally easier than to think of the properties manually."
                paragraph "Code as input" $ do
                    s
                        "The third problem with property discovery, as it currently stands, is that the input is itself a piece of code."
                    s
                        "This code contains the names of, types of, and references to the implementations of all the functions in the signature upon which one wishes to run the property algorithm."
                    s
                        "Writing this code imposes a transaction cost that ensures that property discovery is not feasible in practice from a developer's perspective."
            subsubsection "Signature Inference" $ do
                s
                    "Signature inference is a new approach to taming the complexity of property discovery, while simultaneously solving the input problem."
                s
                    "The premise is that a programmer should not have to specify all relevant functions in order to perform property discovery."
                s
                    "Ideally, the programmer should also not have to write an extra piece of code just to run the property algorithm."
                s
                    "Signature inference consists of inferring which functions are relevant to a given function, and combining them into a signature."
                s
                    "Because signature inference requires access to the compile-time information about the code under test, a simple extension involves generating the code that is required to run the property discovery algorithm."
                s
                    "All together, signature inference has the potential to solve the problems with property discovery and, by extension, property testing and unit testing."
