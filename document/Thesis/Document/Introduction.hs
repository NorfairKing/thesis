{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Introduction
    ( thesisIntroduction
    ) where

import DocImport

import Thesis.Document.References

thesisIntroduction :: Thesis
thesisIntroduction =
    section "Introduction" $
    -- The goal is to eventually get to my own contribution
    -- Something familiar to the reader that motivates the work, they should think "yes this is a problem that should be fixed"
    --
    -- 2 categories of correctness:
    --   -- language enforced
    --   -- Help via tooling
    --     -- Formal methods
    --     -- testing
    --
    -- state exxplicitly: this is the problem that we are trying to fix
    --  -- Writing correct code is hard
    --    -- Writing tests is expensive
    --      -- Discovery is expensive
    --
    -- My contributions:
    -- - Easyspec, the tool
    -- - The DSL for strategies
    -- - Easyspec-evaluate to evaluate strategies
     do
        s "Writing correct code is hard."
        s
            "In order to write correct code, a programmer first has to decide what it means for code to be correct, and then write code that matches these expectations."
        s
            "The process is further complicated by the fact that these two phases are rarely separated in practice."
        lnbk
        s
            "The first step in writing correct code is to choose a programming language."
        l
            [ "Compilation, static typing, immutability, functional programming, purity,"
            , raw "higher-order"
            , "programming and laziness are just some examples of programming language features that should support the programmer in writing correct code"
            ]
        lnbk
        s
            "Beyond what the language of choice can offer, tools that employ formal methods such as formal verification, model checking, various static analysis techniques, etc, can provide additional correctness guarantees."
        s
            "These usually suffer from one of two problems: Either the programmer already has to have decided what it means for their code to be correct, or the method is so specific in its scope that it cannot help in general."
        s
            "Moreover, they are also often too expensive in terms of engineering effort or computing power, or both, to be used in practice."
        lnbk
        s "The most commonly adopted approach in practice, is testing."
        s
            "Testing suffers from the same issue that a programmer already has to know what it means for their code to be correct, but it is rather more generally applicable."
        s
            "The idea of automated testing is that programmers can tell if their code is faulty before running the code in production."
        s
            "In principle, the programmer writes extra code that will automatically test whether the subject code is faulty."
        s
            "It seems that testing currently has the best balance between cost and benefit, but it is still too often beneficial from a business perspective to skip writing tests in the short term."
        lnbk
        l
            [ emph "Unit testing"
            , "is the most widely adopted approach to testing"
            ]
        s
            "A unit test consists of a piece of code, without arguments, and possibly some assertions about the results."
        s
            "We say that a unit test passes if, when run, the code does not crash and no assertions fail."
        s "Unit tests have two main problems."
        lnbk
        l
            [ "Edsger W. Dijkstra famously said"
            , dquoted $
              s
                  "Program testing can be a very effective way to show the presence of bugs, but it is hopelessly inadequate for showing their absence."
            , cite theHumbleProgrammerRef
            ]
        s
            "Indeed, unit tests often fail to address all possible aspects of the code under test."
        s
            "One would have to write at least one unit test for every possible code path of the code under test to have a chance to cover all possible aspects of the code under test."
        l
            [ "In what follows, we will call this problem the"
            , emph "coverage problem"
            ]
        lnbk
        s "Unit testing is quite expensive when it comes to developer time."
        s
            "Developers have to come up with enough examples of inputs and their corresponding outputs to cover the subject code."
        s "Then they have to implement the corresponding unit testing code."
        s
            "Not only does this process take a lot of time, it is also a mentally taxing task since the process is repetitive and often unrewarding."
        s
            "The problem of cost is exacerbated by the sad reality that management often does not see the value in testing."
        s
            "After all, if the code works, then testing looks like an extra cost that could be cut."
        s
            "If the code contains mistakes, then spending time testing (instead of ignoring the mistakes) just makes the development process seem slower."
        s
            "As a result, tests are all too often omitted from a software project."
        lnbk
        l
            [ emph "Property testing"
            , cite quickcheckRef
            , cite leancheckRef
            , "differs from unit testing in one key aspect: The test code now takes an argument as input"
            ]
        s
            "For any argument, the test code now tests a property of this argument, namely the property that the test passes when given this input."
        s
            "We say that the property test passes if it passes for a given number of randomly generated input values."
        lnbk
        s
            "Property testing solves the coverage problem of unit testing probabilistically, but exacerbates the cost problem of unit testing."
        s
            "As the number of executions of a property test increases, the probability that all possible aspects of the code under test are covered, should tend to one."
        s
            "This means that a programmer now only needs to write a limited number of property tests, instead of a large number of unit tests."
        s
            "However, coming up with general properties of code is often considered more difficult than coming up with example test cases."
        s
            "Consequently, the developer time costs of property testing is even higher and, as a result, property testing is rarely ever done in practice."
        lnbk
        l
            [ emph "Property discovery"
            , cite quickspecRef <> ","
            , cite quickspec2Ref <> ","
            , cite speculateRef
            , "is a technique to produce property tests for subject code automatically"
            ]
        s
            "By eliminating the human effort from conceiving tests, this approach can be combined with property testing to solve both of the problems with unit testing."
        s
            "By discovering properties automatically, the programmer is relieved from having to think of examples or properties, and now only has to select the properties that they think should hold."
        s
            "Property discovery is a relatively new technique, that is not ready for use in practical software engineering yet."
        lnbk
        l
            [ raw "Claessen et al."
            , cite quickspecRef
            , "have explored automatic discovery of equational properties, and have shown that property discovery is a complex problem"
            ]
        s
            "The first attempt failed to discover large properties or properties of a large codebase in a reasonable amount of time."
        l
            [ "Subsequent research"
            , cite quickspec2Ref
            , "has improved upon these limitations, but remains unable to discover properties of realistic codebases"
            ]
        lnbk
        l
            [ "The input to the property discovery algorithm developed"
            , cite quickspecRef
            , "and"
            , cite quickspec2Ref
            , "is called a"
            , emph "signature"
            , "of functions"
            ]
        s
            "A signature is a set of functions that are defined to be relevant in property discovery."
        s "All functions that are not in the signature are completely ignored."
        s
            "This means that, to discover the properties of a single function, the programmer has to specify which functions are relevant."
        s
            "The process of figuring out which functions are relevant in property discovery is often only marginally easier than to think of the properties manually."
        lnbk
        s
            "This signature is defined by a piece of code that the programmer has to write manually."
        s
            "It contains the names, types, and references to the implementations, of all the functions in the signature upon which one wishes to run the property algorithm."
        s
            "Writing this code represents a significant burden on the programmer that ensures that property discovery is not a practical method in practice from a developer's perspective."
        lnbk
        s
            "We contribute a new approach to taming the computational complexity of property discovery: signature inference."
        s
            "The premise is that a programmer should not have to specify all relevant functions in order to perform property discovery."
        s
            "Ideally, the programmer should also not have to write an extra piece of code just to run the property discovery algorithm."
        s
            "Signature inference consists of inferring appropriate input for the current property discovery mechanism by using compile time information about the subject code."
        l
            [ "This approach therein simultaneously solves the"
            , dquoted "code as input"
            , "problem"
            ]
        s
            "Altogether, signature inference has the potential to solve the problems with property discovery and, by extension, property testing and unit testing."
        l
            [ "We propose a domain specific language to define what we have named"
            , emph "signature inference strategies"
            ]
        s
            "These describe how to automatically supply input to a property discovery algorithm and combine the results."
        s
            "Lastly, we contribute several signature inference strategies and a framework in which these signature inference strategies can be evaluated and compared to each other."
