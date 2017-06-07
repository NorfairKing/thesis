{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

-- import Thesis.Document.References
thesisSignatureInference :: Thesis
thesisSignatureInference = do
    section "Signature Inference" $ do
        s "In this section I will use the concept of signature inference."
        subsection "Premise" $ do
            s
                "Signature inference is best introduced by explaining the thought process that lead to its definition."
            s
                "QuickSpec requires a set of functions, and discovers all the equations that relate them."
            s
                "This work, however, asserts that usually a programmer is not necessarily in all the equations relating all the functions in the entire codebase."
            s
                "The assumption is that a programmer is more interested in the properties that relate a very small number of functions, say one."
            s "This function is called the focus function."
            l
                [ "The question then becomes"
                , dquoted
                      "How do we choose the signature to give to QuickSpec such that it will find properties of that one focus function?"
                ]
            "Ideally the signature will be chosen such that:"
            itemize $ do
                item $
                    s
                        "QuickSpec finds all of the properties that relate the focus function with the rest of the codebase."
                item $
                    s
                        "QuickSpec does not waste any time finding the properties of the codebase that do not involve the focus function."
            s
                "Signature inference is the process of choosing the right signatures to give to QuickSpec, with the above goals."
        subsection "Signature inference strategies" $ do
            s
                "A signature inference strategy is the general data type that will drive signature inference."
            s
                "It is defined to contain a function that infers a signature from two pieces of data, The focus functions and the complete scope of functions that are available."
            s
                "The type signature of a signature inference strategy looks a bit like the following."
            hask "Id -> Id -> InferredSignature"
            l
                [ "Here,"
                , haskInline "Id"
                , "is type of which the values represent Haskell functions"
                ]
            l
                [ "In particular, an"
                , haskInline "Id"
                , "contains the name and type of the corresponding Haskell function"
                ]
            l
                [ "A value of type"
                , haskInline "InferredSignature"
                , "is not a signature for quickspec"
                ]
            l
                [ "In fact, an"
                , haskInline "InferredSignature"
                , "more closely resembles a forest of signatures"
                ]
        subsection "Running QuickSpec on an inferred signature" $ do
            l
                [ "Given an"
                , haskInline "InferredSignature"
                , "QuickSpec is run as follows"
                ]
            s
                "First, QuickSpec is run on the leaves of each tree of signatures."
            s
                "In every branch of a tree, the equations that are discovered by QuickSpec at the nodes below that branch are added to the signature in that node as background properties."
            s
                "After running QuickSpec on each tree of signatures in that way, the discovered properties are simply combined to form the final list of discovered equations."
    section "Evaluation of inference strategies" $
        s
            "In this section I will explain how difference inference strategies can be evaluated objectively."
