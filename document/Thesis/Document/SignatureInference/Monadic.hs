{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document.SignatureInference.Monadic
    ( thesisSignatureInferenceMonadic
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Sections

thesisSignatureInferenceMonadic :: Thesis
thesisSignatureInferenceMonadic =
    subsection "Monadic Signature Inference Strategies" $ do
        s
            "Graph signature inference strategies are expressive enough to declare dependencies between QuickSpec runs, but they cannot use information from previous runs in subsequent runs."
        s
            "One hypothesis suggests that the equations that are discovered in QuickSpec runs may teach more about the functions at hand, and what we can expect when we subsequently run QuickSpec."
        s
            "The definition of a signature inference strategy would have to be adapted again, to make it even more expressive."
        subsubsection "Definition" $ do
            s
                "One standard abstraction that allows for the expression of composition is the monad."
            s
                "We adapted the result of a signature inference strategy to be defined as a monadic piece of data that expresses how and when QuickSpec should be run."
            hereFigure $
                haskL
                    [ "type SignatureInferenceStrategy"
                    , "    = [Function] -> [Function] -> InferM ()"
                    , ""
                    , "data InferM a where"
                    , "    InferPure :: a -> InferM a"
                    , "    InferFmap :: (a -> b) -> InferM a -> InferM b"
                    , "    InferApp :: InferM (a -> b) -> InferM a -> InferM b"
                    , "    InferBind :: InferM a -> (a -> InferM b) -> InferM b"
                    , "    "
                    , "    InferFrom"
                    , "        :: Signature"
                    , "        -> [OptiToken]"
                    , "        -> InferM (OptiToken, [Equation])"
                    ]
            l
                [ "To allow for monadic computation, but not for arbitrary input or output, the"
                , haskInline "InferM"
                , "monad represents a syntax tree that describes a computation"
                ]
            s
                "The following instances then enable syntactic sugar to construct such a syntax tree."
            hereFigure $
                haskL
                    [ "instance Functor InferM where"
                    , "    fmap = InferFmap"
                    , ""
                    , "instance Applicative InferM where"
                    , "    pure = InferPure"
                    , "    (<*>) = InferApp"
                    , ""
                    , "instance Monad InferM where"
                    , "    (>>=) = InferBind"
                    ]
            l
                [ "The special constructor"
                , haskInline "InferFrom"
                , "describes the intent to run QuickSpec on the given signature"
                ]
            l
                [ "For each run of QuickSpec, an"
                , haskInline "OptiToken"
                , "is generated that describes that run of QuickSpec, and can be given to subsequent runs to inform QuickSpec about the corresponding previous discoveries"
                ]
            l ["As an example,", chunks, "can now be described as follows"]
            hereFigure $ do
                haskL
                    [ "inferChunks :: [Function] -> [Function] -> InferM ()"
                    , "inferChunks focus scope = do"
                    , "    (l1t, _) <- InferFrom focus []"
                    , "    forM_ [(f, s) | f <- focus, s <- scope] $ \\(f, s) ->"
                    , "        InferFrom [f, s] [l1t]"
                    ]
                caption $ chunks <> " as a monadic signature inference strategy"
            s "This monad only expresses the intent to run QuickSpec."
            s
                "One still requires a straightforward interpreter to actually discover any properties."
        subsubsection "Chunks Plus" $ do
            l
                [ "The increased expressiveness of monadic signature inference strategies opened the doors for a new strategy:"
                , chunksPlus
                ]
            s
                "This strategy is built upon the assumption that if two functions are related by any properties that only relate these two properties, then they are more likely to also be related in properties with more different functions."
            l
                [ "The idea is that"
                , chunksPlus
                , "first runs QuickSpec in a similar way to"
                , chunks
                , "and potentially then runs QuickSpec some more based on the following procedure"
                ]
            l
                [ "For each set of two different scope functions, the corresponding nodes in the"
                , chunks
                , "strategy are considered"
                ]
            s
                "If QuickSpec finds relevant equations in both of these nodes, a new node is created that contains both of these scope functions and the focus function."
            s
                "This new node then points to appropriate two nodes as a dependant."
            l
                [ "As an example, consider the same example scope as in section"
                , ref chunksSection
                ]
            let chunksPlusExampleGraphLabel = "fig:chunks-example-graph"
            l
                [ "The"
                , chunksPlus
                , "signature inference strategy will first run QuickSpec exactly as"
                , chunks
                , "would, and then conditionally on the signatures with three functions as depicted in grey in figure"
                , ref chunksPlusExampleGraphLabel
                ]
            hereFigure $ do
                withDotAsset $(embedAsset "chunks-plus.dot") $ \fp ->
                    center $
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
                caption $
                    sequence_
                        [ "The graph that "
                        , chunksPlus
                        , "builds for the example"
                        ]
                lab chunksPlusExampleGraphLabel
