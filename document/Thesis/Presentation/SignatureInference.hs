{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Presentation.Utils

signatureInference :: Thesis
signatureInference = do
    pauseSlide 2
    section "Signature Inference" $ do
        pictureSlide "Automated, but still slow" assetRuntimeFullBackgroundPlot
        f "" $ huge $ center "Definitions"
        g "Definitions: Property" $ do
            vfill
            " Example:"
            hask "reverse (reverse ls) = ls"
            vfill
            " Short for:"
            hask "(\\ls -> reverse (reverse ls)) = (\\ls -> ls)"
            vfill
            " In general:"
            hask $
                T.unlines
                    [ "(f :: A -> B) = (g :: A -> B)"
                    , "for some A and B with"
                    , "instance Arbitrary A"
                    , "instance Eq B"
                    ]
            vfill
        g "Definitions: Size of property" $ do
            " Example:"
            hask "xs <= mySort xs = myIsSorted xs"
            pause
            raw "Size: 4"
            pause
            vfill
            " In general: It's complicated"
        g "Definitions: Property of a function" $ do
            "Functions:"
            hask $ T.unlines ["f = (* 2)", "g = (* 3)", "z = 0"]
            vfill
            "Properties of " <> haskInline "f" <> ":"
            hask $ T.unlines ["f (g x) = g (f x)", "f z = z"]
            "Not properties of " <> haskInline "f" <> ":"
            hask $ T.unlines ["g z = z"]
            vfill
            raw "relevant property = property of focus function"
        g "Definitions: Relevant function" $ do
            "Functions:"
            hask $ T.unlines ["f = (* 2)", "g = (* 3)", "z = 0", "h = id"]
            "Properties:"
            hask $
                T.unlines ["f (g x) = g (f x)", "f z = z", "g z = z", "h x = x"]
            vfill
            haskInline "g" <> " and " <> haskInline "z" <> " are relevant to " <>
                haskInline "f" <>
                " but " <>
                haskInline "h" <>
                " is not."
        g "Definitions: Scope" $ do
            "Scope: Functions in scope"
            vfill
            pause
            "Size of scope: Number of functions in scope"
            vfill
            pause
            "Size of signature: Number of functions in signature"
        pictureSlide "Automated, but still slow" assetRuntimeFullBackgroundPlot
        f "Why is this slow?" $
            enumerate $ do
                item "Maximum size of the discovered properties"
                pause
                item "Size of the signature"
        lightbulbslide
        f "Critical insight" $ do
            center "We are not interested in the entire codebase."
            center "We are interested in a relatively small amount of code."
        g "Reducing the size of the signature" $ do
            hask $
                T.unlines
                    [ "inferSignature"
                    , "  :: [Function] -- Focus functions"
                    , "  -> [Function] -- Functions in scope"
                    , "  -> [Function] -- Chosen functions"
                    ]
        g "Full background and empty background" $ do
            hask $ "inferFullBackground _ scope = scope"
            hask $ "inferEmptyBackground focus _ = focus"
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeFullBreakthroughEmptyBackgroundPlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundEmptyBackgroundPlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic similarity: Name" $ do
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityName [focus] scope"
                    , "    = take 5 $ sortOn"
                    , "      (\\sf ->"
                    , "        hammingDistance"
                    , "          (name focus) (name sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeFullBreakthroughSyntacticSimilarityNamePlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic similarity: Implementation" $ do
            raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityName [focus] scope"
                    , "    = take 5 $ sortOn"
                    , "      (\\sf ->"
                    , "        hammingDistance"
                    , "          (symbols focus) (symbols sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeFullBreakthroughSyntacticSimilaritySymbolsPlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic similarity: Type" $ do
            raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityName [focus] scope"
                    , "    = take 5 $ sortOn"
                    , "      (\\sf ->"
                    , "        hammingDistance"
                    , "          (getTypeParts focus) (getTypeParts sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeFullBreakthroughSyntacticSimilarityTypePlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot $ \fp -> do
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        f "Other things we tried" $ do
            enumerate $ do
                item "Similarity using a different metric: edit distance"
                item "Unions of the previous strategies"
        pictureSlide "Breakthrough" assetNrDifferentFunctionsPlot
        comment
            "60-70% of all properties involve onle one or 2 different functions"
        comment
            "90% of all properties involve three or fewer different functions"
        comment
            "That means that it doesn't make sense to ever put your entire codebase into a signature."
        lightbulbslide
        f "" $ large $ center "We can run QuickSpec more than once!"
        g "Inferred Signature" $
            small $ do
                hask $
                    T.unlines
                        [ "type SignatureInferenceStrategy"
                        , "    = [Function] -> [Function] -> InferredSignature"
                        ]
                pause
                "Combine the results of a run:"
                hask $ "type InferredSignature = [Signature]"
                pause
                "Combine the results of a run, and use them as background properties:"
                hask $ "type InferredSignature = Forrest Signature"
                pause
                "Combine the results, use them for optimisation, and share previous runs:"
                hask $ "type InferredSignature = DAG Signature"
                pause
                "Combine the results, use them for optimisation, share previous runs and allow for reflection:"
                hask $
                    T.unlines
                        [ "type InferredSignature ="
                        , "    DAG ([Signature, [Equation]) -> Signature)"
                        ]
        g "Trivial Strategies" $ do
            hask $
                T.unlines
                    [ "emptyBackground focus _"
                    , "    = DAG.singleton $ const focus"
                    ]
            pause
            hask $
                T.unlines
                    [ "fullBackground _ scope"
                    , "    = DAG.singleton $ const scope"
                    ]
        g "Chunks" $ do
            small $ hask "chunks :: SignatureInferenceStrategy"
            vspace $ Cm 0.5
            footnotesize $
                hask $
                T.unlines
                    [ "> chunks"
                    , ">     [sort :: Ord a => [a] -> [a]]"
                    , ">     [reverse :: [a] -> [a], id :: a -> a]"
                    ]
            footnotesize $
                mintedText $
                T.unlines
                    [ "[sort, reverse]"
                    , "        |"
                    , "        v"
                    , "     -> [sort]"
                    , "     |"
                    , "     |"
                    , "[sort, id]"
                    ]
        g "Inferred Signature" $
            small $ do
                hask $
                    T.unlines
                        [ "type SignatureInferenceStrategy"
                        , "    = [Function] -> [Function] -> InferredSignature"
                        ]
                pause
                "Combine the results, use them for optimisation, share previous runs and allow for reflection:"
                hask $
                    T.unlines
                        [ "type InferredSignature ="
                        , "    DAG ([Signature, [Equation]) -> Signature)"
                        ]
        pictureSlide
            "The runtime of full breakthrough"
            assetRuntimeFullBreakthroughFullBreakthroughPlot
        pictureSlide
            "The outcome of full breakthrough: Relevant equations"
            assetRelevantEquationsFullBreakthroughFullBreakthroughPlot
        pictureSlide "Why does full breakthrough find more relevant equations?" $
            assetEquationsFullBreakthroughFullBreakthroughPlot
        g "Why does full breakthrough find more relevant equations?" $
            tiny $ do
                "Scope:"
                hask $
                    T.unlines
                        [ "i = (+ 1)"
                        , "j = (+ 2)"
                        , "k = (+ 3)"
                        , "l = (+ 4)"
                        , "m = (+ 5)"
                        , "n = (+ 6)"
                        , "o = (+ 7)"
                        , "p = (+ 8)"
                        , "q = (+ 9)"
                        , "r = (+ 10)"
                        ]
                pause
                vfill
                minipage Nothing (raw "0.4" <> textwidth) $ do
                    "Full background:"
                    mintedText $
                        T.unlines
                            [ "i (i x) = j x"
                            , "i (j x) = k x"
                            , "i (k x) = l x"
                            , "i (l x) = m x"
                            , "i (m x) = n x"
                            , "i (n x) = o x"
                            , "i (o x) = p x"
                            , "i (p x) = q x"
                            , "i (q x) = r x"
                            ]
                    "Relevant to r:"
                    mintedText "i (q x) = r x"
                pause
                minipage Nothing (raw "0.4" <> textwidth) $ do
                    "Full breakthrough for r:"
                    mintedText $
                        T.unlines
                            [ "q (i x) = r x"
                            , "q (q x) = p (r x)"
                            , "q (q (q x)) = o (r (r x))"
                            , "q (q (q (q (q x)))) = m (r (r (r (r x))))"
                            , "q (q (q (q (q (q x))))) = l (r (r (r (r (r x)))))"
                            ]
                    "All relevant"
        f "Great promise, but ..." $ do
            enumerate $ do
                item
                    "Only works for functions in scope of which the type is in scope too."
                pause
                item "Crashes on partial functions."
                pause
                item "Only works with built in instances."
                pause
                item "Data has to have an Arbitrary instance in scope."
                pause
                item "Does not play with CPP."
                pause
                item "Does not play well with higher kinded type variables"
            pause
            "All technical problems, not theoretical problems!"
        f "Further Research" $
            enumerate $ do
                item "Can we go faster?"
                pause
                item "Which constants do we choose for built in types?"
                pause
                item "Can we apply this to effectful code?"
        g "Call to action" $ do
            "Proofs of concept:"
            mintedText $
                T.unlines
                    [ "https://github.com/nick8325/quickcheck"
                    , "https://github.com/nick8325/quickspec"
                    , "https://github.com/NorfairKing/easyspec"
                    ]
            "Now we need to make it production ready!"
