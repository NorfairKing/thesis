{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Academic.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Presentation.Public.Utils

signatureInference :: Thesis
signatureInference =
    section "Signature Inference" $ do
        pictureSlide "Automated, but still slow" assetRuntimeFullBackgroundPlot
        note ["Now we have automated QuickSpec, but it still slow"]
        g "Definition: Property" $ do
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
        f "Why is this slow?" $
            enumerate $ do
                item "Maximum size of the discovered properties"
                pause
                item "Size of the signature"
        lightbulbslide
        f "Critical Insight" $ do
            center $ s "We are not interested in the entire codebase."
            center $ s "We are interested in a relatively small amount of code."
            note
                [ s "This means that we have an entirely different goal than QuickSpec."
                , s "Comparisons with QuickSpec are not really fair, but we have nothing else to compare to."
                ]
        g "Reducing the Size of the Signature" $
            hask $
            T.unlines
                [ "inferSignature"
                , "  :: [Function] -- Focus functions"
                , "  -> [Function] -- Functions in scope"
                , "  -> [Function] -- Chosen functions"
                ]
        g "Full Background and Empty Background" $ do
            hask "inferFullBackground _ scope = scope"
            hask "inferEmptyBackground focus _ = focus"
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeFullBackgroundEmptyBackgroundPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundEmptyBackgroundPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic Similarity: Name" $ do
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityName [focus] scope"
                    , "    = take 5 $ sortOn"
                    , "      (\\sf ->"
                    , "        distance"
                    , "          (name focus) (name sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeChunksSyntacticSimilarityNamePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityNamePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic Similarity: Implementation" $ do
            raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "inferSyntacticSimilaritySymbols i [focus] scope"
                    , "    = take i $ sortOn"
                    , "      (\\sf ->"
                    , "        distance"
                    , "          (symbols focus) (symbols sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeChunksSyntacticSimilaritySymbolsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilaritySymbolsPlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        g "Syntactic Similarity: Type" $ do
            raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityType i [focus] scope"
                    , "    = take i $ sortOn"
                    , "      (\\sf ->"
                    , "        distance"
                    , "          (getTypeParts focus) (getTypeParts sf))"
                    , "      scope "
                    ]
            pause
            only [OneSlide 2] $
                withRegisteredAsset
                    assetRuntimeChunksSyntacticSimilarityTypePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
            pause
            only [OneSlide 3] $
                withRegisteredAsset
                    assetRelevantEquationsFullBackgroundSyntacticSimilarityTypePlot $ \fp ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure textwidth
                        ]
                        fp
        f "Other Things we Tried" $
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
        f "" $ huge $ center "We can run QuickSpec more than once!"
        g "Inferred Signature" $ do
            "Combine the results of multiple runs:"
            hask "[Signature]"
            pause
            "User previous results as background properties:"
            hask "Forest Signature"
            pause
            "Share previous runs:"
            hask "DAG Signature"
        g "Chunks" $ do
            small $ hask "chunks :: SignatureInferenceStrategy"
            footnotesize $
                hask $
                T.unlines
                    [ "> chunks"
                    , ">     [sort :: Ord a => [a] -> [a]]"
                    , ">     [reverse :: [a] -> [a], id :: a -> a]"
                    ]
            withDotAsset assetChunksDot $ \fp ->
                center $
                includegraphics
                    [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
                    fp
        pictureSlide
            "The Runtime of Chunks"
            assetRuntimeFullBackgroundChunksPlot
        pictureSlide
            "The Outcome of Chunks: Relevant equations"
            assetRelevantEquationsFullBackgroundChunksPlot
        pictureSlide
            "Why does chunks find more relevant equations?"
            assetEquationsFullBackgroundChunksPlot
        g "Why does chunks find more relevant equations?" $ do
            "Scope:"
            hask $
                T.unlines ["a = (+ 1)", "b = (+ 2)", "c = (+ 3)", "d = (+ 4)"]
            pause
            vfill
            minipage Nothing (raw "0.4" <> textwidth) $ do
                "Full background:"
                mintedText $
                    T.unlines
                        ["a (a x) = b x", "a (b x) = c x", "a (c x) = d x"]
                "Relevant to d:"
                mintedText "a (c x) = d x"
            pause
            hfill
            minipage Nothing (raw "0.4" <> textwidth) $ do
                "Chunks for d:"
                mintedText $
                    T.unlines ["b (b x) = d x", "a (a (a (a x))) = d x"]
                "All relevant"
            hfill
        g "Inferred Signature" $
            hask $
            T.unlines
                [ "type SignatureInferenceStrategy"
                , "    = [Function] -> [Function] -> InferredSignature"
                , ""
                , ""
                , ""
                , "type InferredSignature ="
                , "    DAG ([(Signature, [Equation])] -> Signature)"
                ]
        g "Inferred Signature" $
            small $
            hask $
            T.unlines
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
        g "Chunks Plus" $ do
            small $ hask "chunksPlus :: SignatureInferenceStrategy"
            footnotesize $
                hask $
                T.unlines
                    [ "> chunksPlus"
                    , ">     [sort :: Ord a => [a] -> [a]]"
                    , ">     [reverse :: [a] -> [a], id :: a -> a]"
                    ]
            withDotAsset assetChunksPlusDot $ \fp ->
                center $
                includegraphics
                    [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
                    fp
        pictureSlide
            "The runtime of chunks plus"
            assetRuntimeFullBackgroundChunksPlusPlot
        pictureSlide
            "The outcome of chunks plus: Relevant equations"
            assetRelevantEquationsFullBackgroundChunksPlusPlot
        g "Neat" $
            mintedText $
            T.unlines
                [ "$ time stack exec easyspec \\"
                , "      -- discover MySort.hs MySort.mySort"
                , ""
                , "xs <= mySort xs = myIsSorted xs"
                , "mySort xs <= xs = True"
                , "myIsSorted (mySort xs) = True"
                , "mySort (mySort xs) = mySort xs"
                , ""
                , "3.61s user 1.14s system 193% cpu 2.450 total"
                ]
        g "Composing Strategies" $
            haskL
                [ "type Reducing"
                , "    = [Function] -> [Function] -> [Function]"
                , ""
                , "type Drilling"
                , "    = [Function] -> [Function] -> InferM ()"
                ]
        g "Composing Strategies" $
            small $
            haskL
                [ "composeReducings :: Reducing -> Reducing -> Reducing"
                , "composeReducings r1 r2 focus = r2 focus . r1 focus"
                , ""
                , "composeDrillings :: Drilling -> Drilling -> Drilling"
                , "composeDrillings d1 d2 focus scope = do"
                , "    d1 focus scope"
                , "    d2 focus scope"
                , ""
                , "composeReducingWithDrilling"
                , "    :: Reducing -> Drilling -> Drilling"
                , "composeReducingWithDrilling r d focus scope"
                , "    = d focus $ r focus scope"
                ]
        pictureSlide
            "The runtime of chunks plus composed with reducings"
            assetRuntimeChunksPlusReducingsPlot
        pictureSlide
            "The outcome of chunks plus composed with reducings: Relevant equations"
            assetRelevantEquationsChunksPlusReducingsPlot
        pictureSlide "All strategies" assetRelevantEquationsAll
        f (raw "Great promise, but ...") $ do
            enumerate $ do
                pause
                item $
                    s
                        "Only works for functions in scope of which the type is in scope too."
                pause
                item $ s "Crashes on partial functions."
                pause
                item $ s "Only works with built in instances."
                pause
                item $ s "Data has to have an Arbitrary instance in scope."
                pause
                item $ s "Does not play with CPP."
                pause
                item $ s "Does not play well with higher kinded type variables."
            pause
            "All technical problems, not theoretical problems!"
        f "Further Research" $
            enumerate $ do
                item "Can we go faster?"
                pause
                item "Which constants do we choose for built in types?"
                pause
                item "Can we apply this to effectful code?"
                pause
                item "Relative importance of equations"
