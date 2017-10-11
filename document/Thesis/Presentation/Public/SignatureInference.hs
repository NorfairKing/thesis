{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Public.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies
import Thesis.Presentation.Public.Utils

signatureInference :: Thesis
signatureInference = do
    pauseSlide 2
    section "Signature Inference" $ do
        pictureSlide "Automated, but still slow" assetRuntimeFullBackgroundPlot
        note ["Now we have automated QuickSpec, but it still slow"]
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
        f "Why is this slow?" $
            enumerate $ do
                item "Maximum size of the discovered properties"
                pause
                item "Size of the signature"
        lightbulbslide
        f "Critical insight" $ do
            center $ s "We are not interested in the entire codebase."
            center $ s "We are interested in a relatively small amount of code."
            note
                [ s "This means that we have an entirely different goal than QuickSpec."
                , s "Comparisons with QuickSpec are not really fair, but we have nothing else to compare to."
                ]
        g "Reducing the size of the signature" $
            hask $
            T.unlines
                [ "inferSignature"
                , "  :: [Function] -- Focus functions"
                , "  -> [Function] -- Functions in scope"
                , "  -> [Function] -- Chosen functions"
                ]
        g "Full background and empty background" $ do
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
        pauseSlide 3
        g "Syntactic similarity: Name" $ do
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
        g "Syntactic similarity: Implementation" $ do
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
        g "Syntactic similarity: Type" $ do
            raw "\\setminted{highlightlines=5}"
            hask $
                T.unlines
                    [ "inferSyntacticSimilarityType i [focus] scope"
                    , "    = take i $ sortOn"
                    , "      (\\sf ->"
                    , "        distance"
                    , "          (typeParts focus) (typeParts sf))"
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
            hask $
                T.unlines
                    [ "type SignatureInferenceStrategy"
                    , "    = [Function] -> [Function] -> InferredSignature"
                    ]
            pause
            "Combine the results of multiple runs:"
            hask "type InferredSignature = [Signature]"
            pause
            "User previous results as background properties:"
            hask "type InferredSignature = Forest Signature"
            pause
            "Share previous runs:"
            hask "type InferredSignature = DAG Signature"
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
            "The runtime of chunks"
            assetRuntimeFullBackgroundChunksPlot
        pictureSlide
            "The outcome of chunks: Relevant equations"
            assetRelevantEquationsFullBackgroundChunksPlot
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
        g "Call to action" $ do
            "Proofs of concept:"
            mintedText $
                T.unlines
                    [ "https://github.com/nick8325/quickcheck"
                    , "https://github.com/nick8325/quickspec"
                    , ""
                    , "https://github.com/NorfairKing/easyspec"
                    ]
            "Now we need to make it production ready!"
