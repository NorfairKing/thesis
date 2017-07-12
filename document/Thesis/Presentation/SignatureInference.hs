{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Dependencies

signatureInference :: Thesis
signatureInference =
    section "Signature Inference" $ do
        pictureSlide "Automated, but still slow" assetRuntimeFullBackgroundPlot
        f "Biggest Problems" $
            enumerate $ do
                item "Maximum size of the properties discovered"
                pause
                item "Size of the signature"
        lightbulbslide
        f "Critical insight" $ do
            center "We are not interested in the entire codebase."
            center "We are interested in the newest bit of code."
        g "Codebase" $ do
            "Usual Example:"
            tiny $
                mintedText $
                T.unlines
                    [ "$ cloc examples/evaluation/Monoid.hs"
                    , "       1 text file."
                    , "       1 unique file."
                    , "       0 files ignored."
                    , ""
                    , "github.com/AlDanial/cloc v 1.72  T=0.00 s (298.3 files/s, 1491.7 lines/s)"
                    , "-------------------------------------------------------------------------------"
                    , "Language                     files          blank        comment           code"
                    , "-------------------------------------------------------------------------------"
                    , "Haskell                          1              2              0             13"
                    , "-------------------------------------------------------------------------------"
                    ]
            "Real code:"
            tiny $
                mintedText $
                T.unlines
                    [ "$ cloc easyspec/{src,test} easyspec-evaluate/{src,test}"
                    , "      87 text files."
                    , "      86 unique files."
                    , "       1 file ignored."
                    , ""
                    , "github.com/AlDanial/cloc v 1.72  T=0.09 s (941.4 files/s, 76775.7 lines/s)"
                    , "-------------------------------------------------------------------------------"
                    , "Language                     files          blank        comment           code"
                    , "-------------------------------------------------------------------------------"
                    , "Haskell                         86           1018            163           5833"
                    , "-------------------------------------------------------------------------------"
                    , "SUM:                            86           1018            163           5833"
                    , "-------------------------------------------------------------------------------"
                    ]
        g "Reducing the size of the signature" $ do
            hask $
                T.unlines
                    [ "inferSignature"
                    , "  :: [Function] -- Focus functions"
                    , "  -> [Function] -- Functions in scope"
                    , "  -> [Function] -- Chosen functions"
                    ]
            vfill
            "Possible proxies for relevancy"
            enumerate $ do
                item "Syntactical similarity of the name by character"
                item "Syntactical similarity of the implementation by symbol"
                item "Syntactical similarity of the type by symbol"
                item "Similarity using a different metric"
                item "Combinations of the above"
        pictureSlide
            "Different functions in properties"
            assetNrDifferentFunctionsPlot
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
                "Combine the results of a run:"
                hask $ "type InferredSignature = [Signature]"
                pause
                "Combine the results of a run, and use them as background properties:"
                hask $ "type InferredSignature = Forrest Signature"
                pause
                "Combine the results, use them for optimisation, and share previous runs:"
                hask $ "type InferredSignature = DAG Signature"
                pause
                vspace $ Cm 0.5
                hask $
                    T.unlines
                        [ "type SignatureInferenceStrategy"
                        , "    = [Function] -> [Function] -> InferredSignature"
                        ]
        g "Trivial Strategies" $ do
            hask $
                T.unlines
                    ["emptyBackground focus _", "    = DAG.singleton focus"]
            pause
            hask $
                T.unlines
                    ["fullBackground _ scope", "    = DAG.singleton scope"]
        g "Full Breakthrough" $ do
            small $ hask "fullBreakthrough :: Int -> SignatureInferenceStrategy"
            vspace $ Cm 0.5
            footnotesize $
                hask $
                T.unlines
                    [ "> fullBreakthrough 1"
                    , "    [sort :: Ord a => [a] -> [a]]"
                    , "    [reverse :: [a] -> [a], id :: a -> a]"
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
