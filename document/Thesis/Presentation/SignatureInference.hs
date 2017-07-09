{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies

import Thesis.Presentation.Motivation

signatureInference :: Thesis
signatureInference =
    section "Signature Inference" $ do
        f "" $ center "Automated, but still slow"
        f "Biggest Problems" $
            enumerate $ do
                item "Maximum size of the properties discovered"
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
                "Combine the results of a run, and use them as background properties:"
                hask $ "type InferredSignature = Forrest Signature"
                "Combine the results, use them for optimisation, and share previous runs:"
                hask $ "type InferredSignature = DAG Signature"
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
                    , "    [sort :: a -> a]"
                    , "    [reverse :: [a] -> [a], id :: Ord a => [a] -> [a]]"
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
