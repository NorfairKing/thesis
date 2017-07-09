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
        pictureSlide
            "Different functions in properties"
            assetNrDifferentFunctionsPlot
