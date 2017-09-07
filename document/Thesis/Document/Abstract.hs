{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Abstract
    ( thesisAbstract
    ) where

import DocImport

thesisAbstract :: Thesis
thesisAbstract = do
    newpage
    abstract $
        -- Setting / Motivation
     do
        s
            "Property discovery is the process of discovering properties of code via automated testing."
        s
            "It has the potential to be a great tool for a practical approach to software correctness."
        -- Problem statement
        s
            "Unfortunately current methods remain infeasible because of the immense search space."
        -- Approach
        l
            [ "We contribute a new approach to taming the complexity of a"
            , raw "state-of-the-art"
            , "generate and test algorithm"
            ]
        s
            "Our approach runs this algorithm several times with carefully chosen inputs of smaller size: signature inference."
        s "We implement this approach in a new tool called EasySpec."
        -- Results
        s
            "The results suggest that this approach is several orders of magnitude faster, fast enough to be practical, and produces similar results."
