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
            "It has the potential to be a great tool for practical software correctness."
        -- Problem statement
        s
            "Unfortunately current methods remain infeasible because of the immense search space."
        -- Approach
        s
            "We contribute a new approach to taming the computational complexity of the current generate and test algorithm that runs this algorithm several times with carefully chosen inputs of smaller size: signature inference."
        s "We implement this approach in a new tool called EasySpec."
        -- Results
        s
            "The results suggest that this approach is several orders of magnitude faster, fast enough to be practical, and does not sacrifice any results."
        s
            "In fact, this approach improves upon the current property discovery algorithm with respect to the number of relevant equations discovered as well."
