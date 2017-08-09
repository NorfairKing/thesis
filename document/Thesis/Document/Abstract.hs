{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Abstract
    ( thesisAbstract
    ) where

import DocImport

thesisAbstract :: Thesis
thesisAbstract = do
    newpage
    abstract $ do
        s
            "We contribute a technique that can make property discovery practically feasible by taming the complexity of the discovery algorithm."
        lnbk
        -- Motivation
        s
            "Property discovery has the potential to be a great tool for practical software correctness."
        -- Problem statement
        s
            "Unfortunately current methods remain infeasible because of the complexity of the discovery algorithm."
        -- Approach
        s
            "We contribute a new approach to taming the complexity of the discovery algorithm that runs the discovery algorithm multiple times with intelligently chosen inputs of smaller size: signature inference."
        -- Results
        s
            "The results suggest that this approach is multiple orders of magnitude faster, fast enough to be practical, and does not sacrifice any results."
        s
            "In fact, this approach improves upon the current property discovery algorithm with respect to results as well."
        -- Conclusions
        s
            "In conclusion, this contribution has made property discovery a feasible tool for use in practical software development."
