{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Abstract
    ( thesisAbstract
    ) where

import DocImport

thesisAbstract :: Thesis
thesisAbstract = do
    newpage
    abstract
        "We contribute a technique that can make property discovery practically feasible by taming the complexity"
