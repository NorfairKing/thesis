{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Abstract
    ( thesisAbstract
    ) where

import DocImport

thesisAbstract :: Thesis
thesisAbstract = do
    newpage
    abstract "We contribute ..."
