{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Bibliography
    ( thesisBibliography
    ) where

import DocImport

import Thesis.Document.References

thesisBibliography :: Thesis
thesisBibliography = do
    nocite quickcheckRef
    nocite quickspecRef
    comm1 "bibliographystyle" "plain"
    comm1 "bibliography" $ raw "thesis"
