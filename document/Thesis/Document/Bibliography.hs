{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Bibliography
    ( thesisBibliography
    ) where

import DocImport

thesisBibliography :: Thesis
thesisBibliography = do
    comm1 "bibliographystyle" "plain"
    comm1 "bibliography" $ raw "thesis"
