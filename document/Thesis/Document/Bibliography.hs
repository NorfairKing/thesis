{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Bibliography
    ( thesisBibliography
    ) where

import DocImport

import Thesis.Document.References

thesisBibliography :: Thesis
thesisBibliography = do
    mapM_
        nocite
        [ quickcheckRef
        , quickspecRef
        , hipspecRef
        , smartcheckRef
        , fitspecRef
        , filteringRef
        , smallcheckRef
        , ghcRef
        , mash2Ref
        ]
    comm1 "bibliographystyle" "plain"
    comm1 "bibliography" $ raw "thesis"
