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
        , ghcRef
        , filteringRef
        , mash2Ref
        ]
    comm1 "bibliographystyle" "plain"
    comm1 "bibliography" $ raw "thesis"
