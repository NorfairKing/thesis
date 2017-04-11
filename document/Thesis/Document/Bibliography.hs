{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Bibliography
    ( thesisBibliography
    ) where

import DocImport

import Control.Monad.Reader
import qualified Data.Text as T

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
    bibFileName <- asks (projectBibFileName . projectConfig)
    comm1 "bibliography" $ raw $ T.pack bibFileName
