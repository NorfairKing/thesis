{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Bibliography
    ( thesisBibliography
    ) where

import DocImport

import Control.Monad.State
import qualified Data.Text as T

import Thesis.Document.References

thesisBibliography :: Thesis
thesisBibliography =
    slow $ do
        packageDep ["hidelinks"] "hyperref"
        mapM_
            nocite
            [ quickcheckRef
            , quickspecRef
            , hipspecRef
            , smartcheckRef
            , fitspecRef
            , filteringRef
            , smallcheckRef
            , leancheckRef
            , speculateRef
            , ghcRef
            , mash2Ref
            ]
        comm1 "bibliographystyle" "plain"
        bibFileName <- gets (projectBibFileName . projectConfig)
        comm1 "bibliography" $ raw $ T.pack bibFileName
