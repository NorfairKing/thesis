{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.EntireDocument
    ( entireDocument
    ) where

import DocImport

import Thesis.Document.TitlePage
import Thesis.Document.DocTechDetails

import Text.LaTeX as HaTeX (article)

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] HaTeX.article
    document $ do
        titlePage
        docTechDetails
