{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.EntireDocument
    ( entireDocument
    ) where

import DocImport

import Thesis.Document.Abstract
import Thesis.Document.Bibliography
import Thesis.Document.Conclusion
import Thesis.Document.DocTechDetails
import Thesis.Document.Introduction
import Thesis.Document.TitlePage

import Text.LaTeX as HaTeX (article)

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] HaTeX.article
    document $ do
        titlePage
        docTechDetails
        thesisAbstract
        newpage
        tableofcontents
        newpage
        thesisIntroduction
        thesisConclusion
        thesisBibliography
