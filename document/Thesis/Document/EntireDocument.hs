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
        addDraftWatermark
        thesisAbstract
        newpage
        thesisTableOfContents
        thesisIntroduction
        thesisConclusion
        thesisBibliography

thesisTableOfContents :: Thesis
thesisTableOfContents = do
    packageDep ["hidelinks"] "hyperref" -- To make table of contents clickable
    tableofcontents
    newpage
