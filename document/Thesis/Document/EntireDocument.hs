{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.EntireDocument
    ( entireDocument
    ) where

import DocImport

import Thesis.Document.Abstract
import Thesis.Document.Background
import Thesis.Document.Bibliography
import Thesis.Document.Conclusion
import Thesis.Document.DocTechDetails
import Thesis.Document.Introduction
import Thesis.Document.TitlePage

import Text.LaTeX as HaTeX (article)

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] HaTeX.article
    headersAndFooters
    document $ do
        titlePage
        docTechDetails
        thesisAbstract
        newpage
        thesisTableOfContents
        thesisIntroduction
        thesisBackground
        thesisConclusion
        thesisBibliography

thesisTableOfContents :: Thesis
thesisTableOfContents = do
    packageDep ["hidelinks"] "hyperref" -- To make table of contents clickable
    tableofcontents
    newpage
