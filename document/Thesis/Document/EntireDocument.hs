{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.EntireDocument
    ( entireDocument
    ) where

import DocImport

import Thesis.Document.Abstract
import Thesis.Document.Acknowledgements
import Thesis.Document.Background
import Thesis.Document.Bibliography
import Thesis.Document.Conclusion
import Thesis.Document.DocTechDetails
import Thesis.Document.Evaluation
import Thesis.Document.Introduction
import Thesis.Document.PropertyDiscovery
import Thesis.Document.SignatureInference
import Thesis.Document.TitlePage

import Text.LaTeX as HaTeX (article)

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] HaTeX.article
    headersAndFooters
    document $ do
        titlePage
        docTechDetails
        mintedColors
        thesisAbstract
        newpage
        thesisAcknowledgements
        newpage
        thesisTableOfContents
        thesisIntroduction
        thesisBackground
        thesisPropertyDiscovery
        thesisSignatureInference
        thesisEvaluation
        thesisConclusion
        thesisBibliography

thesisTableOfContents :: Thesis
thesisTableOfContents =
    slow $ do
        packageDep ["hidelinks"] "hyperref" -- To make table of contents clickable
        slow $ do
            tableofcontents
            comm0 "listoffigures"
            -- comm0 "listoftables" TODO uncomment if I have any tables
            newpage

mintedColors :: Thesis
mintedColors = do
    packageDep_ "minted"
    packageDep_ "xcolor"
    comm3 "definecolor" (raw "mintedbgcolor") (raw "rgb") (raw "0.95,0.95,0.95")
