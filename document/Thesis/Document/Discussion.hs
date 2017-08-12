{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Discussion where

import DocImport

thesisDiscussion :: Thesis
thesisDiscussion =
    section "Discussion" $
        -- Answer questions that people usually have
        -- What are the downsides
     do
        subsection "Shortcomings" $ do
            s "EasySpec produces false positives, just like QuickSpec."
            s
                "EasySpec can only discover properties that already hold (modulo false positives)."
            s
                "This means that properties that you may want to have hold about code will not be discovered if the code does not already satisfy those properties."
        subsection "Configurability" $ do
            s "Multiple user interfaces for property discovery can be imagined."
            s
                "Different signature inference strategies make it possible to support multiple use cases."
