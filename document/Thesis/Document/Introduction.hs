{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Introduction
    ( thesisIntroduction
    ) where

import DocImport

thesisIntroduction :: Thesis
thesisIntroduction = do
    section "Introduction" "Signature inference..."
