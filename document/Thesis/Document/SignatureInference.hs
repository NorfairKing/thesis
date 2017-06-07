{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

-- import Thesis.Document.References
thesisSignatureInference :: Thesis
thesisSignatureInference =
    section
        "Signature Inference"
        "Signature inference is best introduced by explaining the thought process that lead to its definition."
