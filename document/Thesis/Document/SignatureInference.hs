{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference
    ( thesisSignatureInference
    ) where

import DocImport

import Thesis.Document.SignatureInference.Automation
import Thesis.Document.SignatureInference.Distance
import Thesis.Document.SignatureInference.DrillingAndReducing
import Thesis.Document.SignatureInference.Graph
import Thesis.Document.SignatureInference.Monadic
import Thesis.Document.SignatureInference.Premise
import Thesis.Document.SignatureInference.Reducing
import Thesis.Document.SignatureInference.TypeReachability

thesisSignatureInference :: Thesis
thesisSignatureInference =
    section "Signature Inference with EasySpec" $ do
        s
            "In this section we will introduce the concept of signature inference and how we implemented it in the EasySpec tool."
        thesisSignatureInferencePremise
        thesisSignatureInferenceAutomation
        thesisSignatureInferenceReducing
        thesisSignatureInferenceDistance
        thesisSignatureInferenceTypeReachability
        thesisSignatureInferenceGraph
        thesisSignatureInferenceMonadic
        thesisSignatureInferenceDrillingAndReducing
