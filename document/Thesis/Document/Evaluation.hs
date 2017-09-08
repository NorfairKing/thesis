{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation
    ( thesisEvaluation
    ) where

import DocImport

import Thesis.Document.Evaluation.DiscoveryComplexity
import Thesis.Document.Evaluation.Evaluators
import Thesis.Document.Evaluation.Experiments
import Thesis.Document.Evaluation.Strategies

thesisEvaluation :: Thesis
thesisEvaluation =
    section "Evaluation" $ do
        s
            "In this section we evaluate the different signature inference strategies that were introduced in the previous section."
        thesisEvaluationDiscoveryComplexity
        thesisEvaluationEvaluators
        thesisEvaluationExperiments
        thesisEvaluationStrategies
