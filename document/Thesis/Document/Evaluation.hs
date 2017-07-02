{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation
    ( thesisEvaluation
    ) where

import DocImport

import Thesis.Document.Assets
import Thesis.Document.Dependencies

thesisEvaluation :: Thesis
thesisEvaluation =
    section "Evaluation" $ do
        s "In this section I will discuss the evaluation of our work."
        subsection "Practical Feasability of previous work" $
            l
                [ "Even with the QuickSpec code generation automated, the complexity of the"
                , fullBackground
                , "signature inference strategy still makes it completely unusable for practical use"
                ]
        center $
            withRegisteredAsset assetRuntimePlot $ \runtimePlotFile ->
                includegraphics
                    [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
                    runtimePlotFile
