{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Main
    ( buildThesisDocumentIn
    ) where

import Import hiding (All)

import Text.LaTeX.LambdaTeX

import Thesis.Document.EntireDocument

buildThesisDocumentIn :: Path s Dir -> IO ()
buildThesisDocumentIn bdir = do
    let config =
            ProjectConfig
            { projectGenerationConfig =
                  GenerationConfig {generationSelection = [All]}
            , projectBibFileName = "thesis"
            , projectTexFileName = "thesis"
            , projectBuildDir = toFilePath bdir
            }
    eet <- buildLaTeXProject entireDocument config
    case eet of
        Left errs -> die $ unlines $ map show errs
        Right () -> pure ()
