{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Main
    ( buildThesisDocumentIn
    ) where

import Import hiding (All)

import Text.LaTeX
import Text.LaTeX.LambdaTeX

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
    print config
    eet <- buildLaTeXProject entireDocument config
    case eet of
        Left errs -> die $ unlines $ map show errs
        Right () -> pure ()

type Thesis = Thesis' ()

type Thesis' = ΛTeXT IO

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] Text.LaTeX.article
    document $ do
        "hello"
        "world"
