{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Main
    ( buildThesisDocument
    ) where

import Import hiding (All)

import Development.Shake

import Text.LaTeX
import Text.LaTeX.LambdaTeX

buildThesisDocument :: IO ()
buildThesisDocument = do
    let tmpDir = $(mkRelDir "tmp")
    ensureDir tmpDir
    withCurrentDir tmpDir $ do
        let config =
                ProjectConfig
                { projectGenerationConfig =
                      GenerationConfig {generationSelection = [All]}
                , projectBibFileName = "thesis"
                , projectTexFileName = "thesis"
                }
        eet <- buildLaTeXProject entireDocument config
        case eet of
            Left errs -> die $ unlines $ map show errs
            Right () -> do
                cmd
                    [ "latexmk" :: String
                    , "-pdf"
                    , "thesis.tex"
                    , "-shell-escape"
                    , "-halt-on-error"
                    ]

type Thesis = Thesis' ()

type Thesis' = ΛTeXT IO

entireDocument :: Thesis
entireDocument = do
    documentclass [oneside, a4paper] Text.LaTeX.article
    document $ do
        "hello"
        "world"
