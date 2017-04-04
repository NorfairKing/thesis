{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document.Main
    ( buildThesisDocumentIn
    ) where

import Import hiding (All)

import qualified Data.ByteString.Char8 as SB8

import Control.Monad.Reader

import Text.LaTeX.LambdaTeX

import Thesis.Document.Assets
import Thesis.Document.EntireDocument
import Thesis.Document.Types

import qualified Language.Aspell as Aspell
import qualified Language.Aspell.Options as Aspell

buildThesisDocumentIn :: Path Abs Dir -> IO ()
buildThesisDocumentIn bdir = do
    let config =
            ProjectConfig
            { projectGenerationConfig =
                  GenerationConfig {generationSelection = [All]}
            , projectBibFileName = "thesis"
            , projectTexFileName = "thesis"
            , projectBuildDir = toFilePath bdir
            }
    sc <- startAspell bdir
    let env = ThesisEnv {spellChecker = sc}
    eet <- runReaderT (buildLaTeXProject entireDocument config) env
    case eet of
        Left errs -> die $ unlines $ map show errs
        Right () -> pure ()

startAspell :: Path Abs Dir -> IO Aspell.SpellChecker
startAspell rd = do
    let dictAsset = $(embedAsset "custom_dictionary.pws")
    dictFile <- makeAsset rd dictAsset
    errOrSc <-
        Aspell.spellCheckerWithOptions
            [ Aspell.Dictionary "en_GB"
            , Aspell.Encoding Aspell.UTF8
            , Aspell.PersonalWordList $ SB8.pack $ toFilePath dictFile
            ]
    case errOrSc of
        Left err -> die $ unwords ["Unable to start aspell:", show err]
        Right sc -> pure sc
