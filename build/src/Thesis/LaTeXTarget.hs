{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.LaTeXTarget where

import Import hiding (All)

import qualified Data.ByteString.Char8 as SB8

import Control.Monad.State

import Text.LaTeX.LambdaTeX

import Thesis.Document.Assets
import Thesis.Document.Types

import qualified Language.Aspell as Aspell
import qualified Language.Aspell.Options as Aspell

buildLaTexTargetWithNameIn ::
       String
    -> Path Abs Dir
    -> BuildKind
    -> Selection
    -> Bool
    -> Thesis
    -> IO ()
buildLaTexTargetWithNameIn name bdir bkind selection fast document = do
    let config =
            ProjectConfig
            { projectGenerationConfig =
                  GenerationConfig {generationSelection = selection}
            , projectBibFileName = name
            , projectTexFileName = name
            , projectBuildDir = toFilePath bdir
            }
    sc <- startAspell bdir
    let env =
            ThesisEnv
            { spellChecker = Just sc
            , buildKind = bkind
            , projectConfig = config
            , fastBuild = fast
            }
    eet <- evalStateT (buildLaTeXProject (unThesis document) config) env
    case eet of
        Left errs -> die $ unlines $ map show errs
        Right () -> pure ()

startAspell :: Path Abs Dir -> IO Aspell.SpellChecker
startAspell rd = do
    dictFile <- makeAsset rd $ dictAsset $(embedAsset "custom_wordlist.txt")
    errOrSc <-
        Aspell.spellCheckerWithOptions
            [ Aspell.Dictionary "en_GB"
            , Aspell.Encoding Aspell.UTF8
            , Aspell.PersonalWordList $ SB8.pack $ toFilePath dictFile
            ]
    case errOrSc of
        Left err -> die $ unwords ["Unable to start aspell:", show err]
        Right sc -> pure sc
  where
    dictAsset wordListAsset =
        Asset
        { assetPath = "custom_dictionary.pws"
        , assetContents =
              SB8.concat
                  [ "personal_ws-1.1 en " <>
                    SB8.pack
                        (show (length (SB8.lines (assetContents wordListAsset)))) <>
                    " utf-8\n"
                  , assetContents wordListAsset
                  ]
        }
