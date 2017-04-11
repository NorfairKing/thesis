module Thesis.Document.Types where

import Import

import Control.Monad.Reader

import Text.LaTeX.LambdaTeX

import Language.Aspell as Aspell

type Thesis = Thesis' ()

type Thesis' = ΛTeXT (ReaderT ThesisEnv IO)

data ThesisEnv = ThesisEnv
    { spellChecker :: Aspell.SpellChecker
    , buildKind :: BuildKind
    , projectConfig :: ProjectConfig
    }

data BuildKind
    = BuildDraft
    | BuildFinal
    deriving (Show, Eq)
