module Thesis.Document.Types where

import Import

import Control.Monad.Reader

import Text.LaTeX.LambdaTeX

import Language.Aspell as Aspell

type Thesis = Thesis' ()

type Thesis' = ΛTeXT (ReaderT ThesisEnv IO)

newtype ThesisEnv = ThesisEnv
    { spellChecker :: Aspell.SpellChecker
    }
