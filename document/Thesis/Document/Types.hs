{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Thesis.Document.Types where

import Import

import GHC.Generics

import Data.String

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.Reader

import qualified Language.Aspell as Aspell

import Text.LaTeX.Base.Class
import Text.LaTeX.LambdaTeX

type Thesis = Thesis' ()

newtype Thesis' a = Thesis
    { unThesis :: ΛTeXT (ReaderT ThesisEnv IO) a
    } deriving ( Generic
               , Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader ThesisEnv
               )

deriving instance a ~ () => LaTeXC (Thesis' a)

instance a ~ () => IsString (Thesis' a) where
    fromString s = do
        spellCheck $ T.pack s
        Thesis $ fromString s

instance Monoid (Thesis' ()) where
    mempty = Thesis mempty
    mappend (Thesis t1) (Thesis t2) = Thesis $ mappend t1 t2

t :: ΛTeXT (ReaderT ThesisEnv IO) a -> Thesis' a
t = Thesis

data ThesisEnv = ThesisEnv
    { spellChecker :: Aspell.SpellChecker
    , buildKind :: BuildKind
    , projectConfig :: ProjectConfig
    }

data BuildKind
    = BuildDraft
    | BuildFinal
    deriving (Show, Eq)

spellCheck :: Text -> Thesis
spellCheck text = do
    sc <- asks spellChecker
    forM_ (T.words text) $ \w -> do
        let wbs = T.encodeUtf8 $ filterBad w
        unless (SB.null wbs) $ do
            sugs <- liftIO $ Aspell.suggest sc wbs
            case find (== wbs) sugs of
                Just _ -> pure ()
                Nothing ->
                    liftIO $
                    fail $
                    unlines $
                    unwords
                        [ "Aspell had suggestions for"
                        , show wbs
                        , "in the sentence:"
                        , show text
                        ] :
                    map show sugs
  where
    filterBad = T.filter (not . (`elem` badChars))
    badChars = ['.', ',', ':', '?', '(', ')']
