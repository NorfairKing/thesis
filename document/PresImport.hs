{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PresImport
    ( module PresImport
    , module DocImport
    , module X
    ) where

import DocImport

import System.FilePath

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Beamer as X

import Thesis.Document.Assets

subtitle :: Thesis -> Thesis
subtitle = comm1 "subtitle"

solarizedtheme :: Asset
solarizedtheme = moveup $(embedAsset "beamercolorthemesolarized.sty")
  where
    moveup :: Asset -> Asset
    moveup a = a {assetPath = takeFileName $ assetPath a}

usecolortheme :: Thesis -> Thesis -> Thesis
usecolortheme =
    liftL2 $ \a1 a2 -> TeXComm "usecolortheme" [OptArg a1, FixArg a2]

url :: Text -> Thesis
url = comm1 "url" . raw

f :: Thesis -> Thesis -> Thesis
f title content =
    frame $ do
        frametitle title
        content
