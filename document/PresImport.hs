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
f t_ content =
    frame $ do
        frametitle t_
        content

g :: Thesis -> Thesis -> Thesis
g t_ content =
    fragileFrame $ do
        frametitle t_
        content

fragileFrame :: Thesis -> Thesis
fragileFrame content = do
    raw "\n\\begin{frame}[fragile]\n"
    content
    raw "\n\\end{frame}\n" -- This must not be indented or have comments behind it.

pictureSlide :: Thesis -> Asset -> Thesis
pictureSlide title_ asset =
    f title_ $
    withRegisteredAsset asset $ \fp ->
        includegraphics
            [KeepAspectRatio True, IGWidth $ CustomMeasure textwidth]
            fp

haskFile :: Asset -> Thesis
haskFile = mintedFile "haskell"

verbatimFile :: Asset -> Thesis
verbatimFile = mintedFile "text"

mintedFile :: Text -> Asset -> Thesis
mintedFile lang asset =
    withRegisteredAsset asset $ \fp ->
        comm2 "inputminted" (raw lang) $ raw $ fromString fp
