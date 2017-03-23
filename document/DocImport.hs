{-# LANGUAGE OverloadedStrings #-}

module DocImport
    ( module X
    , DocImport.includegraphics
    , DocImport.titlepage
    , s
    , quoted
    , dquoted
    ) where

import Import as X

import Text.LaTeX as X
       hiding (ref, pageref, cite, article, label, titlepage, article)
import Text.LaTeX.LambdaTeX as X hiding (Selector(..))

import Text.LaTeX.Base
import Text.LaTeX.Base.Class  as X (comm0, comm1)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Graphicx as X (IGOption(..))
import Text.LaTeX.Packages.Graphicx as HaTeX

import Thesis.Document.Types as X

includegraphics :: [IGOption] -> FilePath -> Thesis
includegraphics opts path = do
    packageDep_ "graphicx"
    HaTeX.includegraphics opts path

titlepage :: Thesis -> Thesis
titlepage = liftL $ TeXEnv "titlepage" []

-- Shorter than sequence_
-- To model a sentence.
s :: [Thesis] -> Thesis
s ns = do
    sequence_ $ intersperse " " ns
    ". "

quoted :: Thesis -> Thesis
quoted n = "`" <> n <> "'"

dquoted :: Thesis -> Thesis
dquoted n = raw "``" <> n <> raw "''"
