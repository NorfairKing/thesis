{-# LANGUAGE OverloadedStrings #-}

module DocImport
    ( module X
    , DocImport.includegraphics
    , DocImport.titlepage
    , s
    , quoted
    , dquoted
    , DocImport.abstract
    , DocImport.section
    , DocImport.subsection
    , DocImport.subsubsection
    , DocImport.paragraph
    ) where

import Import as X

import Text.LaTeX as X
       hiding (ref, pageref, cite, article, label, titlepage, article,
               abstract, section, subsection, subsubsection, paragraph)
import Text.LaTeX.LambdaTeX as X hiding (Selector(..))

import qualified Text.LaTeX as HT
       (abstract, paragraph, section, subsection, subsubsection)
import Text.LaTeX.Base
import Text.LaTeX.Base.Class as X (comm0, comm1)
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

abstract :: Thesis -> Thesis
abstract func = do
    raw "\n"
    note "abstract" $ HT.abstract func

section :: Text -> Thesis -> Thesis
section n func = do
    raw "\n"
    note n $ do
        HT.section (raw n)
        func

subsection :: Text -> Thesis -> Thesis
subsection n func = do
    raw "\n"
    note n $ do
        HT.subsection (raw n)
        func

subsubsection :: Text -> Thesis -> Thesis
subsubsection n func = do
    raw "\n"
    note n $ do
        HT.subsubsection (raw n)
        func

paragraph :: Text -> Thesis -> Thesis
paragraph n func = do
    raw "\n"
    note n $ do
        HT.paragraph (raw n)
        func
