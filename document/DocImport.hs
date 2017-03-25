{-# LANGUAGE OverloadedStrings #-}

module DocImport
    ( module X
    , DocImport.includegraphics
    , DocImport.titlepage
    , l
    , s
    , quoted
    , dquoted
    , DocImport.abstract
    , DocImport.section
    , DocImport.subsection
    , DocImport.subsubsection
    , DocImport.paragraph
    , declarePart
    , citationNeeded
    ) where

import Import as X

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.LaTeX as X
       hiding (ref, pageref, cite, article, label, titlepage, article,
               abstract, section, subsection, subsubsection, paragraph)
import Text.LaTeX.LambdaTeX as X hiding (Selector(..))

import qualified Text.LaTeX as HT
       (abstract, paragraph, section, subsection, subsubsection)
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
-- To model a sentence with arbitrary contents.
l :: [Thesis] -> Thesis
l ns = do
    sequence_ $ intersperse " " ns
    ". "

-- To model a sentence that can be inspected
s :: Text -> Thesis
s t = do
    let f m = liftIO $ fail $ unlines [m, T.unpack t]
    when (T.null t) $ f "Sentence cannot be empty."
    unless (isUpper $ T.head t) $ f "Sentence must start with a capital."
    unless (T.last t == '.') $ f "Sentence must end in a full stop."
    fromString $ T.unpack t
    " "

quoted :: Thesis -> Thesis
quoted n = "`" <> n <> "'"

dquoted :: Thesis -> Thesis
dquoted n = raw "``" <> n <> raw "''"

abstract :: Thesis -> Thesis
abstract func = do
    raw "\n"
    declarePart "abstract" $ HT.abstract func

section :: Text -> Thesis -> Thesis
section n func = do
    raw "\n"
    declarePart n $ do
        HT.section (raw n)
        func

subsection :: Text -> Thesis -> Thesis
subsection n func = do
    raw "\n"
    declarePart n $ do
        HT.subsection (raw n)
        func

subsubsection :: Text -> Thesis -> Thesis
subsubsection n func = do
    raw "\n"
    declarePart n $ do
        HT.subsubsection (raw n)
        func

paragraph :: Text -> Thesis -> Thesis
paragraph n func = do
    raw "\n"
    declarePart n $ do
        HT.paragraph (raw n)
        func

declarePart :: Text -> Thesis -> Thesis
declarePart partname func = do
    let name = T.pack . kebabCase . sanitize . T.unpack $ partname
    note name $ do
        currentPart <- λgets stateCurrentPart
        let d = length $ unPart currentPart
        liftIO $ T.putStrLn $ T.replicate (2 * d) " " <> name
        func

sanitize :: String -> String
sanitize = concatMap replaceBad
  where
    replaceBad :: Char -> String
    replaceBad '-' = " "
    replaceBad '\'' = ""
    replaceBad c = [c]

kebabCase :: String -> String
kebabCase str = intercalate "-" $ words $ map toLower str

citationNeeded :: Thesis
citationNeeded = "[CITATION NEEDED]"
