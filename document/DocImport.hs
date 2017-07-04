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
    , headersAndFooters
    , hask
    , haskInline
    , mintedText
    , mintedTextInline
    , minted
    , mintedInline
    , DocImport.item
    , DocImport.cite
    , DocImport.nocite
    , DocImport.packageDep
    , DocImport.packageDep_
    , emptyBackground
    , fullBackground
    ) where

import Import as X

import Control.Monad.Reader

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.LaTeX as X
       hiding (abstract, article, article, cite, item, label, pageref,
               paragraph, ref, section, subsection, subsubsection, titlepage)
import Text.LaTeX.LambdaTeX as X
       hiding (Selector(..), cite, nocite, packageDep, packageDep_)
import qualified Text.LaTeX.LambdaTeX as LT
       (cite, nocite, packageDep, packageDep_)

import qualified Text.LaTeX as HT
       (abstract, item, paragraph, section, subsection, subsubsection)
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
    raw ". "

-- To model a sentence that can be inspected
s :: Text -> Thesis
s t_ = do
    let f m = liftIO $ fail $ unlines [m, T.unpack t_]
    when (T.null t_) $ f "Sentence cannot be empty."
    unless (isUpper $ T.head t_) $ f "Sentence must start with a capital."
    unless (T.last t_ == '.') $ f "Sentence must end in a full stop."
    spellCheck t_
    fromString $ T.unpack t_
    raw " "

quoted :: Thesis -> Thesis
quoted n = raw "`" <> n <> raw "'"

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
declarePart partname (Thesis func) = do
    let name = T.pack . kebabCase . sanitize . T.unpack $ partname
    t $
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
citationNeeded = raw "[CITATION NEEDED]"

headersAndFooters :: Thesis
headersAndFooters = do
    packageDep_ "fancyhdr"
    comm1 "pagestyle" "fancy"
    comm0 "fancyhf"
    bkind <- asks buildKind
    when (bkind == BuildDraft) $
        comm1
            "cfoot"
            "This is an unfinished draft. Please do not distribute it."

hask :: Text -> Thesis
hask = minted "haskell"

haskInline :: Text -> Thesis
haskInline = mintedInline "haskell"

mintedText :: Text -> Thesis
mintedText = minted "text"

mintedTextInline :: Text -> Thesis
mintedTextInline = mintedInline "text"

minted :: Text -> Text -> Thesis
minted language code = do
    packageDep_ "minted"
    raw "\n"
    let f =
            liftL2 $ \lang cont ->
                TeXEnv "minted" [FixArg lang] $ "\n" <> cont <> "\n"
    f (raw language) (raw $ T.unlines (map ("    " <>) (T.lines code)))
    raw "\n"

mintedInline :: Text -> Text -> Thesis
mintedInline language code = do
    packageDep_ "minted"
    comm2 "mintinline" (raw language) (raw code)

comm2 :: LaTeXC l => String -> l -> l -> l
comm2 name = liftL2 $ \l1 l2 -> TeXComm name [FixArg l1, FixArg l2]

item :: Thesis -> Thesis
item i = do
    HT.item Nothing
    i

cite :: Reference -> Thesis
cite = Thesis . LT.cite

nocite :: Reference -> Thesis
nocite = Thesis . LT.nocite

packageDep :: [LaTeX] -> Text -> Thesis
packageDep args = Thesis . LT.packageDep args

packageDep_ :: Text -> Thesis
packageDep_ = Thesis . LT.packageDep_

emptyBackground :: Thesis
emptyBackground = mintedTextInline "empty-background"

fullBackground :: Thesis
fullBackground = mintedTextInline "full-background"
