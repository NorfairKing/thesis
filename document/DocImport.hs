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
    , citationNeeded'
    , headersAndFooters
    , hask
    , haskL
    , haskInline
    , mintedText
    , mintedTextL
    , mintedTextInline
    , minted
    , mintedInline
    , comm2
    , comm3
    , DocImport.item
    , DocImport.cite
    , DocImport.nocite
    , DocImport.packageDep
    , DocImport.packageDep_
    , emptyBackground
    , fullBackground
    , syntacticSimilarityName
    , syntacticSimilaritySymbols
    , syntacticSimilarityType
    , chunks
    , chunksPlus
    , slow
    , question
    , todo
    , hereFigure
    , lab
    , DocImport.ref
    , m
    , ma
    , pars
    , getBuildKind
    ) where

import Import as X

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.LaTeX as X
       hiding (abstract, article, article, cite, item, label, pageref,
               paragraph, ref, section, subsection, subsubsection, titlepage)
import Text.LaTeX.LambdaTeX as X
       hiding (Selector(..), cite, nocite, packageDep, packageDep_, ref)
import qualified Text.LaTeX.LambdaTeX as LT
       (cite, label, nocite, packageDep, packageDep_, ref)
import Text.LaTeX.Packages.AMSMath as X

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
    let f m_ = liftIO $ fail $ unlines [m_, T.unpack t_]
    when (T.null t_) $ f "Sentence cannot be empty."
    unless (isUpper $ T.head t_) $ f "Sentence must start with a capital."
    unless (T.last t_ == '.') $ f "Sentence must end in a full stop."
    spellCheck t_
    raw t_
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
    newpage
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
citationNeeded = do
    raw "[CITATION NEEDED]"
    todo "Make sure to find a citation here"

citationNeeded' :: Thesis -> Thesis
citationNeeded' t_ = do
    raw "[CITATION NEEDED]"
    todo $ do
        "Make sure to find a citation here, about"
        t_

headersAndFooters :: Thesis
headersAndFooters = do
    packageDep_ "fancyhdr"
    comm1 "pagestyle" "fancy"
    comm0 "fancyhf"
    bkind <- gets buildKind
    when (bkind == BuildDraft) $
        comm1 "cfoot" $
        s "This is an unfinished draft. Please do not distribute it."

hask :: Text -> Thesis
hask = minted "haskell"

haskL :: [Text] -> Thesis
haskL = hask . T.unlines

haskInline :: Text -> Thesis
haskInline = mintedInline "haskell"

mintedText :: Text -> Thesis
mintedText = minted "text"

mintedTextL :: [Text] -> Thesis
mintedTextL = mintedText . T.unlines

mintedTextInline :: Text -> Thesis
mintedTextInline = mintedInline "text"

minted :: Text -> Text -> Thesis
minted language code = do
    packageDep_ "minted"
    raw "\n"
    let f =
            liftL2 $ \lang cont ->
                TeXEnv
                    "minted"
                    [OptArg (TeXRaw "bgcolor=mintedbgcolor"), FixArg lang] $
                "\n" <> cont <> "\n"
    f (raw language) (raw $ T.unlines (map ("    " <>) (T.lines code)))
    raw "\n"

mintedInline :: Text -> Text -> Thesis
mintedInline language code = do
    packageDep_ "minted"
    let f =
            liftL2 $ \lang cont ->
                TeXComm
                    "mintinline"
                    [ OptArg (TeXRaw "bgcolor=mintedbgcolor")
                    , FixArg lang
                    , FixArg cont
                    ]
    f (raw language) (raw code)

comm2 :: LaTeXC l => String -> l -> l -> l
comm2 name = liftL2 $ \l1 l2 -> TeXComm name [FixArg l1, FixArg l2]

comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 name =
    liftL3 $ \l1 l2 l3 -> TeXComm name [FixArg l1, FixArg l2, FixArg l3]

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

syntacticSimilarityName :: Thesis
syntacticSimilarityName = mintedTextInline "syntactic-similarity-name"

syntacticSimilaritySymbols :: Thesis
syntacticSimilaritySymbols = mintedTextInline "syntactic-similarity-symbols"

syntacticSimilarityType :: Thesis
syntacticSimilarityType = mintedTextInline "syntactic-similarity-type"

chunks :: Thesis
chunks = mintedTextInline "chunks"

chunksPlus :: Thesis
chunksPlus = mintedTextInline "chunks-plus"

slow :: Thesis -> Thesis
slow func = do
    f <- gets fastBuild
    unless f func

question :: Thesis -> Thesis
question t_ = todo_ [TeXRaw "linecolor=blue"] $ "Question for Dmitriy: " <> t_

todo :: Thesis -> Thesis
todo = todo_ []

todo_ :: [LaTeX] -> Thesis -> Thesis
todo_ extraArgs t_ = do
    packageDep_ "todonotes"
    flip liftL t_ $ \rt_ ->
        TeXComm "todo" [MOptArg $ TeXRaw "inline" : extraArgs, FixArg rt_]

hereFigure :: Thesis -> Thesis
hereFigure n = do
    packageDep_ "float"
    fig n
  where
    fig =
        liftL $ \n_ ->
            TeXEnv "figure" [OptArg $ TeXRaw "H"] (comm0 "centering" <> n_)

lab :: Text -> Thesis
lab = t . LT.label

ref :: Text -> Thesis
ref = t . LT.ref

m :: Thesis -> Thesis
m = math . unSpellCheck

ma :: Thesis -> Thesis
ma = mathDisplay . unSpellCheck

pars :: LaTeXC l => l -> l
pars = autoParens

unSpellCheck :: Thesis -> Thesis
unSpellCheck func = do
    sb <- get
    put $ sb {spellChecker = Nothing}
    func
    put sb

getBuildKind :: Thesis' BuildKind
getBuildKind = gets buildKind
