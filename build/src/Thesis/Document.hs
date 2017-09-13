{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thesis.Document where

import Import

import GHC.Generics hiding (Selector)

import Text.LaTeX.LambdaTeX.Selection.Types
       (Selection, Selector(..))

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Path

import Thesis.Document.EntireDocument
import Thesis.Document.Types
import Thesis.LaTeXTarget
import qualified Thesis.Presentation.Public.EntirePresentation
       as Public
import Thesis.Utils

thesisDraftRule :: String
thesisDraftRule = "draft"

thesisFinalRule :: String
thesisFinalRule = "final"

documentRules :: Selection -> Bool -> Rules ()
documentRules sel f = do
    void $ addOracle $ \(SelectionQ ()) -> pure sel
    void $ addOracle $ \(FastQ ()) -> pure f
    simpleRule "draft" BuildDraft entireDocument
    simpleRule "final" BuildFinal entireDocument
    simpleRule
        "presenter-public-presentation"
        BuildDraft
        Public.entirePresentation
    simpleRule "public-presentation" BuildFinal Public.entirePresentation

simpleRule :: String -> BuildKind -> Thesis -> Rules ()
simpleRule name build doc =
    rulesForDocumentWithName name build doc >>= (\df -> name ~> needP [df])

newtype SelectionQ =
    SelectionQ ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

deriving instance Generic Selector

instance Hashable Selector

instance NFData Selector

instance Binary Selector

newtype FastQ =
    FastQ ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

rulesForDocumentWithName ::
       String -> BuildKind -> Thesis -> Rules (Path Abs File)
rulesForDocumentWithName name bkind document = do
    absTmpDir <- liftIO $ makeAbsolute tmpDir
    let tmpFile = liftIO . resolveFile absTmpDir
    texFile <- tmpFile $ name ++ ".tex"
    bibFile <- tmpFile $ name ++ ".bib"
    [texFile, bibFile] $&%> do
        putLoud $
            unwords
                [ "Running thesis generator to make"
                , toFilePath texFile
                , "and"
                , toFilePath bibFile
                ]
        s <- askOracle $ SelectionQ ()
        f <- askOracle $ FastQ ()
        liftIO $ buildLaTexTargetWithNameIn name absTmpDir bkind s f document
    tmpPdfFile <- tmpFile $ name ++ ".pdf"
    tmpPdfFile $%> do
        needP [texFile, bibFile]
        cmd
            (Cwd $ toFilePath absTmpDir)
            (WithStdout True)
            (WithStderr True)
            (EchoStdout False)
            (EchoStderr False)
            [ "latexmk"
            , "-pdf"
            , "-shell-escape"
            , "-halt-on-error"
            , "-interaction=nonstopmode"
            , toFilePath $ filename texFile
            ]
    absOutDir <- liftIO $ makeAbsolute outDir
    outPdfFile <- liftIO $ resolveFile absOutDir $ name ++ ".pdf"
    outPdfFile `byCopying` tmpPdfFile
    pure outPdfFile
