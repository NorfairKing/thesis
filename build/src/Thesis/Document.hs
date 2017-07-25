module Thesis.Document where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.Document.EntireDocument
import Thesis.Document.Types
import Thesis.LaTeXTarget
import Thesis.Presentation.EntirePresentation
import Thesis.Utils

thesisDraftRule :: String
thesisDraftRule = "draft"

thesisFinalRule :: String
thesisFinalRule = "final"

documentRules :: Rules ()
documentRules = do
    simpleRule "draft" BuildDraft entireDocument
    simpleRule "final" BuildFinal entireDocument
    simpleRule "presenter-presentation" BuildDraft entirePresentation
    simpleRule "public-presentation" BuildFinal entirePresentation

simpleRule :: String -> BuildKind -> Thesis -> Rules ()
simpleRule name build doc =
    rulesForDocumentWithName name build doc >>= (\df -> name ~> needP [df])

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
        liftIO $ buildLaTexTargetWithNameIn name absTmpDir bkind document
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
