module Thesis.Document where

import Import

import Text.LaTeX.LambdaTeX.Selection.Types (Selection)

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

documentRules :: Selection -> Rules ()
documentRules sel = do
    simpleRule "draft" BuildDraft sel entireDocument
    simpleRule "final" BuildFinal sel entireDocument
    simpleRule "presenter-presentation" BuildDraft sel entirePresentation
    simpleRule "public-presentation" BuildFinal sel entirePresentation

simpleRule :: String -> BuildKind -> Selection -> Thesis -> Rules ()
simpleRule name build sel doc =
    rulesForDocumentWithName name build sel doc >>= (\df -> name ~> needP [df])

rulesForDocumentWithName ::
       String -> BuildKind -> Selection -> Thesis -> Rules (Path Abs File)
rulesForDocumentWithName name bkind sel document = do
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
        liftIO $ buildLaTexTargetWithNameIn name absTmpDir bkind sel document
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
