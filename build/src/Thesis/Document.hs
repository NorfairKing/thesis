module Thesis.Document where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.Document.Main
import Thesis.Document.Types
import Thesis.Utils

thesisDraftRule :: String
thesisDraftRule = "draft"

thesisFinalRule :: String
thesisFinalRule = "final"

documentRules :: Rules ()
documentRules = do
    rulesForDocumentWithName "draft" BuildDraft >>=
        (\df -> thesisDraftRule ~> needP [df])
    rulesForDocumentWithName "final" BuildFinal >>=
        (\df -> thesisFinalRule ~> needP [df])

rulesForDocumentWithName :: String -> BuildKind -> Rules (Path Abs File)
rulesForDocumentWithName name bkind = do
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
        liftIO $ buildThesisDocumentWithNameIn name absTmpDir bkind
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
            , toFilePath $ filename texFile
            ]
    absOutDir <- liftIO $ makeAbsolute outDir
    outPdfFile <- liftIO $ resolveFile absOutDir $ name ++ ".pdf"
    outPdfFile `byCopying` tmpPdfFile
    pure outPdfFile
