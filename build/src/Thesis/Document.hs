{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.Document.Main
import Thesis.Utils

thesisBib :: Path Rel File
thesisBib = tmpDir </> $(mkRelFile "thesis.bib")

thesisTex :: Path Rel File
thesisTex = tmpDir </> $(mkRelFile "thesis.tex")

thesisPdf :: Path Rel File
thesisPdf = tmpDir </> $(mkRelFile "thesis.pdf")

thesisOut :: Path Rel File
thesisOut = outDir </> $(mkRelFile "thesis.pdf")

documentRules :: Rules ()
documentRules = do
    [thesisBib, thesisTex] $&%> do
        putLoud $
            unwords
                [ "Running thesis generator to make"
                , toFilePath thesisTex
                , "and"
                , toFilePath thesisBib
                ]
        liftIO (buildThesisDocumentIn tmpDir)
    thesisPdf $%> do
        needP [thesisBib, thesisTex]
        cmd
            (Cwd $ toFilePath tmpDir)
            (WithStdout True)
            (WithStderr True)
            (EchoStdout False)
            (EchoStderr False)
            [ "latexmk"
            , "-pdf"
            , "-shell-escape"
            , "-halt-on-error"
            , toFilePath $ filename thesisTex
            ]
    thesisOut `byCopying` thesisPdf
