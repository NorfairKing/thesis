{-# LANGUAGE TemplateHaskell #-}

module Thesis.Utils where

import Import

import Development.Shake
import Development.Shake.Path

-- TODO make it also possible to do this with absolute paths.
(<.>) :: MonadThrow m => Path Rel File -> String -> m (Path Rel File)
(<.>) f e = parseRelFile $ toFilePath f ++ "." ++ e

tmpDir :: Path Rel Dir
tmpDir = $(mkRelDir "tmp")

outDir :: Path Rel Dir
outDir = $(mkRelDir "out")

byCopying :: Path r File -> Path s File -> Rules ()
byCopying to from =
    to $%> do
        needP [from]
        putLoud $ unwords ["Copying", toFilePath from, "to", toFilePath to]
        copyFile from to

hidden :: Path r File -> Bool
hidden f = ".swp" `isSuffixOf` toFilePath f
