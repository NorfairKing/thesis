{-# LANGUAGE TemplateHaskell #-}

module Thesis.Utils where

import Import

-- TODO make it also possible to do this with absolute paths.
(<.>)
    :: MonadThrow m
    => Path Rel File -> String -> m (Path Rel File)
(<.>) f e = parseRelFile $ toFilePath f ++ "." ++ e

tmpDir :: Path Rel Dir
tmpDir = $(mkRelDir "tmp")

outDir :: Path Rel Dir
outDir = $(mkRelDir "out")
