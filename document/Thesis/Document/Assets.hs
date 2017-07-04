{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.Document.Assets
    ( Asset(..)
    , embedAsset
    , embedExternalAsset
    , makeAsset
    , withRegisteredAsset
    ) where

import DocImport

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.FileEmbed (bsToExp, makeRelativeToProject)
import qualified System.FilePath as FP

data Asset = Asset
    { assetPath :: FilePath -- Relative to the directory that 'assets' is in.
    , assetContents :: ByteString
    } deriving (Show, Eq)

embedAsset :: FilePath -> Q Exp
embedAsset assetFp = do
    let fp = "assets" FP.</> assetFp
    qAddDependentFile fp
    relFile <- makeRelativeToProject fp
    contents <- runIO (SB.readFile relFile)
    [|Asset {assetPath = fp, assetContents = contents}|]

embedExternalAsset :: Path Abs File -> FilePath -> Q Exp
embedExternalAsset p relPath = do
    let fp = toFilePath p
    qAddDependentFile fp
    contents <- runIO $ SB.readFile fp
    [|Asset {assetPath = relPath, assetContents = contents}|]

instance Lift ByteString where
    lift = bsToExp

makeAsset :: Path Abs Dir -> Asset -> IO (Path Abs File)
makeAsset rd Asset {..} = do
    dstPath <- resolveFile rd assetPath
    ensureDir $ parent dstPath
    exists <- doesFileExist dstPath
    let makeIt = SB.writeFile (toFilePath dstPath) assetContents
    if exists
        then do
            contents <- SB.readFile (toFilePath dstPath)
            unless (contents == assetContents) makeIt
        else makeIt
    pure dstPath

registerAsset :: Asset -> Thesis
registerAsset asset =
    t $
    registerAction (assetPath asset) $ \rootdir -> do
        rd <- resolveDir' rootdir
        void $ makeAsset rd asset

withRegisteredAsset :: Asset -> (FilePath -> Thesis) -> Thesis
withRegisteredAsset a func = do
    registerAsset a
    func $ assetPath a
