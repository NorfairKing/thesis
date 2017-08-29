{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.Document.Assets
    ( Asset(..)
    , embedAsset
    , embedExternalAsset
    , makeAsset
    , withRegisteredAsset
    , withDotAsset
    ) where

import DocImport

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.FileEmbed (bsToExp, makeRelativeToProject)
import Data.Hashable
import qualified System.FilePath as FP
import System.Process

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
    exists_ <- doesFileExist dstPath
    let dp = toFilePath dstPath
    let hp = hashPath dp
    let makeIt = do
            SB.writeFile dp assetContents
            writeFile hp (show $ hash assetContents)
    if exists_
        then do
            h <- read <$> readFile hp
            unless (h == hash assetContents) makeIt
        else makeIt
    pure dstPath
  where
    hashPath = (++ ".hash")

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

registerDotAsset :: Asset -> Thesis' FilePath
registerDotAsset asset = do
    let p = assetPath asset FP.-<.> "pdf"
    t $
        registerAction p $ \rootdir -> do
            rd <- resolveDir' rootdir
            fp <- makeAsset rd asset
            let fp' = toFilePath fp FP.-<.> "pdf"
            liftIO $
                print $ unwords ["dot", "-Tpdf", toFilePath fp, "-o" ++ fp']
            liftIO $ callProcess "dot" ["-Tpdf", toFilePath fp, "-o" ++ fp']
    pure p

withDotAsset :: Asset -> (FilePath -> Thesis) -> Thesis
withDotAsset a func = do
    fp' <- registerDotAsset a
    func fp'
