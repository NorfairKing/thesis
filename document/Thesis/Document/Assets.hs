{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.Document.Assets
    ( Asset(assetPath)
    , embedAsset
    , registerAsset
    ) where

import DocImport

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.FileEmbed (makeRelativeToProject, bsToExp)
import qualified System.Directory as D
import qualified System.FilePath as FP

data Asset = Asset
    { assetPath :: FilePath
    , assetContents :: ByteString
    } deriving (Show, Eq)

embedAsset :: FilePath -> Q Exp
embedAsset assetFp = do
    let fp = "assets" FP.</> assetFp
    qAddDependentFile fp
    relFile <- makeRelativeToProject fp
    contents <- runIO (SB.readFile relFile)
    [|Asset {assetPath = fp, assetContents = contents}|]

instance Lift ByteString where
    lift = bsToExp

registerAsset :: Asset -> Thesis
registerAsset Asset {..} =
    registerAction assetPath $ \rd -> do
        D.createDirectoryIfMissing True $ FP.takeDirectory $ rd FP.</> assetPath
        SB.writeFile (rd FP.</> assetPath) assetContents
