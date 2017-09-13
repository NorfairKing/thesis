module Thesis.Document.Dependencies.TH
    ( makeDependencyAssets
    ) where

import Import

import Language.Haskell.TH

import EasySpec.Evaluate.Analyse.Common as EE
import EasySpec.Evaluate.Build as EE

import Thesis.Document.Assets

makeDependencyAsset :: (String, FilePath, IO (Path Abs File)) -> Q [Dec]
makeDependencyAsset trip = makeDependencyAssets [trip]

makeDependencyAssets :: [(String, FilePath, IO (Path Abs File))] -> Q [Dec]
makeDependencyAssets trips' = do
    trips <- runIO $ forM trips' $ \(n, p, genPath) -> (,,) n p <$> genPath
    runIO $ do
        bd <- EE.buildDir
        withCurrentDir bd $ do
            cd <- getCurrentDir
            print cd
            EE.runBuild $ map (toFilePath . (\(_, _, thd) -> thd)) trips
    fmap concat $ forM trips $ \(n, p, absp) -> makeAssetDec n p absp

makeAssetDec :: String -> FilePath -> Path Abs File -> Q [Dec]
makeAssetDec name relPath assetFile = do
    assetExp <- embedExternalAsset assetFile relPath
    let n = mkName name
    pure
        [ SigD n $ ConT $ mkName "Asset"
        , FunD n [Clause [] (NormalB assetExp) []]
        ]
