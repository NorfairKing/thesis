module Thesis.Document.Dependencies.TH
    ( makeDependencyAssets
    ) where

import Import

import Development.Shake

import Language.Haskell.TH

import EasySpec.Evaluate.Analyse.Common as EE
import EasySpec.Evaluate.Build as EE

import Thesis.Document.Assets

makeDependencyAssets :: [(String, FilePath, IO (Path Abs File))] -> Q [Dec]
makeDependencyAssets trips = do
    runIO $ do
        bd <- EE.buildDir
        withCurrentDir bd $ do
            cd <- getCurrentDir
            print cd
            cmd
                "stack"
                "build"
                ":easyspec-evaluate"
                "--exec"
                ["easyspec-evaluate build raw-data"] :: IO ()
            EE.runBuildEverything
    fmap concat $
        forM trips $ \(n, p, genPath) -> do
            absp <- runIO genPath
            makeAssetDec n p absp

makeAssetDec :: String -> FilePath -> Path Abs File -> Q [Dec]
makeAssetDec name relPath assetFile = do
    assetExp <- embedExternalAsset assetFile relPath
    let n = mkName name
    pure
        [ SigD n $ ConT $ mkName "Asset"
        , FunD n [Clause [] (NormalB assetExp) []]
        ]
