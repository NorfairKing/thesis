{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.DocTechDetails
    ( docTechDetails
    ) where

import DocImport

import qualified Data.Text as T

import Data.Time (getZonedTime)
import Data.Version (showVersion)
import Development.GitRev
import Language.Haskell.TH (runIO, stringE)
import Network.HostName (getHostName)
import System.Info

docTechDetails :: Thesis
docTechDetails = do
    comm1 "section*" "Circumstances"
    comm1 "subsection*" "Generator version"
    "The following information describes the version of the thesis generator that was used to compile this document."
    verbatim $
        T.pack $
        unlines
            [ "commit:             " ++ $(gitHash)
            , "commit date:        " ++ $(gitCommitDate)
            , "commit count:       " ++ $(gitCommitCount)
            , "branch:             " ++ $(gitBranch)
            , "dirty:              " ++ show $(gitDirty)
            ]
    raw "\n"
    comm1 "subsection*" "Compilation Host"
    "The thesis generator was compiled on a machine with the following characteristics."
    verbatim $
        T.pack $
        unlines
            [ "os:                 " ++ $(stringE os)
            , "arch:               " ++ $(stringE arch)
            , "hostname:           " ++ $(stringE =<< runIO getHostName)
            ]
    raw "\n"
    "Haskell Compiler info:"
    verbatim $
        T.pack $
        unlines
            [ "compiler name:      " ++ $(stringE compilerName)
            , "compiler version:   " ++ $(stringE $ showVersion compilerVersion)
            ]
    raw "\n"
    "Compiled on:"
    verbatim $ T.pack $ $(stringE =<< show <$> runIO getZonedTime)
    raw "\n"
    hostname <- liftIO getHostName
    comm1 "subsection*" "Generation Host"
    "The thesis generator was run on a machine with the following characteristics."
    verbatim $
        T.pack $
        unlines
            [ "os:                 " ++ os
            , "arch:               " ++ arch
            , "hostname:           " ++ hostname
            ]
    raw "\n"
    "Haskell Compiler info:"
    verbatim $
        T.pack $
        unlines
            [ "compiler name:      " ++ compilerName
            , "compiler version:   " ++ showVersion compilerVersion
            ]
    raw "\n"
    zonedTime <- liftIO getZonedTime
    comm0 "LaTeX" <> " code generated on:"
    verbatim $ T.pack $ show zonedTime
    raw "\n"
    packageDep ["yyyymmdd", "hhmmss"] "datetime"
    s [comm0 "LaTeX", "compiled on", comm0 "today", " at ", comm0 "currenttime"]
    newline
    s ["The", quoted "thesis", "project was started on", "2017-03-09"]
