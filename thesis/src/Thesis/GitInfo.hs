{-# LANGUAGE TemplateHaskell #-}

module Thesis.GitInfo where

import Import

import Development.GitRev
import Development.Shake
import Development.Shake.Path

import Thesis.Utils

gitInfoRules :: Rules ()
gitInfoRules = do
    gitHashFile $%> liftIO (writeFile (toFilePath gitHashFile) $(gitHash))
    gitBranchFile $%> liftIO (writeFile (toFilePath gitBranchFile) $(gitBranch))
    gitCommitCountFile $%>
        liftIO (writeFile (toFilePath gitCommitCountFile) $(gitCommitCount))
    gitCommitDateFile $%>
        liftIO (writeFile (toFilePath gitCommitDateFile) $(gitCommitDate))

tmpGitDir :: Path Rel Dir
tmpGitDir = tmpDir </> $(mkRelDir "git")

gitHashFile :: Path Rel File
gitHashFile = tmpGitDir </> $(mkRelFile "hash")

gitBranchFile :: Path Rel File
gitBranchFile = tmpGitDir </> $(mkRelFile "branch")

gitCommitCountFile :: Path Rel File
gitCommitCountFile = tmpGitDir </> $(mkRelFile "commit-count")

gitCommitDateFile :: Path Rel File
gitCommitDateFile = tmpGitDir </> $(mkRelFile "commit-date")
