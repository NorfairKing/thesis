module Thesis.ShakeBuild where

import Development.Shake

import Thesis.Document

thesisShakeBuildRules :: Rules ()
thesisShakeBuildRules =
    documentRules
