module Thesis.ShakeBuild where

import Development.Shake

import Text.LaTeX.LambdaTeX.Selection.Types (Selection)

import Thesis.Document

thesisShakeBuildRules :: Selection -> Rules ()
thesisShakeBuildRules = documentRules
