module Thesis.ShakeBuild where

import Import

import Development.Shake

import Text.LaTeX.LambdaTeX.Selection.Types (Selection)

import Thesis.Document

thesisShakeBuildRules :: Selection -> Bool -> Rules ()
thesisShakeBuildRules = documentRules
