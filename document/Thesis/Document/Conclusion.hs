{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Conclusion
    ( thesisConclusion
    ) where

import DocImport

thesisConclusion :: Thesis
thesisConclusion =
    section "Conclusion" $ do
        subsection "Furter Work" $ pure ()
        subsection "Acknowledgements" $ pure ()
