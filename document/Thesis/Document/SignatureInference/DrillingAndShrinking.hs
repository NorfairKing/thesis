{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.DrillingAndShrinking
    ( thesisSignatureInferenceDrillingAndShrinking
    ) where

import DocImport

thesisSignatureInferenceDrillingAndShrinking :: Thesis
thesisSignatureInferenceDrillingAndShrinking =
    subsection "Drilling and Shrinking" $ do
        s "In the previous sections, a larger pattern emerges."
        s
            "The idea that the entire codebase is too large to consider even a substantial part of it, leads us to signature inference strategies that try to to reduce the scope to a smaller subset of relevant functions."
        l ["In this section we call this concept", dquoted "shrinking"]
        s
            "On the other hand, we find signature inference strategies that find more relevant equations (but fewer irrelevant equations) and spend less time as a result."
        l ["In this section we call this concept", dquoted "drilling"]
        s
            "Now we can combine these two ideas and make new signature inference strategies by composing drilling with shrinking."
        subsubsection "Shrinking" $ do
            s "A shrinking corresponds to a function as follows."
            hask "[Function] -> [Function] -> [Function]"
            s
                "Note that this corresponds exactly to the idea of a reducing signature inference strategy."
            todo "reference back to that section"
            s "We have already considered the following shrinkings."
            todo "continue here"
