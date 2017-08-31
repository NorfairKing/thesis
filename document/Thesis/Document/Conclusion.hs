{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Conclusion
    ( thesisConclusion
    ) where

import DocImport

thesisConclusion :: Thesis
thesisConclusion =
    section "Conclusion" $ do
        subsection "Signature Inference" $ do
            s
                "QuickSpec has made great progress toward practical property discovery."
            s
                "It looked promising, until we had a look at the complexity of QuickSpec with respect to realistic codebases."
            s
                "Signature inference has proved to be a promising approach to making property discovery practical by taming the complexity of QuickSpec."
            s
                "The progress that we achieved on making property discovery practical for codebases of realistic sizes promises that property discovery could become a real and very valuable tool in software development."
        subsection "Further Work" $ do
            s
                "Significant engineering effort is required to make property discovery realistically usable."
            s
                "Most of the necessary research is done, but significant corners had to be cut with respect to practical usability."
            s
                "Nevertheless, there is still room for more research on this topic."
            lnbk
            s
                "The signature inference strategies discussed in this work only ever choose functions out of the scope and translate their types literally."
            s
                "These strategies never attempt to add anything to the signature that was not already in scope."
            s
                "It may be useful to consider adding entirely new functions or expressions to a signature."
            s
                "Literals are a good example of such expressions: it could be useful to add the zero constant to the signature, if the focus involves numbers."
            s
                "Similarly, it may be useful to add the empty list to the scope, if the focus involves lists, etc."
            lnbk
            s
                "Automatic monomorphisation of higher kinded type variables has been glossed over in this work, because it is a nontrivial concept to implement."
            s
                "Higher kinded type variables cannot simply be translated in the same way that simple type variables can be translated because QuickSpec currently has no support for such a translation."
            lnbk
            s
                "Type class instances should be discovered by the signature inference tool, but currently are not."
            s
                "This limitation has cut down the practical significance of EasySpec significantly, and requires engineering effort to lift."
            s
                "However, instance discovery could also aid signature inference in ways that we have not considered yet."
