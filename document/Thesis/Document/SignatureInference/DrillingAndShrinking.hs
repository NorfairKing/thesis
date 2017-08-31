{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.DrillingAndShrinking
    ( thesisSignatureInferenceDrillingAndShrinking
    ) where

import DocImport

import Thesis.Document.Sections

thesisSignatureInferenceDrillingAndShrinking :: Thesis
thesisSignatureInferenceDrillingAndShrinking =
    subsection "Composing strategies: Drilling and Shrinking" $ do
        s "In the previous sections, a larger pattern emerges."
        s
            "The idea that the entire codebase is too large to consider even a substantial part of it, leads us to signature inference strategies that try to to reduce the scope to a smaller subset of relevant functions."
        l ["In this section we call this concept", dquoted "shrinking"]
        s
            "On the other hand, we find signature inference strategies that find more relevant equations (but fewer irrelevant equations) and spend less time as a result."
        l ["In this section we call this concept", dquoted "drilling"]
        s
            "Now we can combine these two ideas and make new signature inference strategies by composing previous signature inference strategies."
        subsubsection "Shrinking" $ do
            s "A shrinking corresponds to a function as follows."
            hask "[Function] -> [Function] -> [Function]"
            l
                [ "Note that this corresponds exactly to the idea of a reducing signature inference strategy from section"
                , ref reducingSISSection
                ]
            s "We have already considered the following shrinkings."
            itemize $ do
                item emptyBackground
                item syntacticSimilarityName
                item syntacticSimilarityType
                item syntacticSimilaritySymbols
                item typeReachability
        subsubsection "Drilling" $ do
            s "A drilling corresponds to a function as follows."
            hask "[Function] -> [Function] -> InferM ()"
            s
                "The idea here is to find as many relevant equations as possible for the given focus."
            s "We have already considered the following drillings."
            itemize $ do
                item fullBackground
                item chunks
                item chunksPlus
        subsubsection "Composing two shrinkings" $ do
            s
                "Given two shrinkings, we can create a new shrinking by composition."
            haskL
                [ "composeShrinkings"
                , "  :: ([Function] -> [Function] -> [Function])"
                , "  -> ([Function] -> [Function] -> [Function])"
                , "  -> ([Function] -> [Function] -> [Function])"
                , "composeShrinkings s1 s2 focus = s2 focus . s1 focus"
                ]
            s
                "Note that this is only a useful idea in practice if the two shrinkings do not predetermine the size of the result."
            l
                [ "For example, composing"
                , syntacticSimilarityName
                , "with"
                , syntacticSimilarityType
                , "is not useful because the result will be equivalent to the result of"
                , syntacticSimilarityType
                , footnote $
                  l
                      [ "This is modulo some details involving the parameter"
                      , m "i"
                      , "for each shrinking"
                      ]
                ]
            l
                [ "However, composing, for example,"
                , typeReachability
                , "with"
                , syntacticSimilarityName
                , "could be useful"
                ]
        subsubsection "Composing two drillings" $ do
            s
                "Given two drillings, we can create a new drilling by composition."
            haskL
                [ "composeDrillings"
                , "  :: ([Function] -> [Function] -> InferM ())"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "composeDrillings d1 d2 focus scope = do"
                , "  d1 focus scope"
                , "  d2 focus scope"
                ]
            s
                "In theory composing two drillings could be useful, for example if the two drillings operate fundamentally differently and if they both do not take much time."
            s
                "In practice we only have very similar drillings, so we will not be looking at any compositions of drillings."
        subsubsection "Composing a shrinking with a drilling" $ do
            s
                "Given a shrinking and a drilling, we can compose them to make a new drilling as follows."
            haskL
                [ "composeShrinkingAndDrilling"
                , "  :: ([Function] -> [Function] -> [Function]"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "composeShrinkingAndDrilling s d focus = "
                , "  d focus . s focus"
                ]
            l
                [ "This composition opens up opportunities for new"
                , raw "trade-offs"
                ]
            s
                "For example, in such a composition, we could combine a shrinking that reduces a scope to a constant size with a drilling that does not concern itself with the size of the scope."
        subsubsection "Filling the gaps" $ do
            s
                "Consider all possible compositions of one shrinking and one drilling."
            l
                [ "Any combination of a shrinking and"
                , fullBackground
                , "as the drilling corresponds to that shrinking by itself"
                ]
            l
                [ "Any combination of"
                , emptyBackground
                , "with a drilling will not be much different in practice from just using"
                , emptyBackground
                , "by itself"
                ]
            s
                "Now let us consider the combinations that we have not discussed yet."
            l
                [ "We can combine each of the shrinkings with both"
                , chunks
                , "and"
                , chunksPlus
                , "as a drilling, to define the following new strategies"
                ]
            hereFigure $ do
                tabular Nothing [LeftColumn, LeftColumn] $ do
                    chunksSimilarityName & chunksPlusSimilarityName
                    lnbk
                    chunksSimilaritySymbols & chunksPlusSimilaritySymbols
                    lnbk
                    chunksSimilarityType & chunksPlusSimilarityType
                    lnbk
                    chunksTypeReachability & chunksPlusTypeReachability
                    lnbk
                caption "Missing signature inference strategies"
        subsubsection "Special compositions" $ do
            s
                "The last signature inference strategies that we considered were compositions of two shrinkings and a drilling."
            l
                [ "The first shrinking is"
                , typeReachability
                , "the second is one of the distance based shrinkings, and the drilling is"
                , chunksPlus
                ]
            l
                [ "At this point we did not consider similar combinations with"
                , chunks
                , "anymore, because the scope size would be constant after the distance based shrinking, which meant that the drilling was allowed to be slow"
                ]
            l
                [ "We called these strategies"
                , chunksPlusReachabilityName <> ","
                , chunksPlusReachabilitySymbols
                , "and"
                , chunksPlusReachabilityType
                ]
