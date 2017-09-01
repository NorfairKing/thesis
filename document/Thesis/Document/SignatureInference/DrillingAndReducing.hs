{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.SignatureInference.DrillingAndReducing
    ( thesisSignatureInferenceDrillingAndReducing
    ) where

import DocImport

import Thesis.Document.Sections

thesisSignatureInferenceDrillingAndReducing :: Thesis
thesisSignatureInferenceDrillingAndReducing =
    subsection "Composing strategies: Drilling and Reducing" $ do
        s "In the previous sections, a larger pattern emerges."
        s
            "The idea that the entire codebase is too large to consider even a substantial part of it, leads us to signature inference strategies that try to to reduce the scope to a smaller subset of relevant functions."
        s
            "On the other hand, we find signature inference strategies that find more relevant equations (but fewer irrelevant equations) and spend less time as a result."
        l ["In this section we call this concept", dquoted "drilling"]
        s
            "Now we can combine these two ideas and make new signature inference strategies by composing previous signature inference strategies."
        s "A reducing corresponds to a function as follows."
        hask "[Function] -> [Function] -> [Function]"
        s "We have already considered the following reducings."
        itemize $ do
            item emptyBackground
            item syntacticSimilarityName
            item syntacticSimilarityType
            item syntacticSimilaritySymbols
            item typeReachability
        s "A drilling corresponds to a function as follows."
        hask "[Function] -> [Function] -> InferM ()"
        s
            "The idea here is to find as many relevant equations as possible for the given focus."
        s "We have already considered the following drillings."
        itemize $ do
            item fullBackground
            item chunks
            item chunksPlus
        subsubsection "Composing two reducings" $ do
            s
                "Given two reducings, we can create a new reducing by composition."
            haskL
                [ "composeReducings"
                , "  :: ([Function] -> [Function] -> [Function])"
                , "  -> ([Function] -> [Function] -> [Function])"
                , "  -> ([Function] -> [Function] -> [Function])"
                , "composeReducings s1 s2 focus = s2 focus . s1 focus"
                ]
            s
                "Note that this is only a useful idea in practice if the two reducings do not both predetermine the size of the result."
            l
                [ "For example, composing"
                , syntacticSimilarityName
                , "with"
                , syntacticSimilarityType
                , "is not useful because the result will be equivalent to the result of"
                , syntacticSimilarityType <>
                  footnote
                      (l [ "This is modulo some details involving the parameter"
                         , m "i"
                         , "for each reducing"
                           ])
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
        subsubsection "Composing a reducing with a drilling" $ do
            s
                "Given a reducing and a drilling, we can compose them to make a new drilling as follows."
            haskL
                [ "composeReducingAndDrilling"
                , "  :: ([Function] -> [Function] -> [Function]"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "  -> ([Function] -> [Function] -> InferM ())"
                , "composeReducingAndDrilling s d focus = "
                , "  d focus . s focus"
                ]
            l
                [ "This composition opens up opportunities for new"
                , raw "trade-offs"
                ]
            s
                "For example, in such a composition, we could combine a reducing that reduces a scope to a constant size with a drilling that does not concern itself with the size of the scope."
        subsubsection "Filling the gaps" $ do
            s
                "Consider all possible compositions of one reducing and one drilling."
            l
                [ "Any combination of a reducing and"
                , fullBackground
                , "as the drilling corresponds to that reducing by itself"
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
                [ "We can combine each of the reducings with both"
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
                caption "Composed signature inference strategies"
        subsubsection "Special compositions" $ do
            s
                "The last signature inference strategies that we consider are compositions of two reducings and a drilling."
            l
                [ "The first reducing is"
                , typeReachability
                , "the second is one of the distance based reducings, and the drilling is"
                , chunksPlus
                ]
            l
                [ "At this point we did not consider similar combinations with"
                , chunks
                , "anymore, because the scope size would be constant after the distance based reducing, which meant that the drilling was allowed to be slow"
                ]
            l
                [ "We called these strategies"
                , chunksPlusReachabilityName <> ","
                , chunksPlusReachabilitySymbols
                , "and"
                , chunksPlusReachabilityType
                ]
