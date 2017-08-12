{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Discussion where

import DocImport

thesisDiscussion :: Thesis
thesisDiscussion =
    section "Discussion" $
        -- Answer questions that people usually have
        -- What are the downsides
     do
        s
            "This work has explored several different signature inference strategies."
        let summaryLabel = "fig:summary"
        l ["A summary can be found in figure", ref summaryLabel]
        let s_ = "S"
            m_ = "M"
            f_ = "F"
            i_ = "i"
        hereFigure $ do
            let fmt =
                    [ VerticalLine
                    , LeftColumn
                    , VerticalLine
                    , LeftColumn
                    , VerticalLine
                    ]
            let header = do
                    hline
                    "Strategy" & "Time Complexity"
                    lnbk
                    hline
                    hline
            tabular Nothing fmt $ do
                header
                let o n = m $ raw "O\\left(" <> n <> raw "\\right)"
                mapM_
                    (\(strat, compl) -> strat & o compl >> lnbk >> hline)
                    [ (fullBackground, s_ ^: (2 * m_))
                    , (emptyBackground, f_)
                    , (syntacticSimilarityName, i_)
                    , (syntacticSimilaritySymbols, i_)
                    , (syntacticSimilarityType, i_)
                    , (chunks, s_)
                    , (chunksPlus, s_ ^: 2)
                    ]
            lab summaryLabel
            caption "A summary of the different signature inference strategies"
        s "The symbols in this table are defined as follows."
        itemize $ do
            let sym symb_ expl_ = item $ m symb_ <> ": " <> expl_
            sym s_ "size of the scope"
            sym m_ "maximum size of the discovered properties"
            sym f_ "size of the focus"
            sym
                i_
                "chosen size of the signature in a reducing signature inference strategy"
        subsection "Configurability" $ do
            s
                "The research into different signature inference strategies has had the nice side effect that signature inference can now be configured accordingly."
            lnbk
            s
                "Indeed, different signature inference strategies make it possible to support multiple use cases."
            s
                "One potential user interface involves online property discovery and immediate feedback."
            s
                "This use case can be enabled by choosing a signature inference strategy that runs in constant time and space, and would then work on almost any code base."
            lnbk
            s
                "Another potential user interface involves running property discovery over night."
            s
                "In this case there is a larger number of feasible signature inference strategies available."
            l
                [ "For best results, a strategy that runs in linear or logarithmic time can be chosen, such as"
                , chunks
                ]
            todo "what else?"
        subsection "Shortcomings" $ do
            s
                "EasySpec suffers from most of the functional shortcomings that QuickSpec has."
            s
                "This includes false positives, which means that some times properties are discovered that do not hold."
            s
                "This is not a big problem since a human must still select the properties that they want to have hold, and the properties are still tested afterwards."
            lnbk
            s
                "EasySpec can only discover properties that already hold (modulo false positives)."
            s
                "This means that properties that you may want to have hold about code will not be discovered if the code does not already satisfy those properties."
            todo "what else?"
        todo "what else?"
