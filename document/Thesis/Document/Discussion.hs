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
        l ["A summary can be found in Figure", ref summaryLabel]
        let s_ = "S"
            m_ = "M"
            f_ = "F"
            i_ = "i"
            j_ = "j"
            dc_ = "DC"
            dc_' = "DC'"
        hereFigure $ do
            let fmt =
                    [ VerticalLine
                    , LeftColumn
                    , VerticalLine
                    , LeftColumn
                    , VerticalLine
                    , LeftColumn
                    , VerticalLine
                    ]
            let header = do
                    hline
                    "Strategy" & m dc_ & m dc_' -- Discovery Complexity"
                    lnbk
                    hline
                    hline
            tabular Nothing fmt $ do
                let syntDC = i_ ^: (2 * m_)
                    chunDC = f_ * i_ * 2 ^: (2 * m_)
                    chunPDC = f_ * (i_ ^: 2) * 3 ^: (2 * m_)
                header
                mapM_
                    (\(strat, compl, compl2) ->
                         strat & m (bigoh compl) & m (bigoh compl2) >> lnbk >>
                         hline)
                    [ (fullBackground, s_ ^: (2 * m_), s_ ^: 14)
                    , (emptyBackground, f_ ^: (2 * m_), 1)
                    , (syntacticSimilarityName, syntDC, 1)
                    , (syntacticSimilaritySymbols, syntDC, 1)
                    , (syntacticSimilarityType, syntDC, 1)
                    , (typeReachability, s_ ^: (2 * m_), s_ ^: 14)
                    , (chunks, s_ * f_ * 2 ^: (2 * m_), s_)
                    , (chunksPlus, (s_ ^: 2) * f_ * 3 ^: (2 * m_), s_ ^: 2)
                    , (chunksSimilarityName, chunDC, 1)
                    , (chunksSimilaritySymbols, chunDC, 1)
                    , (chunksSimilarityType, chunDC, 1)
                    , (chunksTypeReachability, s_ * f_ * 2 ^: (2 * m_), s_)
                    , (chunksPlusSimilarityName, chunPDC, 1)
                    , (chunksPlusSimilaritySymbols, chunPDC, 1)
                    , (chunksPlusSimilarityType, chunPDC, 1)
                    , ( chunksPlusTypeReachability
                      , (s_ ^: 2) * f_ * 3 ^: (2 * m_)
                      , s_ ^: 2)
                    , (chunksPlusReachabilityName, chunPDC, 1)
                    , (chunksPlusReachabilitySymbols, chunPDC, 1)
                    , (chunksPlusReachabilityType, chunPDC, 1)
                    ]
            caption "A summary of the different signature inference strategies"
            lab summaryLabel
        s "The symbols in this table are defined as follows."
        itemize $ do
            let sym symb_ expl_ = item $ m symb_ <> ": " <> expl_
            sym s_ "size of the scope"
            sym m_ "maximum size of the discovered properties"
            sym f_ "size of the focus"
            sym
                i_
                "chosen size of the signature in a reducing signature inference strategy"
            sym j_ "chosen depth of type reachability"
            sym dc_ "worst case discovery complexity"
            sym dc_' $ do
                sequence_
                    [ "worst case discovery complexity, when "
                    , m m_
                    , " is fixed to be "
                    , m 7 <> ", "
                    , m f_
                    , " is fixed to be "
                    , m 1 <> ", "
                    , m i_
                    , " is fixed to be "
                    , m 5
                    , " and "
                    , m j_
                    , " is fixed to be "
                    , m 7
                    , raw ";\n"
                    ]
                s "This is the configuration that we chose."
        subsection "Configurability" $ do
            s
                "The exploration of different signature inference strategies has had the nice side effect that signature inference can now be configured to use a chosen signature inference strategy."
            lnbk
            s
                "Indeed, different signature inference strategies make it possible to support multiple use cases."
            s
                "One potential user interface involves online property discovery and immediate feedback."
            s
                "This use case can be enabled by choosing a signature inference strategy that has an appropriate discovery complexity."
            lnbk
            s
                "Another potential user interface involves running property discovery overnight."
            s
                "In this case there is a larger number of feasible signature inference strategies available."
            lnbk
            s
                "Users are not forced to use any single signature inference strategy, and are invited to choose their own and evaluate whether it is appropriate for their use case."
        subsection "Shortcomings" $ do
            s
                "EasySpec suffers from most of the functional shortcomings that QuickSpec has."
            s
                "This includes false positives, which means that sometimes properties are discovered that do not hold."
            s
                "This is not a big problem since a human must still select the properties that they want to have hold, and the properties are still tested afterwards with different random input."
            lnbk
            s
                "Both QuickSpec and EasySpec can only discover properties that already hold (modulo false positives)."
            s
                "This means that properties that you may want to have hold about code will not be discovered if the code does not already satisfy those properties."
            lnbk
            s
                "Higher kinded type variables are not supported in EasySpec because their monomorphisation still has to be done manually in QuickSpec and QuickSpec has no dummy higher kinded variables as it does for other variables."
            lnbk
            l
                [ "Both QuickSpec and EasySpec use the"
                , haskInline "arbitrary"
                , "generators from the"
                , haskInline "Arbitrary"
                , "type class to generate random values"
                ]
            s
                "Custom generators could be of great value if certain properties only hold for a subset of a type, but neither QuickSpec nor EasySpec currently supports them."
            l
                [ "Furthermore, EasySpec currently does not find any instances that are in scope, so EasySpec will only operate on types of which QuickSpec already has the Arbitrary instance"
                , raw "built-in"
                ]
            lnbk
            s
                "Lastly, because EasySpec uses the interactive evaluator that built into GHC by interpolating Strings, there are many issues with respect to modules and unexported symbols."
            l
                [ "For example, EasySpec does not work well on modules that export functions of which the type contains unexported symbols, such as the function"
                , haskInline "error :: HasCallStack a => String -> a"
                , "wherein"
                , haskInline "HasCallStack"
                , "is not exported"
                ]
            lnbk
            s
                "EasySpec uses the GHC API to type check code, and translates the resulting types to a representation defined in an external library."
            s
                "This translation allowed for quicker iteration because the translated representation was easier to work with than internal representation in GHC, but the translation is not lossless."
            s "Therefore the translation incurs several limitations."
            s
                "It further complicates certain common practical situations such as modules and unexported symbols."
            s
                "Moreover, it also prevents us from using the type checking mechanisms within GHC to implement type reachability."
