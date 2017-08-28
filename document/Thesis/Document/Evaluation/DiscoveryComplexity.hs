{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Evaluation.DiscoveryComplexity
    ( thesisEvaluationDiscoveryComplexity
    ) where

import DocImport

thesisEvaluationDiscoveryComplexity :: Thesis
thesisEvaluationDiscoveryComplexity =
    subsection "Discovery Complexity" $ do
        s
            "The nature of signature inference strategies is that they perform some local computation interleaved with property discovery by QuickSpec."
        s
            "This means that traditional complexity analysis will give results that are not as good at modelling the runtime of different signature inference strategies than a domain specific complexity analysis could be."
        s
            "For this work, we conceived a type of complexity analysis that specifically applies to signature inference strategies."
        s
            "Its assumptions are that the local computation in the signature inference strategies are relatively cheap compared to the property discovery that QuickSpec performs."
        s
            "As long as signature inference strategies do not implement property discovery in their local computation, this is a reasonable assumption."
        todo "How do I show that this is reasonable?"
        s
            "For the purpose of the analysis, we will consider the local computation free, and we will focus on the complexity of the property discovery."
        s
            "The complexity of property discovery in QuickSpec is naturally upper bounded by the number of possible equations (including ill typed equations) that could be discovered."
        s
            "This means that we will compute the discovery complexity of a signature inference strategy as the sum of those numbers, over the executions of property discovery."
        subsubsection "Maxmimum Number of Discovered Equations" $ do
            let m_ = "M"
                t_ = "T"
                s_ = "S"
            l
                [ "The number of properties of maximum size"
                , m m_
                , "that can be discovered in a scope of"
                , m s_
                , "equations is related to the number of terms"
                , m t_
                , "that can be built using those functions"
                ]
            l
                [ "Because QuickSpec only discovers properties that consist of function applications and variables, we can compute the number of possible terms of maximum size"
                , m s_
                , "as follows"
                ]
            lnbk
            let v_ = "V"
                c_ n = "T" !: n
            l
                [ "If we assume a fixed number of possible variables"
                , m v_
                , "that could occur in a term, the number of possible terms of size one"
                , m $ c_ 1
                , "is"
                , m $ s_ + v_
                ]
            let f_ = "f"
                g_ = "g"
            l
                [ "For terms of size two, the number of possible terms"
                , m $ c_ 2
                , "is equal to"
                , m $ pars (s_ + v_) ^: 2
                , "because every such term is of the form"
                , m $ f_ <> g_
                , "where"
                , m f_
                , "and"
                , m g_
                , "are both terms of size one"
                ]
            let h_ = "h"
            s "For terms of size three, there are two options."
            l
                [ "A term of size three is either of the form"
                , m $ f_ <> pars (g_ <> h_)
                , "or of the form"
                , m $ pars (f_ <> g_) <> h_
                ]
            l
                [ "This means that there are"
                , m $ 2 * pars (s_ + v_) ^: 3
                , "different terms of size three"
                ]
            let n_ = "n"
            l
                [ "In general, the number of possible terms of size"
                , m n_
                , "is equal to the number of possible binary trees with"
                , m n_
                , "leaves times the number of possible combinations of contents of those leaves"
                , m $ pars (s_ + v_) ^: n_
                ]
            let cat_ n = "C" !: n
            l
                [ "The number of binary trees with"
                , m n_
                , "leaves, is well known to be the Catalan number"
                , m $ cat_ (n_ - 1)
                ]
            question "How do I cite this? I got it from Wikipedia"
            packageDep_ "amsmath"
            ma $
                c_ n_ =: cat_ (n_ - 1) * pars (s_ + v_) ^: n_ =: frac 1 n_ *
                comm2 "binom" (2 * n_ - 2) (n_ - 1) *
                pars (s_ + v_) ^: n_
            l
                [ "The number of terms of"
                , emph "maximum"
                , "size"
                , m n_
                , "is then a sum as follows"
                ]
            let i_ = "i"
            ma $
                sumFromTo (i_ =: 1) m_ <>
                (frac 1 n_ * comm2 "binom" (2 * i_ - 2) (i_ - 1) *
                 pars (s_ + v_) ^: i_)
            l
                [ "To arrive at the maximum number of discovered equations of size"
                , m m_
                , "from a scope of"
                , m s_
                , "functions, we take all possible tuples of terms of maximum size"
                , m m_
                ]
            ma $
                pars
                    (sumFromTo (i_ =: 1) m_ <>
                     (frac 1 n_ * comm2 "binom" (2 * i_ - 2) (i_ - 1) *
                      pars (s_ + v_) ^: i_)) ^:
                2
            s
                "Now we only need to choose the number of distinct variables we allow in an equation."
            s
                "Variables are scoped over both sides of the equation, but not across different equations."
            l
                [ "This means that more than"
                , m $ 2 * m_
                , "different variables will never be used in the same equation, but there could be a (rather useless) equation consisting only of"
                , m $ 2 * m_
                , "different variables"
                ]
            l
                [ "To conclude, the maximum number of equations of maximum size"
                , m m_
                , "that can be discovered using a scope of"
                , m s_
                , "functions can be computed as follows"
                ]
            ma $
                pars
                    (sumFromTo (i_ =: 1) m_ <>
                     (frac 1 n_ * comm2 "binom" (2 * i_ - 2) (i_ - 1) *
                      pars (s_ + 2 * m_) ^: i_)) ^:
                2
            l
                [ "In our experiments, we have fixed the maximum size of discovered equations to be"
                , m 7
                ]
            l
                [ "This naturally limits the maximum size of discovered equations in a scope of"
                , m s_
                , "functions to the following number"
                ]
            ma $
                pars
                    (sumFromTo (i_ =: 1) 7 <>
                     (frac 1 n_ * comm2 "binom" (2 * i_ - 2) (i_ - 1) *
                      pars (s_ + 14) ^: i_)) ^:
                2
            l
                [ "In general, this number is"
                , m $ bigoh $ s_ ^: (2 * m_) <> ","
                , "but because we were able to fix"
                , m m_ <> " to be"
                , m 7 <> ","
                , "this number is"
                , m $ bigoh $ s_ ^: 14
                , "for us"
                ]
        subsubsection "Example" $ do
            s
                "We will look at the discovery complexity of two different signature inference strategies in more detail here."
            l
                [ "The first is"
                , fullBackground
                , "and it is easy to analyse because it performs little local computation and only runs QuickSpec once"
                ]
            todo "refer back to the section where full background is defined"
            let s_ = "S"
            l
                [ "For a scope of size"
                , m s_ <> ","
                , fullBackground
                , "runs QuickSpec once with a scope size of"
                , m s_
                ]
            l
                [ "This means that the discovery complexity of"
                , fullBackground
                , "is"
                , m $ bigoh $ s_ ^: 14
                ]
            lnbk
            l ["Next, consider", m chunks]
            todo "refer back to the section where chunks is defined"
            s
                "This signature inference strategy performs some local computation to construct tuples of a scope function and a focus function, and runs QuickSpec as many times as there are such tuples."
            let f_ = "F"
            l
                [ "In a situation with"
                , m s_
                , "scope functions and"
                , m f_
                , "focus functions, there are"
                , m $ s_ * f_
                , "such tuples"
                ]
            l
                [ "This means that"
                , chunks
                , "runs QuickSpec a total of"
                , m $ s_ * f_
                , "times with a scope of constant size"
                , m 2
                ]
            l
                [ "The discovery complexity of"
                , chunks
                , "is therefore linear in the number of those tuples"
                ]
            ma $ bigoh (s_ * f_)
            s
                "This means that the discovery complexity is linear in the scope size if the focus size is constant (usually it is one)."
