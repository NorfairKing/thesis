{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Acknowledgements
    ( thesisAcknowledgements
    ) where

import DocImport

thesisAcknowledgements :: Thesis
thesisAcknowledgements =
    declarePart "Acknowledgements" $ do
        raw "\n"
        center $ large $ textbf "Acknowledgements"
        raw "\n"
        s "I would first like to acknowledge Professor David Basin."
        s
            "He allowed me to work on a topic of my choosing and believed in me when I did."
        lnbk
        s "I would also like to thank my thesis adviser Dr Dmitriy Traytel."
        s
            "His door was always open whenever I ran into a trouble spot or had a question about my research or writing."
        s
            "He consistently allowed this work to be my own, but steered me in the right the direction whenever he thought I needed it."
        lnbk
        s
            "A big thank you to everyone who contributed to proofreading this work."
        s "Their extra eyes were most helpful."
        lnbk
        s
            "Finally, I must express my very profound gratitude to my parents and to my brother for providing me with unfailing support and continuous encouragement throughout my years of study and through the process of researching and writing this thesis."
        s "This accomplishment would not have been possible without them."
        lnbk
        raw
            "As such, I would like to dedicate this work to my father, and introduce this work with his summary of what it means to present a thesis:"
        center $
            raw
                "Ik heb ergens op gestudeerd en nu ga ik mijn vertellingske doen."
        s "Thank you."
        lnbk
        raw "Tom Sydney Kerckhove"
