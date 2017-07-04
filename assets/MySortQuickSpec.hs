{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MySortQuickSpec where

import MySort
import QuickSpec
import Control.Monad

main :: IO ()
main =
    void $
    quickSpec
        signature
        { constants =
              [ constant "mySort" (mkDict mySort :: Dict (Ord A) -> [A] -> [A])
              , constant
                    "myIsSorted"
                    (mkDict myIsSorted :: Dict (Ord A) -> [A] -> Bool)
              ]
        }

mkDict ::
       (c =>
            a)
    -> Dict c
    -> a
mkDict x Dict = x
