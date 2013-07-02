module Magick.Effect where

import qualified Data.Set as S
import Magick.Object
import Magick.Object.Predicates
import Magick.Object.Mutators

data Effect = Effect Layer Timestamp ObjPredicate ObjMutator String

instance Eq Effect where
    (Effect l1 t1 _ _ _) == (Effect l2 t2 _ _ _) = l1 == l2 && t1 == t2

instance Ord Effect where
     (Effect l1 t1 _ _ _) `compare` (Effect l2 t2 _ _ _)
        | l1 == l2  = t1 `compare` t2
        | otherwise = l1 `compare` l2

instance Show Effect where
    show (Effect l t _ _ s) = "[" ++ show l ++ show t ++ "] <" ++ s ++ ">"

type Timestamp = Integer

data Layer = Copy
           | Control
           | Text
           | Types
           | Abilities
           | PTCharacteristic
           | PTSet
           | PTModify
           | PTCounters
           | PTSwitch
           deriving (Eq, Ord, Show)

allCreaturesPlus1Plus1   = Effect PTModify 5 (permanent &&? creature) (1 +/+ 1) "--"
redCreaturesMinus2Minus2 = Effect PTModify 6 (permanent &&? creature &&? red) (2 -/- 2) "--"
switchAllPT              = Effect PTSwitch 9 (permanent &&? creature) switchPT "--"

applyEffect :: Effect -> S.Set Object -> S.Set Object
applyEffect (Effect _ _ pred mut _) = S.map conditionalApply
    where conditionalApply obj = if pred obj then mut obj else obj
