module Magick.Effect where

import qualified Data.Set as S
import Magick.Object
import Magick.Object.Predicates
import Magick.Object.Mutators

data Effect = Effect { layer :: Layer
                     , timestamp :: Timestamp
                     , selector :: ObjPredicate
                     , mutator :: ObjMutator
                     , text :: String }

instance Eq Effect where
    e1 == e2 = (layer e1      == layer e2) &&
               (timestamp e1  == timestamp e2) &&
               (text e1       == text e2)

instance Ord Effect where
    e1 `compare` e2 = (layer e1, timestamp e1, text e1) `compare`
                      (layer e2, timestamp e2, text e2)

instance Show Effect where
    show e = "[" ++
             (show $ layer e) ++ " " ++ (show $ timestamp e) ++
             "] <" ++ text e ++ ">"

type Timestamp = Integer

data Layer = Copy
           | Control
           | Text
           | CharacteristicTypes
           | Types
           | CharacteristicColors
           | Colors
           | Abilities
           | CharacteristicPT
           | PTSet
           | PTModify
           | PTCounters
           | PTSwitch
           deriving (Eq, Ord, Show)

allCreaturesPlus1Plus1 = Effect { layer = PTModify
                                , timestamp = 5
                                , selector = permanent &&? creature
                                , mutator = 1 +/+ 1
                                , text = "Creatures get +1/+1." }

redCreaturesMinus2Minus2 = Effect { layer = PTModify
                                  , timestamp = 6
                                  , selector = permanent &&? creature &&? red
                                  , mutator = (2 -/- 2)
                                  , text = "Red creatures get -2/-2." }

switchAllPT = Effect { layer = PTSwitch
                     , timestamp = 9
                     , selector = permanent &&? creature
                     , mutator = switchPT
                     , text = "Switch each creatures power and toughness." }

applyEffect :: Effect -> S.Set Object -> S.Set Object
applyEffect e = S.map conditionalApply
    where conditionalApply obj = if sel obj then mut obj else obj
          sel = selector e
          mut = mutator e

applyAll :: S.Set Object -> [Effect] -> S.Set Object
applyAll = foldl (flip applyEffect)
