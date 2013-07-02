module Magick.Object.Mutators where

import Data.Set
import Magick.Color
import Magick.Mana
import Magick.Object
import Magick.Type
import Magick.Zone

{- TODO: Lenses? -}

type ObjMutator = Object -> Object

augmentPT :: Integer -> Integer -> ObjMutator
augmentPT p t obj = obj { power = power obj + p, toughness = toughness obj + t }

p +/+ t = augmentPT p t
p -/- t = augmentPT (-p) (-t)
p +/- t = augmentPT p (-t)
p -/+ t = augmentPT (-t) (p)

switchPT :: ObjMutator
switchPT obj = obj { power = toughness', toughness = power' }
               where
                   power' = power obj
                   toughness' = toughness obj

