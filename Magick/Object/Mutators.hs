module Magick.Object.Mutators where

import Data.Set
import Control.Monad (liftM2)
import Magick.Color
import Magick.Mana
import Magick.Object
import Magick.Type
import Magick.Zone

{- TODO: Lenses? -}

type ObjMutator = Object -> Object

augmentPT :: Integer -> Integer -> ObjMutator
augmentPT p t obj = obj { power     = liftM2 (+) (power obj) (Just p)
                        , toughness = liftM2 (+) (toughness obj) (Just t) }

setPT :: Integer -> Integer -> ObjMutator
setPT p t obj = obj { power     = Just p
                    , toughness = Just t }

setMaybePT :: Maybe Integer -> Maybe Integer -> ObjMutator
setMaybePT mp mt obj = obj { power     = mp
                           , toughness = mt }

{- Concise operators for P/T mutation -}
p +/+ t = augmentPT p t
p -/- t = augmentPT (-p) (-t)
p +/- t = augmentPT p (-t)
p -/+ t = augmentPT (-t) (p)

(*/*) = setPT

switchPT :: ObjMutator
switchPT obj = setMaybePT power' toughness' obj
               where
                   power'     = power obj
                   toughness' = toughness obj

