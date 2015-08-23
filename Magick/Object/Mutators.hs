module Magick.Object.Mutators where

import Data.Set
import Control.Monad (liftM2)
import Control.Lens
import Magick.Color
import Magick.Mana
import Magick.Object
import Magick.Player
import Magick.Type
import Magick.Zone

{- TODO: Lenses? -}

type ObjMutator = Object -> Object

augmentPT :: Integer -> Integer -> ObjMutator
augmentPT p t obj = obj { _power     = liftM2 (+) (obj ^. power) (Just p)
                        , _toughness = liftM2 (+) (obj ^. toughness) (Just t) }

setPT :: Integer -> Integer -> ObjMutator
setPT p t obj = obj { _power     = Just p
                    , _toughness = Just t }

setMaybePT :: Maybe Integer -> Maybe Integer -> ObjMutator
setMaybePT mp mt obj = obj { _power     = mp
                           , _toughness = mt }

{- Concise operators for P/T mutation -}
p +/+ t = augmentPT p t
p -/- t = augmentPT (-p) (-t)
p +/- t = augmentPT p (-t)
p -/+ t = augmentPT (-t) (p)

(*/*) = setPT

switchPT :: ObjMutator
switchPT obj = setMaybePT power' toughness' obj
               where
                   power'     = obj ^. power
                   toughness' = obj ^. toughness

setController :: Player -> ObjMutator
setController p obj = obj { _controller = p }
