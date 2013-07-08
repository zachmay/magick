module Magick.Ability where

import Magick.Action
import Magick.Cost
import Magick.Effect
import Magick.Zone

data Ability = Activated [Zone] Cost Action String
             | Triggered [Zone] Condition String
             | Static [Zone] Effect String

data Condition = Condition


