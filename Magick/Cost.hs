module Magick.Cost where

import Magick.Mana

data Cost = ManaCost [ManaSymbol]
          | TapSource

