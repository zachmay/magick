{-# LANGUAGE TemplateHaskell #-}
module Magick.Player where

import Control.Lens
import Data.Function (on)

data Player = Player { _pid  :: Integer
                     , _name :: String
                     , _life :: Integer }

makeLenses ''Player

instance Eq Player where
    p1 == p2 = _pid p1 == _pid p2

instance Ord Player where
    compare = compare `on` _pid

instance Show Player where
    show p = "<" ++ (show $ _pid p) ++ "> " ++ _name p

good = Player { _pid = 1, _name = "Good", _life = 20}
evil = Player { _pid = 2, _name = "Evil", _life = 20 }

mkPlayer :: Integer -> String -> Integer -> Player
mkPlayer thePid theName theLife = Player { _pid = thePid
                                         , _name = theName
                                         , _life = theLife }
