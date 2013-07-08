module Magick.Player where

import Data.Function (on)

data Player = Player { pid  :: Integer
                     , name :: String }

instance Eq Player where
    p1 == p2 = pid p1 == pid p2

instance Ord Player where
    compare = compare `on` pid
instance Show Player where
    show p = "<" ++ (show $ pid p) ++ "> " ++ name p


