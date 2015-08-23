{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Zone.Battlefield where

import Magick.Player (Player)
import Magick.Zone
import qualified Data.Set as S

data Battlefield o =
    Battlefield { _zid :: ZoneId
                , name :: String
                , owner :: Player
                , contents :: S.Set o }

battlefieldPut :: (Ord o) => Battlefield o -> o -> Battlefield o
battlefieldPut bf obj = bf { contents = contents' }
    where
        contents' = S.insert obj $ contents bf

battlefieldRemove :: (Ord o) => Battlefield o -> o -> Battlefield o
battlefieldRemove bf obj = bf { contents = contents' }
    where
        contents' = S.delete obj $ contents bf

instance UnorderedZone Battlefield o where
    put = battlefieldPut
    remove = battlefieldRemove

mkBF zoneId n pl objs = Battlefield { _zid = zoneId, name = n, owner = pl, contents = S.fromList objs }
