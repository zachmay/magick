module Magick.Zone where

type ZoneID = Integer

data Visibility = Hidden | Public
    deriving (Eq, Show, Ord)

data ZoneType = Battlefield
              | Command
              | Exile
              | Graveyard
              | Hand
              | Library
              | Stack
              | Nowhere
              deriving (Eq, Ord, Show)

data Zone = Zone {
    zid :: ZoneID,
    name :: String,
    zoneType :: ZoneType,
    defaultVisibility :: Visibility
} deriving (Show, Eq, Ord)

mkZone zt vis zi zn = Zone {
    zid = zi,
    name = zn,
    zoneType = zt,
    defaultVisibility = vis
}

mkBattlefield = mkZone Battlefield Public
mkExile       = mkZone Exile       Public
mkGraveyard   = mkZone Graveyard   Public
mkHand        = mkZone Hand        Public
mkLibrary     = mkZone Library     Hidden
mkStack       = mkZone Stack       Public

nowhere = mkZone Nowhere Public (negate 1) "Nowhere"
