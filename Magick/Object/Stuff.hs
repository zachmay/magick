module Magick.Object.Stuff where 

type Power = Integer
type Toughness = Integer
type BaseLoyalty = Integer
type CounterName = String
type ObjectID = Integer
type Ability = String
type TokenType = String

data Rarity = Mythic | Rare | Uncommon | Common
    deriving (Eq, Show)
