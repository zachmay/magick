{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module Magical where

type Power = Int
type Toughness = Int
type BaseLoyalty = Int

type Ability = String

data Rarity = Mythic | Rare | Uncommon | Common
    deriving (Eq, Show)

data Supertype = Legendary | World | Snow
    deriving (Eq, Show)

data Type = Creature
          | Artifact
          | Planeswalker
          | Land
          | Enchantment
          | Instant
          | Sorcery
          deriving (Eq, Show)

type Subtype = String

data Color = White
           | Blue
           | Black
           | Red
           | Green
           deriving (Enum, Eq, Ord, Show)

data ManaSymbol = Mana Color
                | Colorless Int
                | VarColorless String
                | Hybrid ManaSymbol ManaSymbol
                | PhyrexianMana Color
                | SnowMana
                deriving (Eq)

instance Show ManaSymbol where
    show (Mana White)          = "W"
    show (Mana Blue)           = "U"
    show (Mana Black)          = "B"
    show (Mana Red)            = "R"
    show (Mana Green)          = "G"
    show (Colorless n)         = "{" ++ show n ++ "}"
    show (VarColorless s)      = "{" ++ s ++ "}"
    show (Hybrid a b)          = "{" ++ show a ++ "/" ++ show b ++ "}"
    show (PhyrexianMana White) = "{P/W}"
    show (PhyrexianMana Blue)  = "{P/U}"
    show (PhyrexianMana Black) = "{P/B}"
    show (PhyrexianMana Red)   = "{P/R}"
    show (PhyrexianMana Green) = "{P/G}"
    show SnowMana              = "S"
 
convertSymbol (Colorless n)    = n
convertSymbol (VarColorless _) = 0
convertSymbol (Hybrid a b)     = max (convertSymbol a) (convertSymbol b)
convertSymbol _                = 1

type ManaCost = [ManaSymbol]

instance Show ManaCost where
    show manaCost = concatMap show manaCost

convertManaCost = sum . (map convertSymbol)

data Card = Card {
    facets :: [CardFacet]
}

data CardFacet = CardFacet {
    name :: String,
    manaCost :: ManaCost,
    power :: Power,
    toughness :: Toughness,
    abilities :: [Ability],
    supertypes :: [Supertype],
    types :: [Type],
    subtypes :: [Subtype]
}


rr2 = [Colorless 2, Mana Red, Mana Red]
    
