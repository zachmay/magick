module Magick.Object.Facet where

import Data.Set (Set)

import Magick.Color
import Magick.Mana
import Magick.Object.Stuff
import Magick.Type

data Facet = Facet {
    name :: String,
    manaCost :: ManaCost,
    colors :: Set Color,
    power :: Maybe Power,
    toughness :: Maybe Toughness,
    baseLoyalty :: Maybe BaseLoyalty,
    abilities :: [Ability],
    supertypes :: Set Supertype,
    types :: Set Type,
    creatureTypes :: Set CreatureType,
    artifactTypes :: Set ArtifactType,
    landTypes :: Set LandType,
    planeswalkerTypes :: Set PlaneswalkerType,
    spellTypes :: Set SpellType,
    enchantmentTypes :: Set EnchantmentType
} deriving (Show)

