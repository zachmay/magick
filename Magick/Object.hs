{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module Magick.Object where

import Data.Function (on)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Maybe (fromJust)

import Control.Monad (liftM2)

import Magick.Color
import Magick.Cost
import Magick.Mana hiding (colors)
import Magick.Object.Stuff
import Magick.Player (Player)
import Magick.Type

import qualified Data.Set as S

import qualified Magick.Mana as Mana (colors)
import qualified Magick.Object.Facet as F
import qualified Magick.Zone as Z

data Object = Object { oid :: ObjectID
                     , facets :: [F.Facet]
                     , baseFacet :: F.Facet
                     , name :: String
                     , manaCost :: ManaCost
                     , power :: Maybe Power
                     , toughness :: Maybe Toughness
                     , baseLoyalty :: Maybe BaseLoyalty
                     , abilities :: [Ability]
                     , supertypes :: Set Supertype
                     , types :: Set Type
                     , creatureTypes :: Set CreatureType
                     , artifactTypes :: Set ArtifactType
                     , landTypes :: Set LandType
                     , planeswalkerTypes :: Set PlaneswalkerType
                     , spellTypes :: Set SpellType
                     , enchantmentTypes :: Set EnchantmentType
                     , isCard :: Bool
                     , zone :: Z.Zone
                     , controller :: Player
                     , owner :: Player
    {- counters :: Map CounterType Integer -}
}

colors = Mana.colors . manaCost

convertedManaCost = convert . manaCost

{- TODO: Fix spacing where object has no sub/super/types. -}
instance Show Object where
    show obj = "<" ++ (show $ oid obj) ++ "> "
               ++ name obj
               ++ " [" ++ (show $ controller obj) ++ "] "
               ++ powerToughness
                 where powerToughness | power obj == Nothing = ""
                                      | otherwise            = (show . fromJust $ power obj)
                                                                ++ "/"
                                                                ++ (show .fromJust  $ toughness obj)
    {-
    show a = (name a) ++ " " ++ (show $ manaCost a) ++ " <" ++ (show $ oid a) ++ ">\n" ++
             showSuperTypes ++ " " ++ showTypes ++ " - " ++ showSubtypes
             where
                showSuperTypes = intercalate " " . map show . S.toList $ supertypes a
                showTypes = intercalate " " . map show . S.toList $ types a
                showSubtypes = intercalate " " [
                    intercalate " " . map show . S.toList $ creatureTypes a,
                    intercalate " " . map show . S.toList $ artifactTypes a,
                    intercalate " " . map show . S.toList $ landTypes a,
                    intercalate " " . map show . S.toList $ planeswalkerTypes a,
                    intercalate " " . map show . S.toList $ spellTypes a,
                    intercalate " " . map show . S.toList $ enchantmentTypes a ]
    -}



instance Eq Object where
    a == b = oid a == oid b

instance Ord Object where
    compare = compare `on` oid

fromFacets :: ObjectID -> Player -> [F.Facet] -> Object
fromFacets n p fs@(f:rest) = Object {
    oid = n,
    facets = fs,
    baseFacet = f,
    name = F.name f,
    manaCost = F.manaCost f,
    power = F.power f,
    toughness = F.toughness f,
    baseLoyalty = F.baseLoyalty f,
    abilities = F.abilities f,
    supertypes = F.supertypes f,
    types = F.types f,
    creatureTypes = F.creatureTypes f,
    artifactTypes = F.artifactTypes f,
    landTypes = F.landTypes f,
    planeswalkerTypes = F.planeswalkerTypes f,
    spellTypes = F.spellTypes f,
    enchantmentTypes = F.enchantmentTypes f,
    isCard = True,
    zone = Z.nowhere,
    controller = p,
    owner = p
}

