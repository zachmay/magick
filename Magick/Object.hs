{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module Magick.Object where

import Data.Function (on)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Maybe (fromJust)

import Control.Lens
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

data Object = Object { _oid :: ObjectID
                     , _facets :: [F.Facet]
                     , _baseFacet :: F.Facet
                     , _name :: String
                     , _manaCost :: ManaCost
                     , _colors :: Set Color
                     , _power :: Maybe Power
                     , _toughness :: Maybe Toughness
                     , _baseLoyalty :: Maybe BaseLoyalty
                     , _abilities :: [Ability]
                     , _supertypes :: Set Supertype
                     , _types :: Set Type
                     , _creatureTypes :: Set CreatureType
                     , _artifactTypes :: Set ArtifactType
                     , _landTypes :: Set LandType
                     , _planeswalkerTypes :: Set PlaneswalkerType
                     , _spellTypes :: Set SpellType
                     , _enchantmentTypes :: Set EnchantmentType
                     , _isCard :: Bool
                     , _zone :: Z.Zone
                     , _controller :: Player
                     , _owner :: Player
    {- counters :: Map CounterType Integer -}
}

makeLenses ''Object

_convertedManaCost obj = convert . _manaCost $ obj

convertedManaCost :: Getter Object Integer
convertedManaCost = to (convert . _manaCost)

{- TODO: Fix spacing where object has no sub/super/types. -}
instance Show Object where
    show obj = "<" ++ (show $ _oid obj) ++ "> "
               ++ _name obj
               ++ " [" ++ (show $ _controller obj) ++ "] "
               ++ powerToughness
                 where powerToughness | _power obj == Nothing = ""
                                      | otherwise            = (show . fromJust $ _power obj)
                                                                ++ "/"
                                                                ++ (show . fromJust$ _toughness obj)
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
    a == b = _oid a == _oid b

instance Ord Object where
    compare = compare `on` _oid

fromFacets :: ObjectID -> Player -> [F.Facet] -> Object
fromFacets n player fs@(f:rest) = Object {
    _oid = n,
    _facets = fs,
    _baseFacet = f,
    _name = F.name f,
    _manaCost = F.manaCost f,
    _power = F.power f,
    _toughness = F.toughness f,
    _colors = Mana.colors $ F.manaCost f,
    _baseLoyalty = F.baseLoyalty f,
    _abilities = F.abilities f,
    _supertypes = F.supertypes f,
    _types = F.types f,
    _creatureTypes = F.creatureTypes f,
    _artifactTypes = F.artifactTypes f,
    _landTypes = F.landTypes f,
    _planeswalkerTypes = F.planeswalkerTypes f,
    _spellTypes = F.spellTypes f,
    _enchantmentTypes = F.enchantmentTypes f,
    _isCard = True,
    _zone = Z.nowhere,
    _owner = player,
    _controller = player
}
