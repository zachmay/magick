{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module Magick.Object where

import Data.Set (Set)
import qualified Data.Set as S

import Data.List (intercalate)
import Magick.Type
import Magick.Object.Stuff
import qualified Magick.Object.Facet as F
import Magick.Mana hiding (colors)
import qualified Magick.Mana as Mana (colors)
import Magick.Color

data Object = Object {
    oid :: ObjectID,
    facets :: [F.Facet],
    baseFacet :: F.Facet,
    name :: String,
    manaCost :: ManaCost,
    power :: Power,
    toughness :: Toughness,
    baseLoyalty :: BaseLoyalty,
    abilities :: [Ability],
    supertypes :: Set Supertype,
    types :: Set Type,
    creatureTypes :: Set CreatureType,
    artifactTypes :: Set ArtifactType,
    landTypes :: Set LandType,
    planeswalkerTypes :: Set PlaneswalkerType,
    spellTypes :: Set SpellType,
    enchantmentTypes :: Set EnchantmentType,
    isCard :: Bool
    {- tokens :: Map TokenType Integer -}
}

colors = Mana.colors . manaCost

convertedManaCost = convert . manaCost

{- TODO: Fix spacing where object has no sub/super/types. -}
instance Show Object where
    show obj = (name obj) ++ " <" ++ (show $ oid obj) ++ ">"
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
    a `compare` b = (oid a) `compare` (oid b)

fromFacets :: ObjectID -> [F.Facet] -> Object
fromFacets n fs@(f:rest) = Object {
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
    isCard = True
}


llanowarFacet = F.Facet {
    F.name = "Llanowar Elves",
    F.manaCost = ManaCost [Mana Green],
    F.power = 1,
    F.toughness = 1,
    F.baseLoyalty = 0,
    F.abilities = ["T: Add G to your mana pool."],
    F.supertypes = S.empty,
    F.types = S.singleton Creature,
    F.creatureTypes = S.fromList [Elf, Druid],
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

arborFacet = F.Facet {
    F.name = "Dryad Arbor",
    F.manaCost = ManaCost [],
    F.power = 1,
    F.toughness = 1,
    F.baseLoyalty = 0,
    F.abilities = [],
    F.supertypes = S.empty,
    F.types = S.fromList [Creature, Land],
    F.creatureTypes = S.fromList [Dryad],
    F.artifactTypes = S.empty,
    F.landTypes = S.fromList [Forest],
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

genjuFacet = F.Facet {
    F.name = "Genju of the Realm",
    F.manaCost = ManaCost [Mana White, Mana Blue, Mana Black, Mana Red, Mana Green],
    F.power = 0,
    F.toughness = 0,
    F.baseLoyalty = 0,
    F.abilities = ["Enchant land",
                   "{2}: Enchanted land becomes a legendary 8/12 Spirit creature with trample until end of turn. It's still a land.",
                   "When enchanted land is put into a graveyard, you may return Genju of the Realm from your graveyard to your hand."],
    F.supertypes = S.fromList [Legendary],
    F.types = S.fromList [Enchantment],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.fromList [Aura]
}

ajaniFacet = F.Facet {
    F.name = "Ajani Vengeant",
    F.manaCost = ManaCost [Colorless 2, Mana White, Mana Red],
    F.power = 0,
    F.toughness = 0,
    F.baseLoyalty = 3,
    F.abilities = ["+1: Target permanent doesn't untap during its controller's next untap step.",
                   "-2: Ajani Vengeant deals 3 damage to target creature or player.",
                   "-7: Destroy all lands target player controls."],
    F.supertypes = S.empty,
    F.types = S.fromList [Planeswalker],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.fromList [Ajani],
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

relicFacet = F.Facet {
    F.name = "Darksteel Relic",
    F.manaCost = ManaCost [Colorless 0],
    F.power = 0,
    F.toughness = 0,
    F.baseLoyalty = 0,
    F.abilities = ["Darksteel Relic is indestructible."],
    F.supertypes = S.empty,
    F.types = S.fromList [Artifact],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

llanowar = fromFacets 1 [llanowarFacet]
arbor = fromFacets 2 [arborFacet]
genju = fromFacets 3 [genjuFacet]
ajani = fromFacets 4 [ajaniFacet]
relic = fromFacets 5 [relicFacet]

tokenLlanowar = llanowar { isCard = False, oid = 6 }

world = S.fromList [llanowar, arbor, genju, ajani, relic, tokenLlanowar]

