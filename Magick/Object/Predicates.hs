module Magick.Object.Predicates where

import Data.Set
import Magick.Color
import Magick.Mana
import Magick.Object
import Magick.Player (Player)
import Magick.Type
import Magick.Zone

type ObjPredicate = Object -> Bool

{- Predicate composition -}

infixr 3 &&?
infixr 2 ||?

p &&? q = \obj -> (p obj) && (q obj)
p ||? q = \obj -> (p obj) || (q obj)
non p obj = not (p obj)

{- Predicate-level comparison and equality -}

infix 4 <?
infix 4 >?
infix 4 <=?
infix 4 >=?
infix 4 ==?

(<?) :: (Ord a) => (Object -> a) -> a -> ObjPredicate
f <? n = \obj -> f obj < n

(>?) :: (Ord a) => (Object -> a) -> a -> ObjPredicate
f >? n = \obj -> f obj > n

(<=?) :: (Ord a) => (Object -> a) -> a -> ObjPredicate
f <=? n = \obj -> f obj <= n

(>=?) :: (Ord a) => (Object -> a) -> a -> ObjPredicate
f >=? n = \obj -> f obj >= n

(==?) :: (Eq a) => (Object -> a) -> a -> ObjPredicate
f ==? x = \obj -> f obj == x

{- Zone/Object Type predicates -}

permanent :: ObjPredicate
permanent obj = case zone obj of
                  Zone { zoneType = Battlefield } -> True
                  _                               -> False

spell :: ObjPredicate
spell obj = case zone obj of
              Zone { zoneType = Stack } -> True
              _                         -> False

card :: ObjPredicate
card obj = case zone obj of 
             Zone { zoneType = Battlefield } -> False
             Zone { zoneType = Stack }       -> False
             _                               -> True

{- This would count a triggered ability on the stack as a token -}
token :: ObjPredicate
token = not . isCard 

{- Simple type predicates -}

hasType :: Type -> ObjPredicate
hasType t obj = t `member` (types obj)

creature :: ObjPredicate
creature = hasType Creature

planeswalker :: ObjPredicate
planeswalker = hasType Planeswalker

artifact :: ObjPredicate
artifact = hasType Artifact

land :: ObjPredicate
land = hasType Land

enchantment :: ObjPredicate
enchantment = hasType Enchantment

instant :: ObjPredicate
instant = hasType Instant

sorcery :: ObjPredicate
sorcery = hasType Sorcery

{- Subtype predicates -}

creatureType :: CreatureType -> ObjPredicate
creatureType t obj = t `member` (creatureTypes obj)

artifactType :: ArtifactType -> ObjPredicate
artifactType t obj = t `member` (artifactTypes obj)

planeswalkerType :: PlaneswalkerType -> ObjPredicate
planeswalkerType t obj = t `member` (planeswalkerTypes obj)

landType :: LandType -> ObjPredicate
landType t obj = t `member` (landTypes obj)

spellType :: SpellType -> ObjPredicate
spellType t obj = t `member` (spellTypes obj)

enchantmentType :: EnchantmentType -> ObjPredicate
enchantmentType t obj = t `member` (enchantmentTypes obj)

{- Simple color predicates -}

hasColor :: Color -> ObjPredicate
hasColor c obj = c `member` (Magick.Object.colors obj)

white = hasColor White
blue = hasColor Blue
black = hasColor Black
red = hasColor Red
green = hasColor Green

{- Control predicates -}

controlledBy :: Player -> ObjPredicate
controlledBy p = \obj -> p == controller obj

controlledByAny :: Set Player -> ObjPredicate
controlledByAny players = \obj -> controller obj `member` players

{- Ownership predicates -}

ownedBy :: Player -> ObjPredicate
ownedBy p = \obj -> p == owner obj

ownedByAny :: Set Player -> ObjPredicate
ownedByAny players = \obj -> owner obj `member` players
