import Magick.Object
import qualified Magick.Zone as Z
import qualified Magick.Object.Facet as F
import qualified Data.Set as S
import Magick.Type
import Magick.Mana
import Magick.Color
import Magick.Zone
import Magick.Object.Predicates
import Magick.Object.Mutators
import Magick.Effect
import qualified Magick.Player as P

llanowarFacet = F.Facet {
    F.name = "Llanowar Elves",
    F.manaCost = ManaCost [Mana Green],
    F.power = Just 1,
    F.toughness = Just 1,
    F.baseLoyalty = Nothing,
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
    F.power = Just 1,
    F.toughness = Just 1,
    F.baseLoyalty = Nothing,
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
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Nothing,
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
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Just 3,
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
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Nothing,
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

boltFacet = F.Facet {
    F.name = "Lightning Bolt",
    F.manaCost = ManaCost [Mana Red],
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Nothing,
    F.abilities = ["Lightning Bolt deals 3 damage to target creature or player."],
    F.supertypes = S.empty,
    F.types = S.fromList [Instant],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

wrathFacet = F.Facet {
    F.name = "Wrath of God",
    F.manaCost = ManaCost [Colorless 2, Mana White, Mana White],
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Nothing,
    F.abilities = ["Destroy all creatures. They can't be regenerated."],
    F.supertypes = S.empty,
    F.types = S.fromList [Sorcery],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

shivanFacet = F.Facet {
    F.name = "Shivan Dragon",
    F.manaCost = ManaCost [Colorless 4, Mana Red, Mana Red],
    F.power = Just 5,
    F.toughness = Just 5,
    F.baseLoyalty = Nothing,
    F.abilities = ["Flying", "{R}: Shivan Dragon gets +1/+0 until end of turn."],
    F.supertypes = S.empty,
    F.types = S.fromList [Creature],
    F.creatureTypes = S.fromList [Dragon],
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.empty,
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

battlefield = Z.mkBattlefield 1 "Battlefield"
library = Z.mkLibrary 2 "Library"
stack = Z.mkStack 3 "Stack"

good = P.Player { P.pid = 88, P.name = "Zach" }
evil = P.Player { P.pid = 99, P.name = "Evil" }

llanowar = (fromFacets 1 good [llanowarFacet]) { zone = battlefield }
arbor    = (fromFacets 2 good [arborFacet])    { zone = battlefield }
genju    = (fromFacets 3 good [genjuFacet])    { zone = battlefield }
shivan   = (fromFacets 9 good [shivanFacet])   { zone = battlefield }
ajani    = (fromFacets 4 good [ajaniFacet])    { zone = library }
relic    = (fromFacets 5 good [relicFacet])    { zone = library }
bolt     = (fromFacets 7 evil [boltFacet])     { zone = library }
wrath    = (fromFacets 8 evil [wrathFacet])    { zone = library }
shivan2  = (fromFacets 10 evil [shivanFacet])  { zone = battlefield }

shivan3 = shivan2 { oid = 11, controller = good }
tokenLlanowar = llanowar { isCard = False, oid = 6, zone = battlefield, controller = evil }
stackWrath    = wrath { oid = 9, zone = stack, controller = evil }

world = S.fromList [llanowar, arbor, genju, ajani, relic, tokenLlanowar, bolt, wrath, stackWrath, shivan, shivan2, shivan3]

data Foo = Bar { x :: Integer, y :: Integer }
         | Baz { a :: String, b :: String }
         deriving (Show, Eq, Ord)
