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

boltFacet = F.Facet {
    F.name = "Lightning Bolt",
    F.manaCost = ManaCost [Mana Red],
    F.power = 0,
    F.toughness = 0,
    F.baseLoyalty = 0,
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
    F.power = 0,
    F.toughness = 0,
    F.baseLoyalty = 0,
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
    F.power = 5,
    F.toughness = 5,
    F.baseLoyalty = 0,
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

llanowar = (fromFacets 1 [llanowarFacet]) { zone = battlefield }
arbor    = (fromFacets 2 [arborFacet])    { zone = battlefield }
genju    = (fromFacets 3 [genjuFacet])    { zone = battlefield }
shivan   = (fromFacets 9 [shivanFacet])   { zone = battlefield }
ajani    = (fromFacets 4 [ajaniFacet])    { zone = library }
relic    = (fromFacets 5 [relicFacet])    { zone = library }
bolt     = (fromFacets 7 [boltFacet])     { zone = library }
wrath    = (fromFacets 8 [wrathFacet])    { zone = library }
shivan2  = (fromFacets 10 [shivanFacet])  { zone = library }

tokenLlanowar = llanowar { isCard = False, oid = 6, zone = battlefield }
stackWrath    = wrath { oid = 9, zone = stack }

world = S.fromList [llanowar, arbor, genju, ajani, relic, tokenLlanowar, bolt, wrath, stackWrath, shivan, shivan2]

data Foo = Bar { x :: Integer, y :: Integer }
         | Baz { a :: String, b :: String }
         deriving (Show, Eq, Ord)
