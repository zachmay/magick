module Magick.Cards.Example
    ( ajaniVengeant
    , darksteelRelic
    , dryadArbor
    , garrukRelentless
    , genjuOfTheRealms
    , lightningBolt
    , llanowarElves
    , shivanDragon
    , wrathOfGod ) where

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
    F.colors = S.fromList [Green],
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

llanowarElves = [llanowarFacet]

arborFacet = F.Facet {
    F.name = "Dryad Arbor",
    F.manaCost = ManaCost [],
    F.colors = S.fromList [Green],
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

dryadArbor = [arborFacet]

genjuFacet = F.Facet {
    F.name = "Genju of the Realm",
    F.manaCost = ManaCost [Mana White, Mana Blue, Mana Black, Mana Red, Mana Green],
    F.colors = S.fromList [White, Blue, Black, Red, Green],
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

genjuOfTheRealms = [genjuFacet]

ajaniFacet = F.Facet {
    F.name = "Ajani Vengeant",
    F.manaCost = ManaCost [Colorless 2, Mana White, Mana Red],
    F.colors = S.fromList [White, Red],
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

ajaniVengeant = [ajaniFacet]

relicFacet = F.Facet {
    F.name = "Darksteel Relic",
    F.manaCost = ManaCost [Colorless 0],
    F.colors = S.empty,
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

darksteelRelic = [relicFacet]

boltFacet = F.Facet {
    F.name = "Lightning Bolt",
    F.manaCost = ManaCost [Mana Red],
    F.colors = S.fromList [Red],
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

lightningBolt = [boltFacet]

wrathFacet = F.Facet {
    F.name = "Wrath of God",
    F.manaCost = ManaCost [Colorless 2, Mana White, Mana White],
    F.colors = S.fromList [White],
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

wrathOfGod = [wrathFacet]

shivanFacet = F.Facet {
    F.name = "Shivan Dragon",
    F.manaCost = ManaCost [Colorless 4, Mana Red, Mana Red],
    F.colors = S.fromList [Red],
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

shivanDragon = [shivanFacet]

garrukFront = F.Facet {
    F.name = "Garruk Relentless",
    F.manaCost = ManaCost [Colorless 3, Mana Green],
    F.colors = S.fromList [Green],
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Just 3,
    F.abilities = [
        "When Garruk Relentless has two or fewer loyalty counters on him, transform him.",
        "0: Garruk Relentless deals 3 damage to target creature. That creature deals damage equal to its power to him.",
        "0: Put a 2/2 green Wolf creature token onto the battlefield."
    ],
    F.supertypes = S.empty,
    F.types = S.fromList [Planeswalker],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.fromList [Garruk],
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

garrukBack = F.Facet {
    F.name = "Garruk, the Veil-Cursed",
    F.manaCost = ManaCost [],
    F.colors = S.fromList [Black, Green],
    F.power = Nothing,
    F.toughness = Nothing,
    F.baseLoyalty = Nothing,
    F.abilities = [
        "+1: Put a 1/1 black Wolf creature token with deathtouch onto the battlefield.",
        "-1: Sacrifice a creature. If you do, search your library for a creature card, reveal it, put it into your hand, then shuffle your library.",
        "-3: Creatures you control gain trample and get +X/+X until end of turn, where X is the number of creature cards in your graveyard."
    ],
    F.supertypes = S.empty,
    F.types = S.fromList [Planeswalker],
    F.creatureTypes = S.empty,
    F.artifactTypes = S.empty,
    F.landTypes = S.empty,
    F.planeswalkerTypes = S.fromList [Garruk],
    F.spellTypes = S.empty,
    F.enchantmentTypes = S.empty
}

garrukRelentless = [garrukFront, garrukBack]
