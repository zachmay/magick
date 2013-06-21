module Magick.Type where

data Supertype = Legendary | World | Snow
    deriving (Eq, Ord, Show)

data Type = Creature
          | Artifact
          | Planeswalker
          | Land
          | Enchantment
          | Instant
          | Sorcery
          deriving (Eq, Ord, Show)

data CreatureType = Advisor | Ally | Angel | Anteater | Antelope | Ape | Archer
                  | Archon | Artificer | Assassin | AssemblyWorker | Atog
                  | Aurochs | Avatar | Badger | Barbarian | Basilisk | Bat
                  | Bear | Beast | Beeble | Berserker | Bird | Blinkmoth | Boar
                  | Bringer | Brushwagg | Camarid | Camel | Caribou | Carrier
                  | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric
                  | Cockatrice | Construct | Coward | Crab | Crocodile
                  | Cyclops | Dauthi | Demon | Deserter | Devil | Djinn
                  | Dragon | Drake | Dreadnought | Drone | Druid | Dryad
                  | Dwarf | Efreet | Elder | Eldrazi | Elemental | Elephant
                  | Elf | Elk | Eye | Faerie | Ferret | Fish | Flagbearer | Fox
                  | Frog | Fungus | Gargoyle | Germ | Giant | Gnome | Goat
                  | Goblin | Golem | Gorgon | Graveborn | Gremlin | Griffin
                  | Hag | Harpy | Hellion | Hippo | Hippogriff | Homarid
                  | Homunculus | Horror | Horse | Hound | Human | Hydra | Hyena
                  | Illusion | Imp | Incarnation | Insect | Jellyfish
                  | Juggernaut | Kavu | Kirin | Kithkin | Knight | Kobold | Kor
                  | Kraken | Lammasu | Leech | Leviathan | Lhurgoyf | Licid
                  | Lizard | Manticore | Masticore | Mercenary | Merfolk
                  | Metathran | Minion | Minotaur | Monger | Mongoose | Monk
                  | Moonfolk | Mutant | Myr | Mystic | Nautilus | Nephilim
                  | Nightmare | Nightstalker | Ninja | Noggle | Nomad | Octopus
                  | Ogre | Ooze | Orb | Orc | Orgg | Ouphe | Ox | Oyster
                  | Pegasus | Pentavite | Pest | Phelddagrif | Phoenix | Pincher
                  | Pirate | Plant | Praetor | Prism | Rabbit | Rat | Rebel
                  | Reflection | Rhino | Rigger | Rogue | Salamander | Samurai
                  | Sand | Saproling | Satyr | Scarecrow | Scorpion | Scout
                  | Serf | Serpent | Shade | Shaman | Shapeshifter | Sheep
                  | Siren | Skeleton | Slith | Sliver | Slug | Snake | Soldier
                  | Soltari | Spawn | Specter | Spellshaper | Sphinx | Spider
                  | Spike | Spirit | Splinter | Sponge | Squid | Squirrel
                  | Starfish | Surrakar | Survivor | Tetravite | Thalakos
                  | Thopter | Thrull | Treefolk | Triskelavite | Troll | Turtle
                  | Unicorn | Vampire | Vedalken | Viashino | Volver | Wall
                  | Warrior | Weird | Werewolf | Whale | Wizard | Wolf
                  | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie
                  | Zubera
    deriving (Eq, Ord, Show)

data ArtifactType = Contraption | Equipment | Fortification
    deriving (Eq, Ord, Show)

data PlaneswalkerType = Ajani | Bolas | Chandra | Domri | Elspeth | Garruk
                      | Gideon | Jace | Karn | Koth | Liliana | Nissa | Ral
                      | Sarkhan | Sorin | Tamiyo | Tezzeret | Tibalt | Venser
                      | Vraska
    deriving (Eq, Ord, Show)

data LandType = Desert | Forest | Gate | Island | Lair | Locus | Mine
              | Mountain | Plains | PowerPlant | Swamp | Tower | Urzas
    deriving (Eq, Ord, Show)

data EnchantmentType = Aura | Curse | Shrine
    deriving (Eq, Ord, Show)

data SpellType = Arcane | Trap
    deriving (Eq, Ord, Show)


