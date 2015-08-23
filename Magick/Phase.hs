module Magick.Phase where

data Phase = Untap
           | Upkeep
           | Draw
           | Main1
           | BeginCombat
           | DeclareAttackers
           | DeclareBlockers
           | CombatDamage
           | EndCombat
           | Main
           | End
           | Cleanup
           deriving (Enum, Eq, Ord, Show)

phases = [Untap..Cleanup]
