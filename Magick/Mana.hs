{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module Magick.Mana where

import Magick.Color
import qualified Data.Set as S

data ManaSymbol = Mana Color
                | Colorless Integer
                | VarColorless String (Maybe Integer)
                | Hybrid ManaSymbol ManaSymbol
                | PhyrexianMana Color
                | SnowMana
                deriving (Eq, Ord)

instance Show ManaSymbol where
    show (Mana White)          = "{W}"
    show (Mana Blue)           = "{U}"
    show (Mana Black)          = "{B}"
    show (Mana Red)            = "{R}"
    show (Mana Green)          = "{G}"
    show (Colorless n)         = "{" ++ show n ++ "}"
    show (VarColorless s _)    = "{" ++ s ++ "}"
    show (Hybrid a b)          = "{" ++ show a ++ "/" ++ show b ++ "}"
    show (PhyrexianMana White) = "{P/W}"
    show (PhyrexianMana Blue)  = "{P/U}"
    show (PhyrexianMana Black) = "{P/B}"
    show (PhyrexianMana Red)   = "{P/R}"
    show (PhyrexianMana Green) = "{P/G}"
    show SnowMana              = "{S}"

newtype ManaCost = ManaCost [ManaSymbol]
    deriving (Eq, Ord)

instance Show ManaCost where
    show (ManaCost ms) = concatMap show ms

{- Compute converted mana costs -}
convert :: ManaCost -> Integer
convert (ManaCost ms) = sum . (map convertSymbol) $ ms
    where convertSymbol (Colorless n)              = n 
          convertSymbol (VarColorless _ Nothing)   = 0
          convertSymbol (VarColorless _ (Just n))  = n
          convertSymbol (Hybrid a b)               = max (convertSymbol a) (convertSymbol b)
          convertSymbol _                          = 1

{- Compute the set of colors present in a given mana cost -}
colors :: ManaCost -> S.Set Color
colors (ManaCost ms) = S.unions . map symbolColors $ ms
    where symbolColors (Mana c) = S.singleton c
          symbolColors (PhyrexianMana c) = S.singleton c
          symbolColors (Hybrid a b) = S.union (symbolColors a) (symbolColors b)
          symbolColors SnowMana = S.empty
          symbolColors (Colorless _) = S.empty
          symbolColors (VarColorless _ _) = S.empty

{- Some Tests -}
rr2 = ManaCost [Colorless 2, Mana Red, Mana Red]
wwu1 = ManaCost [Colorless 1, Mana White, Mana White, Mana Blue]
c4 = ManaCost [Colorless 4]
fireball = ManaCost [VarColorless "X" Nothing, Mana Red]
fireball5 = ManaCost [VarColorless "X" (Just 5), Mana Red]
bpod = ManaCost [Colorless 3, PhyrexianMana Green]
reaper = ManaCost [Hybrid (Colorless 2) (Mana White),
    Hybrid (Colorless 2) (Mana Blue),
    Hybrid (Colorless 2) (Mana Black),
    Hybrid (Colorless 2) (Mana Red),
    Hybrid (Colorless 2) (Mana Green) ]

