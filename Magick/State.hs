{-# LANGUAGE TemplateHaskell #-}
module Magick.State where

import Control.Lens
import Magick.Object
import Magick.Player

data MagickState = MagickState
    { _uid         :: Integer
    , _players     :: [Player]
    , _playerOrder :: [Player]
    , _objects     :: [Object]
    }
    deriving (Show)

makeLenses ''MagickState

initialState = MagickState
    { _uid = 0
    , _players = []
    , _playerOrder = []
    , _objects = [] }
