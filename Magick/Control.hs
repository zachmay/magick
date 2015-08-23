{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Magick.Control where

import Control.Monad.State
import Control.Lens
import Magick.State
import Magick.Object 
import Magick.Player (Player, mkPlayer)
import Magick.Object.Facet(Facet)

newtype Magick a = Magick {
    runM :: StateT MagickState IO a
} deriving (Monad, MonadIO, MonadState MagickState)

runMagick :: MagickState -> Magick a -> IO (a, MagickState)
runMagick st op = runStateT (runM op) st

tell :: String -> Magick ()
tell s = do
    liftIO . putStrLn $ s

tellIt :: (Show a) => a -> Magick ()
tellIt = tell . show

ask :: String -> Magick Integer
ask str = do
    liftIO . putStr $ str
    input <- liftIO getLine
    return $ read input

askAndTell = do
    n <- ask "What's your age? "
    uid .= n
    objects.traversed.oid += n
    state <- get
    liftIO . print $ state^..objects.traversed.name
    return (n + 1)

playerChoose :: (Show a) => Player -> [a] -> Magick a
playerChoose p xs = do
    forM_ choices (\(ix, choice) -> do
        tell $ (show ix) ++ ".\t" ++ (show choice))
    chosen <- ask $ (show p) ++ ", choose one: "
    return $ xs !! (fromInteger $ chosen - 1)
        where choices = zip [1..] xs

playerChooseMaybe :: (Show a) => Player -> [a] -> Magick (Maybe a)
playerChooseMaybe p xs = do
    tell "0.\tPass"
    forM_ choices (\(ix, choice) -> do
        tell $ (show ix) ++ ".\t" ++ (show choice))
    chosen <- ask $ (show p) ++ ", choose one: "
    case chosen of
        0         -> return Nothing
        otherwise -> return . Just $ xs !! (fromInteger $ chosen - 1)
        where choices = zip [1..] xs

createUID :: Magick Integer
createUID = do
    theID <- use uid
    uid .= theID + 1
    return theID

createObjectFromFacets :: Player -> [Facet] -> Magick Object
createObjectFromFacets player fs = do
    oid <- createUID
    return $ fromFacets oid player fs

createPlayer :: String -> Integer -> Magick Player
createPlayer name lifeTotal = do
    pid <- createUID
    return $ mkPlayer pid name lifeTotal
