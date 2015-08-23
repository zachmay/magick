import Magick.Cards.Example
import Magick.State
import Magick.Control
import Magick.Object.Predicates
import Magick.Object.Mutators
import Control.Lens
import Control.Monad.State
import Control.Monad
import Magick.Object (name)
import Magick.Player (Player)

doStuff = runMagick initialState $ do
    good <- createPlayer "Good" 20
    evil <- createPlayer "Evil" 20
    other <- createPlayer "Other Guy" 20
    shivan <- createObjectFromFacets good shivanDragon
    garruk <- createObjectFromFacets evil garrukRelentless
    players .= [good, evil, other]
    playerOrder .= [good, evil, other]
    objects .= [shivan, garruk]
    po <- use playerOrder
    priorityCycle po

priorityCycle :: [Player] -> Magick ()
priorityCycle [] = do
    resolveTop
    po <- use playerOrder 
    priorityCycle po
priorityCycle (p:ps) = do
    objs <- use objects
    result <- playerChooseMaybe p objs
    case result of
        Nothing -> priorityCycle ps
        obj -> do
            tell "Putting on stack: "
            tellIt obj
            po <- use playerOrder
            priorityCycle po

resolveTop :: Magick ()
resolveTop = do
    tell "***"
    tell "*** Resolving top of stack."
    tell "***"
    return ()

