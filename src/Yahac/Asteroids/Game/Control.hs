module Yahac.Asteroids.Game.Control (
    playerDefaultControls,
    defaultControlState,
    defaultControl,
    ControlFunction(..),
    ControlState(..),
    Direction(..),
    Thrust(..),
    MovementIO

    )
where

import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Bullet
import Yahac.Asteroids.Object.Burst
import Yahac.Asteroids.Internal.Point
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Data.Time
import Data.Maybe
import Data.UUID as UUID
import qualified Data.Map as M


-- | Convenience alias for controlling a player.
type MovementIO = 
        (Game, ControlState)    -- ^ The game and control state.
     -> Bool                    -- ^ Indicates if the state change is turning on (True) or off (False)
     -> IO (Game, ControlState) -- ^ The new state


data ControlFunction = ControlFunction
    { onUp     :: MovementIO
    , onDown   :: MovementIO
    , onLeft   :: MovementIO
    , onRight  :: MovementIO 
    , onFire   :: MovementIO
    , onUpdate :: Int -> MovementIO
    } 


-- | Describes the Control State of a player
data ControlState = ControlState
    { turn          :: Direction  -- ^ The turning direction
    , thrust        :: Thrust     -- ^ The thrust state
    , thrustLevel   :: Double     -- ^ The thrust level for this player
    , rotationLevel :: Double     -- ^ The rotation level for this player 
    , fire          :: Bool       -- ^ Is firing
    , lastFire      :: Int        -- ^ The amount of time (ms) since last fired.  Used to control fire rate.
    , controlUuid   :: UUID       -- ^ The UUID of the player this control belongs to.
    } deriving Show



data Direction = TurnLeft | TurnRight | Center deriving (Show, Eq, Ord)

data Thrust = Forward | Backwards | NoThrust deriving (Show, Eq, Ord)


-- | Creates a new default control state for player
playerDefaultControls uuid = (defaultControl, controlState) where
    never = UTCTime (ModifiedJulianDay 0) 0
    controlState = defaultControlState { controlUuid = uuid, lastFire = 0}
    

defaultControl = ControlFunction 
    { onUp = defaultOnUp
    , onDown = defaultOnDown
    , onLeft = defaultOnLeft
    , onRight = defaultOnRight
    , onFire = defaultOnFire
    , onUpdate = defaultOnUpdate
    } where 
        defaultOnUp c@(g, s) b = do
            let dir | b = Forward
                    | otherwise = NoThrust
                s' = s { thrust = dir}
            return (g, s')
        defaultOnDown c@(g, s) b  = do  
            let dir | b = Backwards
                    | otherwise = NoThrust
                s' = s { thrust = dir}
            return (g, s')
        defaultOnRight c@(g, s) b  = do  
            let dir | b = TurnRight
                    | otherwise = Center
                s' = s { turn = dir}
            return (g, s')
        defaultOnLeft c@(g, s) b  = do  
            let dir | b = TurnLeft
                    | otherwise = Center
                s' = s { turn = dir}
            return (g, s')
        defaultOnFire c@(g, s) b  = do  
            let s' = s {fire = b}
            return (g, s')
        defaultOnUpdate diff c@(g, s) b  = do  
            case toZipper g (controlUuid s) of
                Nothing -> do
                    return (g, s)
                Just z@(os, o) -> do
                    let a = thrustLevel s
                        r0 = rotationLevel s
                        f = fire s
                        p0 = position o'
                        h = rotation o
                        v | thrust s == Forward = (unitVector h) <*> a
                          | thrust s == Backwards = (unitVector h) <*> (-a)
                          | otherwise = (0.0, 0.0)
                    
                        r | turn s == TurnLeft = negate r0
                          | turn s == TurnRight = r0
                          | otherwise        = 0
                        o' = adjVelocity v 
                           $ adjAngularVelocity r o
                           
                    if f then do
                            bullet <- newBullet o'
                            {-g' <- addBurst o' <$> addChild bullet $ fromZipper(os, o')-}
                            g' <- return $ addChild bullet $ fromZipper(os, o')
                            return (g', s)
                         else return (fromZipper (os, o'), s)
        adjAngularVelocity v o = o {angularVelocity = v + angularVelocity o}
        adjVelocity v o = o {velocity = v <+> velocity o}

            
        

defaultControlState = ControlState 
    { turn = Center
    , thrust = NoThrust
    , thrustLevel = 0.005
    , rotationLevel = 0.05
    , fire = False
    , lastFire = 0
    , controlUuid = nil
    }




