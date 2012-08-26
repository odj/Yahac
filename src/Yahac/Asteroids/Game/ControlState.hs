module Yahac.Asteroids.Game.ControlState (
    ControlState(..),
    Direction(..),
    Thrust(..)
    )
    where

import Data.UUID as UUID

data Direction = TurnLeft | TurnRight | Center deriving (Show, Eq, Ord)

data Thrust = Forward | Backwards | NoThrust deriving (Show, Eq, Ord)

-- | Describes the Control State of a player
data ControlState = ControlState
    { turn          :: Direction  -- ^ The turning direction
    , thrust        :: Thrust     -- ^ The thrust state
    , thrustLevel   :: Double     -- ^ The thrust level for this player
    , rotationLevel :: Double     -- ^ The rotation level for this player 
    , fire          :: Bool       -- ^ Is firing
    , lastFire      :: Int        -- ^ The amount of time (ms) since last fired.  Used to control fire rate.
    , totalFired    :: Int        -- ^ The total number of bullets fired
    , controlUuid   :: UUID       -- ^ The UUID of the player this control belongs to.
    , score         :: Int        -- ^ The players score
    } deriving Show


