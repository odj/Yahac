{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Yahac.Asteroids.Object.Basic (
    Basic(..),
    Game(..),
    ObjectUpdateIO,
    defaultBasicObject,
    newBasicObject,
    newBasicObjectWithLabel,
    updatePosition,
    updateGame,
    updateState,
    canCollide,
    toZipper,
    fromZipper,
    modifyGame,
    addChild,
    getRenderPoints,
    lineColor
    
    )
where

import Yahac.Asteroids.Internal.Point
import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Internal.BoundingBox

import qualified Data.Map as M
import Data.UUID as UUID
import Data.UUID.V1
import Data.Time
import Data.Maybe
import Unsafe.Coerce
import Control.Monad
import Control.Applicative (pure)
import qualified Data.Map as M
import Graphics.Rendering.Cairo.Matrix



-- | This becomes a newtype later, but for now type alias prevents
-- cyclic imports
type Game = Basic 

-- | Alias to action that updates the game based on an object within the game.
--   Any game state changes are possible, however, the requirement for IO is 
--   mostly a consequence of requiring the system time for state calculations.
type ObjectUpdateIO = 
    Int        -- ^ The time in ms since the last update
 -> Basic      -- ^ The current object (or at least an object that contains the uuid of the current object)
 -> Basic      -- ^ The top-level game object 
 -> IO(Basic)  -- ^ Returns a new top level game object

-- | Holds all information for rendering and behavior of a object.  The top-level game object
--   is of this type.
data Basic = Basic 
    {
      position          :: {-# UNPACK #-} !Point             -- ^ The position in the current frame.
    , rotation          :: {-# UNPACK #-} !Double            -- ^ The rotation in radians.
    , scaleValue        :: {-# UNPACK #-} !Point             -- ^ The scale relative to the current frame.
    , points            :: ![Point]                          -- ^ A list of points that define the 2D vector to render.
    , lineColorFun      :: Basic -> Color                    -- ^ The line color to render.
    , fillColor         :: !(Maybe Color)                    -- ^ The fill color to render.  Nothing means no fill.
    , lineWidth         :: {-# UNPACK #-} !Double            -- ^ The absolute line width
    , isSegment         :: !Bool                             -- ^ The point list is a segmen, i.e. open path
    , children          :: !(M.Map UUID Basic)               -- ^ A map of all child objects.
    , uuid              :: {-# UNPACK #-} !UUID              -- ^ The current object's UUID.
    , label             :: !(Maybe String)                   -- ^ The current object's label.  
                                                             --   Possibly used to filter on arbitrary strings. 
                                                             --   Not just for 'Show'.

    , mass              :: {-# UNPACK #-} !Double            -- ^ The mass for gravity generators.
    , angularMoment     :: {-# UNPACK #-} !Double            -- ^ The angular mass.
    , velocity          :: {-# UNPACK #-} !Point             -- ^ The velocity.
    , angularVelocity   :: {-# UNPACK #-} !Double            -- ^ The angular velocity.
    , gravityGen        :: !Bool                             -- ^ True if object generates gravity.
    , gravityResFun     :: Basic -> Bool                     -- ^ True if object responds to gravity.
    , deleted           :: !Bool                             -- ^ Schedule object for deletion on the next pass through physics calculations.
    , isVisible         :: !Bool                             -- ^ True if object should be rendered.  If false, all physics still applies.
    , canCollideFun     :: Basic -> Bool
    , onCollideFun      :: [Basic] -> ObjectUpdateIO         -- ^ IO action to take on collision with another object.  The first 
                                                             --   argument is the collision target.

    , updatePositionFun :: ObjectUpdateIO                    -- ^ IO action to take when updating position.
    , updateStateFun    :: ObjectUpdateIO                    -- ^ IO action to take when updating state (i.e. performing physics calculations)
    , updateGameFun     :: ObjectUpdateIO                    -- ^ IO action to take when updating the game (this is where objects are added and removed).
    , age               :: Int                               -- ^ The total age of the object in ms
    , maxLifetime       :: !(Maybe Int)                      -- ^ The maximum lifetime of the object before it is deleted from the top-level map.
    } 


-- | A default object with sane update functions
defaultBasicObject = Basic
    {
      position = (0.0, 0.0)
    , rotation = 0.0
    , scaleValue = (1.0, 1.0)
    , points = []
    , isSegment = False
    , lineColorFun = (\_ -> white)
    , fillColor = Nothing
    , lineWidth = 0.001
    , children = M.empty
    , uuid = UUID.nil
    , label = Nothing
    , mass = 0.0
    , angularMoment = 0.0
    , velocity = (0.0, 0.0)
    , angularVelocity = 0.0
    , gravityGen = False
    , gravityResFun = (\_ -> False)
    , deleted = False
    , isVisible = True
    , canCollideFun = (\_ -> False)
    , onCollideFun = onCollide'
    , updatePositionFun = updatePosition'
    , updateStateFun = updateState'
    , updateGameFun = updateGame'
    , age = 0
    , maxLifetime = Nothing
    } where 
        runUpdate u g = M.fold (\ c g -> u c =<< g) (pure g) (children g)
        alwaysFalse _ _ = False
        updateState' diff self game = do
            let z = toZipper game (uuid self)
            -- NB - The point here is to use the most updated game object if one exists (it always should)
            let o = case z of 
                    Just (_, self') -> self'
                    Nothing -> self
            let v0 = velocity o
                onCollide = onCollideFun o
                colliders = getCollisions o game
                del = case maxLifetime o of
                    Nothing -> False
                    Just d -> (d < age o)
                netG | gravityRes o = netGravity o game
                     | otherwise = (0.0, 0.0)

            onCollide colliders diff o $  modifyGame game $ o
                     { deleted = del
                     , velocity = netG <*> (fromIntegral diff / 1000) <+> v0
                     }

        updatePosition' diff self game = do
            let dDiff = fromIntegral diff / 1000
            let s = self 
                    { position = normalize $ (position self) <+> ((velocity self) <*> dDiff)
                    , rotation = (rotation self) + (angularVelocity self) * dDiff
                    , age = diff + (age self)
                    }
            return $ modifyGame game s

        updateGame' diff self game = do
            let removed = M.filter (not . deleted) (children game)
            return $ game {children = removed}

        onCollide' targets = f where 
            f diff self game = do
            case targets of 
                [] -> return ()
                otherwise -> putStrLn $ "Collision detected: " ++ (show self) ++ " collided with " ++ (show targets)
            return game
            {-updateGame' self game-}

        white = Color 1.0 1.0 1.0 Nothing

        {-getCollisions self game = []-}
        getCollisions self game = collisions where
            id = uuid self
            colliders = M.fold (:) [] $ M.filter (\o -> canCollide o && uuid o /= id)$ children game
            bbSelf = toBoundingBox $ getRenderPoints self
            collisions = filter ((bbIntersects bbSelf) . toBoundingBox . getRenderPoints) colliders

{-# INLINE netGravity #-}
netGravity self game = foldr (\a acc -> g a <+> acc) (0.0, 0.0) generators where
    p0 = position self
    id0 = uuid self
    g basic | (dist2 p0 $ position basic) < 0.0001 = (0.0, 0.0) -- Avoid explosions
            | (uuid basic) /= id0 = (unit (position basic) p0) <*>  -- Don't count yourself!
                                    ((mass basic) / (dist2 p0 $ position basic))
            | otherwise           = (0.0, 0.0)
    generators = M.fold (:) [] $ M.filter gravityGen $ children game

    
instance Show Basic where
    show a = shown where
        id = case label a of
            Nothing -> show $ uuid a
            Just l  -> l
        ch = M.fold (\c acc -> (show c):acc) [] $ children a
        shown | length ch == 0 = id 
              | otherwise = id ++ " -- children: " ++ (show ch)


updatePosition diff self = updatePositionFun self diff self
updateState diff self = updateStateFun self diff self
updateGame diff self = updateGameFun self diff self self
canCollide self = canCollideFun self self
lineColor self = lineColorFun self self
gravityRes self = gravityResFun self self

-- | Creates a basic game object
newBasicObject = do
    Just uuid <- nextUUID
    now <- getCurrentTime
    return $ defaultBasicObject 
        { uuid = uuid
        }

-- | Creates a basic game object with label
newBasicObjectWithLabel l = do
    o <- newBasicObject
    return $ o { label = Just l }

-- | Finds an object anywhere within the game tree and
-- replaces it with the new one.  If the object is not found,
-- the original game state is returned unmodified (wihtout warning)
{-# INLINE modifyGame #-}
modifyGame :: Game -> Basic -> Game
modifyGame game basic = case toZipper game (uuid basic) of
    Nothing -> game
    Just z@(obs, _) -> fromZipper (obs, basic)



type Zipper = ([Basic] , Basic)

-- | Creates a zipper for the requested object within the game, if it
-- can be found.
{-# INLINE getZipper #-}
getZipper :: UUID -> Zipper -> Maybe Zipper
getZipper uuid z = zipper where
    nextZipper z o = ((snd z):(fst z), o)
    ch = map snd $ M.toList $ children $ snd z
    zs = catMaybes $ map ((getZipper uuid) . (nextZipper z)) ch
    z' | length zs == 1 = Just $ head zs
       | otherwise      = Nothing
    zipper = case M.lookup uuid (children $ snd z) of
        Just t  -> Just $ nextZipper z t
        Nothing -> z'

{-# INLINE toZipper #-}
toZipper game targetUuid = getZipper targetUuid ([], game)


-- | Reconstructs the game from the zipper
{-# INLINE fromZipper #-}
fromZipper :: Zipper -> Game
fromZipper (([]), o) = o
fromZipper ((x:[]), o) = addChild o x
fromZipper ((x:xs), o) = fromZipper (xs, addChild o x)


-- | Adds a new child object to the game
{-# INLINE addChild #-}
addChild child game = game { children = children'} where
    children' = M.insert (uuid child) child $ children game


-- | Handles screen wrapping
{-# INLINE normalize #-}
normalize (x1, y1) = (nv x1, nv y1) where
    nv v = v - fromIntegral (floor v)


-- | Gets points in the final render space.  This is used for calculating globally relevant bounding boxes.
{-# INLINE getRenderPoints #-}
getRenderPoints object@Basic {points=ps, scaleValue=(sx, sy), position=(px, py), rotation=r0} = 
   map (transformPoint (translate px py $ scale sx sy $ rotate r0 identity)) ps
    
