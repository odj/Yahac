module Yahac.Asteroids.Object.Shard (
    newDefaulShard
        
) where 

import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Internal.Point
import qualified Data.Map as M
import Data.Time
import Unsafe.Coerce
import System.Random
import Control.Applicative hiding ((<*>))

type Bullet = Basic

defaultMaxVelocity = 0.5
defaultShardPoint = [(0.0, 0.0), (0.0, 1.0)]
defaultScale = (0.05, 0.05)
defaultMaxLifetime = 0.5
defaultNumShards = 10
randomAngle = getStdRandom $ randomR (0.0 :: Double, 1.0) 
randomScaleVal = getStdRandom $ randomR (0.5 :: Double, 1.0)
randomVal = getStdRandom $ randomR (0.0 :: Double, 1.0)
randomVector = do
    r1 <- randomVal
    r2 <- randomVal
    return (r1, r2)

randomScale = do
    r1 <- randomScaleVal
    r2 <- randomScaleVal
    return (r1, r2)
    

newDefaulShard  = do
    parent <- newBasicObjectWithLabel "shard"
    frags <- sequence $ map (\_ -> newFragment) [0 .. defaultNumShards]
    let shard = foldr addChild parent frags
    return $ shard 
        { scaleValue = defaultScale
        }

newFragment = do
    frag <- newBasicObject
    p0 <- randomVector
    r0 <- randomAngle
    v0 <- randomVector
    s0 <- randomVector
    return $ frag
        { position = p0
        , rotation = r0
        , velocity = v0 <*> defaultMaxVelocity
        , scaleValue = s0
        , points = defaultShardPoint
        , maxLifetime = Just 10
        }



    



