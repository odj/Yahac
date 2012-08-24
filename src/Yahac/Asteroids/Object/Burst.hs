module Yahac.Asteroids.Object.Burst (
    addBurst,
    addBigBurst
) where

import Control.Applicative ((<$>))
import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Internal.Point
import Yahac.Asteroids.Object.Path

burstRot = [0.0, 2*pi/7.0 .. 2*pi]
bigBurstRot = [0.0, 2*pi/48.0 .. 2*pi]
burstPoints = [(0.0, 0.0), (0.5, 0.0)]
burstColor = Color 1.0 1.0 0.0 Nothing
burstScale = (0.01, 0.01)
burstLife = Just 250
bigBurstLife = Just 500
{-burstLife = Nothing-}
burstVelocity = 0.4
bigBurstVelocity = 0.1
bigBurstPathColor = Color 1.0 0.0 0.0 Nothing


onCollide' targets = f where 
    f diff self game = do
        
        let validHit = case (label $ head targets) of 
                Just "burst" -> False
                otherwise -> True
            collide = do 
                {-putStrLn $ "Collision detected: " ++ (show self) ++ " collided with " ++ (show targets)-}
                {-return $ modifyGame game $ self {deleted = True}-}
                return game

        case targets of 
            [] -> return game
            otherwise -> if validHit 
                         then collide
                         else return game
 

addBurst o g = do
    bursts <- mapM (\_ -> newBasicObjectWithLabel "burst") burstRot
    let r0 = rotation o
        v0 = velocity o
        g' = foldr (\(b, r) -> addChild $ b 
            { velocity = (cos $ r + r0, sin $ r + r0) <*> burstVelocity 
                                                      {-<+> v0-}
            , maxLifetime = burstLife
            , points = burstPoints 
            , lineColorFun = (\_ -> burstColor)
            , scaleValue = burstScale
            , rotation = r + r0
            , position = position o
            , gravityResFun = (\_ -> True)
            , canCollideFun = (\_ -> False)
            , onCollideFun = onCollide'
            }) g $ zip bursts burstRot
    return g'


addBigBurst o g = do
    let newColorPath = (\p -> p {lineColorFun = (\_ -> bigBurstPathColor)}) 
    bursts <- mapM (\_ -> newBasicObjectWithLabel "burst") bigBurstRot
    paths <- mapM (\b -> newColorPath <$> (newPathForUuid $ uuid b)) bursts
    let r0 = rotation o
        v0 = velocity o
        g' = foldr (\(b, r) -> addChild $ b 
            { velocity = (cos $ r + r0, sin $ r + r0) <*> bigBurstVelocity 
                                                      <+> v0
            , maxLifetime = bigBurstLife
            , points = burstPoints 
            , lineColorFun = (\_ -> burstColor)
            , scaleValue = burstScale
            , rotation = r + r0
            , position = position o
            , gravityResFun = (\_ -> True)
            , canCollideFun = (\_ -> False)
            , onCollideFun = onCollide'
            }) g $ zip bursts bigBurstRot
    return $ foldr addChild g' paths


    
