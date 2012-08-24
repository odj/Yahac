module Yahac.Asteroids.Object.Player (
    newPlayer,
    newPlayerWithLabel
    )
where

import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Object.Burst
import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Shard

white = Color 1.0 1.0 1.0 Nothing

newPlayer = do
    let points = 
            [ (0.0, 1.0) 
            , (-0.5, -0.5)
            , (0.5, -0.5)
            ]
    basic <- newBasicObject
    return $ basic
        { points = points
        , label = Just "Player"
        , scaleValue = (0.02, 0.02)
        , mass = 0.001
        , gravityResFun = (\_ -> True)
        , position = (0.5, 0.5)
        , fillColor = Just white
        , canCollideFun = (\_ -> True)
        , onCollideFun = onCollide'
        }


onCollide' targets = f where 
    f diff self game = do
        let validHit (t:ts) = case (label t) of 
                Just "planet" -> True
                otherwise -> validHit ts
            validHit [] = False
        let collide = do 
             game' <- addBigBurst self game
             return $ modifyGame game' $ self {deleted = True}
        case targets of 
            [] -> return game
            otherwise -> if validHit targets && canCollide self
                         then collide
                         else return game
 
 
    
newPlayerWithLabel label = do
    player <- newPlayer
    return $ player
        { label = Just label
        }



