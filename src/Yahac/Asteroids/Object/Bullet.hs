module Yahac.Asteroids.Object.Bullet (
    newBullet
        
) where 

import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Burst
import Yahac.Asteroids.Internal.Point
import Yahac.Asteroids.Internal.Color

import qualified Data.Map as M
import Data.Time

type Bullet = Basic

defaultMuzzleVelocity = 1.0
defaultPoints = [(0.0, 0.0), (0.0, 1.0)]
defaultScale = (0.01, 0.01)
defaultMaxLifetime = 1000

onCollide' targets = f where 
    f diff self game = do
        
        let validHit = case (label $ head targets) of 
                Just "player" -> False
                Just "bullet" -> False
                otherwise -> True
            collide = do 
                putStrLn $ "Collision detected: " ++ (show self) ++ " collided with " ++ (show targets)
                game' <- addBurst self game
                return $ modifyGame game' $ self {deleted = True}

        case targets of 
            [] -> return game
            otherwise -> if validHit 
                         then collide
                         else return game
            


newBullet player = do
    let v0 = unitVector (rotation player) <*> defaultMuzzleVelocity 
                                          <+> velocity player
        p0 = position player
        r0 = rotation player
        m0 = 0.0
    bullet <- newBasicObjectWithLabel "bullet"
    return $ bullet 
        { velocity = v0
        , position = p0
        , rotation = r0
        , mass = m0
        , gravityResFun = (\_ -> True)
        , scaleValue = defaultScale
        , points = defaultPoints
        , lineColorFun = (\_ -> Color 1.0 0.0 0.0 Nothing)
        , maxLifetime = Just defaultMaxLifetime
        , canCollideFun = (\_ -> True)
        , onCollideFun = onCollide'
        , objectType = Bullet $ uuid player
        }



    


