module Yahac.Asteroids.Object.Planet (
    newDefaultPlanet,
    orbitPlanet
        
) where 


import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Path
import Yahac.Asteroids.Internal.Point
import Yahac.Asteroids.Internal.Color
import Data.UUID as UUID
import Data.UUID.V1

import qualified Data.Map as M
import Data.Time

type Bullet = Basic

defaultLineColor = Color 1.0 1.0 0.0 Nothing
noCollideLineColor = Color 0.5 0.0 0.0 Nothing
defaultFillColor = Color 1.0 1.0 0.0 (Just 0.5)
defaultScale = (0.05, 0.05)
defaultPosition = (0.5, 0.5)
defaultMass = 0.002
spitCount = 4


onCollide' targets = f where 
    f diff self game = do
        let validHit (t:ts) = case (label t) of 
                Just "bullet" -> True
                otherwise -> validHit ts
            validHit [] = False
            collide = splitPlanet spitCount diff self game

        case targets of 
            [] -> return game
            otherwise -> if validHit targets && canCollide self
                         then collide 
                         else return game

newDefaultPlanet = do
    let circlePoints = map (\v -> (cos v, sin v)) [0.0, (2 * pi / 16) .. 2*pi]
    planet <- newBasicObjectWithLabel "planet"
    return $ planet 
        { position = defaultPosition
        , scaleValue = defaultScale 
        , mass = defaultMass
        , gravityGen = True
        , gravityResFun = (\s -> (age s) > 250)
        , points = circlePoints
        , lineColorFun = (\s -> case (canCollide s) of 
                True -> defaultLineColor
                False -> noCollideLineColor )
        , canCollideFun = (\s -> (age s) > 100)
        , onCollideFun = onCollide'
        }

halfSize p ang = do
    Just uuid <- nextUUID
    return $ p 
        { uuid = uuid
        , scaleValue = (scaleValue p) </> 2.0
        , velocity = (velocity p) <+> (unitVector ang <*> 0.1)
        , mass = (mass p) / 2.0
        , age = 0
        }

orbitPlanet = do
    planet <- newDefaultPlanet
    return $ planet
        { position = (0.5, 0.40)
        , scaleValue = defaultScale </> 2.0
        , mass = defaultMass / 2.0
        , velocity = (0.1, 0.0)
        , fillColor = Just defaultFillColor
        , label = Just "planet"
        }

splitPlanet :: Int -> ObjectUpdateIO
splitPlanet n diff self game = do
    newPlanets <- case n of 
        0 -> return []
        _ -> sequence $ map (halfSize self) $ tail [0.0,2 * pi / (fromIntegral n) .. 2 * pi]
    newPaths <- sequence $ map (newPathForUuid . uuid) newPlanets
    let selfDeleted = (\g -> modifyGame g $ self {deleted = True, canCollideFun = (\_ -> False)}) game
    let splitAdded =  foldr (addChild) selfDeleted (newPlanets ++ newPaths)
    
    return $ case (fst $ defaultScale) / (fst $ scaleValue self) > 4 of 
        True -> selfDeleted 
        False -> splitAdded
    
    


