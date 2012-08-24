module Yahac.Asteroids.Game (
    newGame,
    newGameSinglePlayer
) where 

import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Object.Player
import Yahac.Asteroids.Object.Path
import Yahac.Asteroids.Object.Planet
import Yahac.Asteroids.Object.Shard
import Yahac.Asteroids.Game.Control
import qualified Data.Map as M
import Control.Applicative

green = Color 0.0 1.0 0.0 Nothing

newGameSinglePlayer = do
    player <- newPlayerWithLabel "Player1"
    playerPath <- newPathForUuid (uuid player)
    centerPlanet <- newDefaultPlanet
    op1 <- orbitPlanet
    game <- addChild (player {position = (0.5, 0.75) }) 
            .   addChild centerPlanet
            .   addChild op1
            .   addChild (playerPath { lineColorFun = (\_ -> green) })
            <$> newEmptyWorld
    let (control, state) = playerDefaultControls $ uuid player
    putStrLn $ show state
    putStrLn $ show game
    return $ (game, state, control)


newGame = do
    centerPlanet <- newDefaultPlanet
    op1 <- orbitPlanet
    game <-     addChild centerPlanet
            .   addChild op1
            <$> newEmptyWorld
    putStrLn $ show game
    return game


newEmptyWorld = do
    let worldPoints = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]
    world <- newBasicObjectWithLabel "world"
    return $ world 
        { points = worldPoints
        , lineColorFun = (\_ -> Color 0.0 0.0 0.0 (Just 0.0))
        }





