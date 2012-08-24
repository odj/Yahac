module Yahac.Asteroids.Object.Path (
    newPathForUuid,
    )
where

import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Shard
import Data.UUID as UUID
import Data.UUID.V1

white = Color 0.4 0.4 0.4 Nothing

newPathForUuid uuid = do
    let points =  []
    basic <- newBasicObject
    return $ basic
        { points = points
        , lineColorFun = (\_ -> white)
        , label = Just "path"
        , scaleValue = (1.0, 1.0)
        , position = (0.0, 0.0)
        , fillColor = Nothing
        , isSegment = True
        , updatePositionFun = (\diff _ game -> return game)
        , updateGameFun = (\diff _ game  -> return game)
        , onCollideFun = (\diff _ -> (\_ game -> return game))
        , updateStateFun = updateStateFun' uuid
        }


maxPoints = 5000

checkPoints [] = []
checkPoints (p0:[]) = [p0]
checkPoints (p0:p1:ps)
    | (abs $ (fst p0) - (fst p1)) > 0.5 = []
    | (abs $ (snd p0) - (snd p1)) > 0.5 = []
    | otherwise = take maxPoints (p0:p1:ps)


updateStateFun' uuid diff self game = do
    case toZipper game uuid of
        Nothing -> return $ modifyGame game $ self {deleted = True}
        Just (os, o) -> do
            return $  modifyGame game $ self {points = checkPoints (position o : points self)}

