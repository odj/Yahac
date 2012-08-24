{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}


module Yahac.Asteroids.Object.Render.Cairo (
    RenderCairo(..)
    ) 
where


import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as MA
import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Internal.Color
import Yahac.Asteroids.Internal.BoundingBox

import Data.Map as M

class RenderCairo a where
    renderCairo :: a -> Render ()

instance RenderCairo Basic where
    renderCairo a = do
        
        -- NB This is a good place to print ad hoc information
        {-liftIO $ case label a of-}
            {-Just "Player1" -> putStrLn $ show $ toBoundingBox $ getRenderPoints a-}
            {-otherwise -> return ()-}

        let Color r g b al =  lineColor a
            w = lineWidth a
            s = scaleValue a
            BoundingBox b0 b1 = toBoundingBox $ points a

        case al of
            Nothing -> setSourceRGB r g b
            Just alpha -> setSourceRGBA r g b alpha

        
        save
        newPath
        translate (fst $ position a) (snd $ position a)
        scale (fst $ scaleValue a) (snd $ scaleValue a)
        rotate (rotation a)
        mapM_ (\p -> lineTo (fst p) (snd p)) (points a)
        if (isSegment a) 
            then return ()
            else closePath

        setLineWidth ((/) w $ fst s)

        strokePreserve 
        case fillColor a of
            Nothing -> return ()
            Just (Color fr fg fb fa) -> case fa of
                Nothing -> setSourceRGB fr fg fb >> fill
                Just alpha -> setSourceRGBA fr fg fb alpha >> fill 
        mapM_ (renderCairo . snd) $ toList (children a)
        
        -- Render the Bounding Box
        {-newPath-}
        {-setLineWidth ((/) w $ 2 * fst s)-}
        {-lineTo (fst b0) (snd b0) >> lineTo (fst b1) (snd b0)-}
        {-lineTo (fst b1) (snd b1) >> lineTo (fst b0) (snd b1)-}
        {-closePath-}
        {-stroke-}

        restore




