{-# OPTIONS_GHC -Wall #-}

module Main where

import Linear
import Vis


repeatBoxes :: Float -> Float -> VisObject Box -> (Float,Float) -> [VisObject Box]
repeatBoxes 0 0 _ _ = []
repeatBoxes 0 ny (Box (x,y,z) t c) (off,rep) = repeatBoxes rep (ny-1) (Box (x,y,z) t c) (off,rep)
repeatBoxes nx ny (Box (x,y,z) t c) (off,rep) = drawBox:nextBox
 where
  centerOff = ((abs (ny-(rep/2))) + (abs (nx-(rep/2)))) * (((-2)*pi)/rep)
  nextBox = (repeatBoxes (nx-1) (ny) (Box (x,y,z) t c) (off,rep))
  drawBox = (Trans (V3 (nx - (rep/2)) (ny-(rep/2)) 0) $ Box (x,y,z + ((-1 + sin ((off*4)+centerOff)*4.5))) t c)

drawCubes :: Float -> VisObject Float
drawCubes time = VisObjects $ text:boxes
  where
    text = Text2d (show time) (100,100) TimesRoman24 green
    x = time
    boxes = repeatBoxes 17 17 (Box (1,1,10) Solid chartreuse) (x,17)

main :: IO ()
main = do
  animate (defaultOpts {optWindowName = "Coding Challange #86"}) drawCubes



































{-drawFun :: Float -> VisObject Double
drawFun time = VisObjects $ [axes,box,ellipsoid,sphere] ++ (map text [-5..5]) ++ [boxText, plane]
  where
    x = realToFrac $ -1 + sin (2*time)
    quat = normalize $ Quaternion 1 (V3 x (1-x) (x*x))

    axes = Axes (0.5, 15)
    sphere = Trans (V3 0 x (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)

    ellipsoid = Trans (V3 x 0 (-1)) $ RotQuat quat $ Ellipsoid (0.2, 0.3, 0.4) Solid (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x) $ RotQuat quat $ Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)-}
