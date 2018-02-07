module Main where

import Linear
import Vis


repeatBoxes :: Float -> Float -> VisObject Float -> (Float,Float) -> [VisObject Float]
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
  animate (defaultOpts {optWindowName = "Coding Challange #86", optInitialCamera = Just (Camera0 50 30 40)}) drawCubes
