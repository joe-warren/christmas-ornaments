#!/usr/bin/env stack
{- stack script --resolver lts-22.6 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.4.0.0
    --extra-dep opencascade-hs-0.4.0.0
-}

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

rubyLogo :: Waterfall.Solid
rubyLogo = 
    let angle = pi / 8
        pathA = Waterfall.fromPath2D . nGon $ 8
        pathB = pathA & Waterfall.uScale 0.5 & Waterfall.translate (unit _z ^* 0.5)
        pathC = pathA & Waterfall.rotate (unit _z) angle & Waterfall.uScale (1 / (cos angle))
        pathD = pathB & Waterfall.rotate (unit _z) angle & Waterfall.scale (V3 (cos angle) (cos angle) 1)
        pathE = circle & Waterfall.fromPath2D & Waterfall.uScale 0.4 & Waterfall.rotate (unit _z) angle & Waterfall.translate (unit _z ^* 0.55)

        precision = 1e-6
        pointedLoft = Waterfall.pointedLoft precision
        loft = Waterfall.loft precision
        partA = pointedLoft (Just (unit _z ^* (-1.25))) [pathA] Nothing
        partB =  loft [pathA, pathB] `Waterfall.intersection` loft [pathC, pathD]
        partC =  loft [pathD, pathE]
    in (partA <> partB <> partC) 

    
nGon :: Int -> Waterfall.Path2D
nGon n =
    let angle = pi * 2 / (fromIntegral n) 
     in Waterfall.pathFrom2D (unit _x)
                [ Waterfall.lineTo (Waterfall.rotate2D ( fromIntegral i * angle) (unit _x))
                | i <- [1..n]
                ]

circle :: Waterfall.Path2D
circle = Waterfall.pathFrom (unit _x)
                [ Waterfall.arcViaTo (unit _y) (negate $ unit _x)
                , Waterfall.arcViaTo (negate $ unit _y) (unit _x)
                ]

hexagon :: Waterfall.Path2D
hexagon = Waterfall.pathFrom2D (unit _x)
                [ Waterfall.lineTo (Waterfall.rotate2D ( fromIntegral i * pi / 3) (unit _x))
                | i <- [1..6]
                ]

ornament :: Waterfall.Solid
ornament = 
    let 
        rubyLogo' = Waterfall.rotate (unit _y) (pi/4) rubyLogo
        com = Waterfall.centerOfMass rubyLogo'
        Just (lo, hi) = Waterfall.axisAlignedBoundingBox rubyLogo'
        hoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 2 $ circle) (Waterfall.fromPath circle)
        hoopScale = 0.05
        positionedHoop = hoop 
            & Waterfall.uScale hoopScale
            & Waterfall.rotate (unit _x) (pi/2) 
            & Waterfall.translate (unit _z ^* (hi ^. _z))
            & Waterfall.translate (unit _x ^* (com ^. _x))
        width = (hi - lo) ^. _x 
        scaleFac = 40 / width
    in Waterfall.uScale scaleFac (rubyLogo' <> positionedHoop)


main :: IO ()
main = do
    Waterfall.writeSTL 0.01 "ruby-ornament.stl" ornament
