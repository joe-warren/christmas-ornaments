#!/usr/bin/env stack
{- stack script --resolver lts-22.6 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.4.0.0
    --extra-dep opencascade-hs-0.4.0.0
-}

-- print two of these, one of them mirrored, and then glue them back to back
-- to get a more symetrical ornament

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

snowflakeLogo :: Waterfall.Solid
snowflakeLogo = 
    let 
        chevrons = 
            Waterfall.centeredCylinder 
                & Waterfall.scale (V3 5 5 1)
                & (Waterfall.translate (unit _y ^*20))
                & (<> (Waterfall.centeredCube & Waterfall.translate (unit _y ^* 0.5) & Waterfall.scale (V3 10 20 1)))
                & (mconcat [Waterfall.rotate (unit _z) phi | phi <- [-pi/6, pi/6]]) 
                & (<> (Waterfall.centeredCylinder & Waterfall.scale (V3 5 5 1)))
                & Waterfall.translate (unit _y ^* 17)
                & mconcat [Waterfall.rotate (unit _z) phi | phi <- [0, pi/3 .. 2*pi]]
        centerCube = Waterfall.centeredCube 
            & Waterfall.scale (V3 15 15 1) 
            & Waterfall.intersection (Waterfall.centeredCylinder & Waterfall.uScale 9.5)
            & (`Waterfall.difference` (Waterfall.centeredCube & Waterfall.uScale 5))
            & Waterfall.rotate (unit _z) (pi/4)
    in chevrons <> centerCube
    
circle :: Waterfall.Path2D
circle = Waterfall.pathFrom (unit _x)
                [ Waterfall.arcViaTo (unit _y) (negate $ unit _x)
                , Waterfall.arcViaTo (negate $ unit _y) (unit _x)
                ]

ornament :: Waterfall.Solid
ornament = 
    let 
        snowflakeLogo' = snowflakeLogo
            & Waterfall.translate (unit _z ^* 0.5) 
            & Waterfall.scale (V3 1 1 5)
        rawHoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 3 $ circle) (Waterfall.fromPath circle)
        hoopClipped = rawHoop `Waterfall.intersection` (Waterfall.centeredCube & Waterfall.translate (unit _z ^* 0.5) & Waterfall.uScale 10)
        hoopPositioned = hoopClipped 
            & Waterfall.uScale 2
            & Waterfall.translate (unit _x ^* 43)
        joinersA = Waterfall.unitCylinder 
            & Waterfall.scale (V3 25 25 1)
            & (`Waterfall.difference` (Waterfall.centeredCylinder & Waterfall.uScale 24))
        joinersB = Waterfall.unitCube 
            & Waterfall.translate (unit _x ^* (-0.5))
            & Waterfall.scale (V3 1 15 1)
            & Waterfall.translate (unit _y ^* 5)
            & mconcat [Waterfall.rotate (unit _z) phi | phi <- [0, pi/3 .. 2*pi]]
    in snowflakeLogo' <> hoopPositioned <> joinersA <> joinersB


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "snowflake-ornament.stl" ornament
