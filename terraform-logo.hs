#!/usr/bin/env stack
{- stack script --resolver lts-23.16 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.5.0.0
    --extra-dep opencascade-hs-0.5.0.0
-}

-- print two of these, one of them mirrored, and then glue them back to back
-- to get a more symetrical ornament

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

toOrthographic :: Waterfall.Solid -> Waterfall.Solid
toOrthographic = 
    Waterfall.rotate (unit _z) (-pi/6)
        . Waterfall.scale  (V3 1 (sqrt 3) 1)
        . Waterfall.rotate (unit _z) (pi/4) 

terraformLogo :: Waterfall.Solid
terraformLogo = 
    let cubesA = mconcat 
            [ Waterfall.unitCube
            , Waterfall.unitCube & Waterfall.translate (unit _x ^* 1.1) 
            , Waterfall.unitCube & Waterfall.translate (unit _y ^* (-1.1))
            ]
        cubesA' = cubesA 
            & Waterfall.translate (unit _x ^* 0.05) 
            & toOrthographic
            & Waterfall.mirror (unit _x)
        cubesB' = Waterfall.unitCube
            & Waterfall.translate (unit _x ^* 0.05) 
            & toOrthographic
        logo = cubesA' <> cubesB'
    in logo

    
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
    let com = Waterfall.centerOfMass terraformLogo
        centeredLogo = Waterfall.translate (negate com) terraformLogo
            & Waterfall.uScale 8
        
        bevelF (a, b) = if a ^. _xy == b^. _xy then Just 5 else Nothing
        bevel = Waterfall.roundConditionalFillet bevelF
        shape  = hexagon 
                & Waterfall.makeShape
                & Waterfall.prism 1
                & Waterfall.translate (unit _z ^* (-0.5))
                & Waterfall.scale (V3 30 30 5) 
                & bevel
        hoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 1.5 $ circle) (Waterfall.makeShape circle)
        positionedHoop = hoop &
            Waterfall.uScale 3 &
            (`Waterfall.intersection` (Waterfall.centeredCube & Waterfall.scale (V3 100 100 5))) &
            Waterfall.translate (unit _y ^* 30)
    in (shape <> positionedHoop) `Waterfall.difference` (centeredLogo)


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "terraform-ornament.stl" ornament
