#!/usr/bin/env stack
{- stack script --resolver lts-23.16 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.5.0.0
    --extra-dep opencascade-hs-0.5.0.0
-}

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

lambda :: Waterfall.Solid
lambda = 
    let path = Waterfall.pathFrom (V2 293 110.72) 
                [ Waterfall.bezierRelative (V2 78.194 85.936) (V2 24.928 152.64) (V2 (-24) 250)
                , Waterfall.lineRelative (V2 40 0)
                , Waterfall.bezierRelative (V2 22.047 (-35.179)) (V2 39.35 (-75.102)) (V2 55.25 (-116.43))
                , Waterfall.bezierRelative (V2 15.546 37.01) (V2 30.807 74.282) (V2 40.75 116.43)
                , Waterfall.lineRelative (V2 40 0) 
                , Waterfall.bezierRelative (V2 (-29.561) (-102.33)) (V2 (-64.433) (-172.76)) (V2 (-112) (-250))
                ]
        solidify = Waterfall.prism 1 . Waterfall.makeShape . Waterfall.closeLoop
    in Waterfall.mirror (unit _y) . Waterfall.scale (V3 0.03 0.03 1) $ solidify path

center :: Waterfall.Solid -> Waterfall.Solid 
center s =
    let Just (lo, high) = Waterfall.axisAlignedBoundingBox s
        v = (lo + high) ^* (- 0.5)
      in  Waterfall.translate v s

yinYang :: Waterfall.Solid 
yinYang =
    let extCylinder = Waterfall.unitCylinder &
            Waterfall.scale (V3 9 9 2)
        cubeMask = Waterfall.centeredCube & 
            Waterfall.translate (unit _x ^* (-0.5)) &
            Waterfall.uScale 100 
        halfCylinder = Waterfall.unitCylinder &
            Waterfall.scale (V3 8 8 2) &
            (`Waterfall.difference` cubeMask)
        smallCylinder = Waterfall.unitCylinder & 
            Waterfall.scale (V3 4 4 2)
        yang = (halfCylinder `Waterfall.difference` (smallCylinder & Waterfall.scale (V3 1 1 2) & Waterfall.translate (unit _y ^* 4))) 
            <> (smallCylinder & Waterfall.translate (unit _y ^* (-4)))
    in (extCylinder `Waterfall.difference` (yang & Waterfall.translate (unit _z))) & 
            Waterfall.rotate (unit _z) (pi/4)

lispLogo :: Waterfall.Solid
lispLogo = 
    let positionedLambda = Waterfall.translate (V3 3.6 0.75 1.5) (center lambda)
    in (yinYang <> positionedLambda) `Waterfall.difference`
            (Waterfall.rotate (unit _z) pi positionedLambda)

ornament :: Waterfall.Solid
ornament = lispLogo

main :: IO ()
main = do
    Waterfall.writeSTL 0.05 "lisp-ornament.stl" ornament
