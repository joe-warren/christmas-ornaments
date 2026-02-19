#!/usr/bin/env stack
{- stack script --resolver lts-24.11
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.6.1.0
    --extra-dep opencascade-hs-0.6.1.0
-}

import qualified Waterfall as W
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

hollowCylinder :: Double -> Double -> Double -> W.Solid
hollowCylinder innerR outerR height = 
    let a = W.centeredCylinder 
                & W.scale (V3 outerR outerR height)
        b = W.centeredCylinder
                & W.scale (V3 innerR innerR (height * 2))
    in a `W.difference` b


kafkaLogo :: W.Solid
kafkaLogo = 
    let height = 3
        smallCyl = 
            hollowCylinder 1 2 height
                & W.chamfer 0.2
                
        spoke = 
            W.centeredCube 
                & W.translate (unit _x ^* 0.5)
                & W.scale (V3 2 0.75 height)
                & W.translate (unit _x ^* 2.25)
        leg = smallCyl 
                    & W.translate (unit _x ^* 6)
                    & W.union spoke
        legs = take 4 . iterate (W.rotate (unit _z) (pi/3)) $ leg
        centerCyl = 
            hollowCylinder 1.51 3.01 height 
                & W.chamfer 0.2
    in mconcat (centerCyl : legs)

ornament :: W.Solid
ornament = 
    let
        scale = 5
        logo' = W.uScale scale kafkaLogo
        center = W.centerOfMass  logo'
        logo = W.translate (negate center) logo'
        hoop = 
            W.torus 5 2
                & W.rotate (unit _x) (-pi/4)
                & W.translate (unit _x ^* (7 * scale))
        in logo <> hoop
 

main :: IO ()
main = do
    W.writeSTL 0.005 "kafka-ornament.stl" ornament
