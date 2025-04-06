#!/usr/bin/env stack
{- stack script --resolver lts-23.16 
    --package linear
    --package lens
    --package waterfall-cad
    --package waterfall-cad-svg
    --extra-dep waterfall-cad-svg-0.5.0.0
    --extra-dep waterfall-cad-0.5.0.0
    --extra-dep opencascade-hs-0.5.0.0
-}

-- print two of these, one of them mirrored, and then glue them back to back
-- to get a more symetrical ornament

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))
import Waterfall.SVG (parsePath)

pythonLogo :: Waterfall.Solid
pythonLogo = 
    let Right path = mconcat <$> parsePath "M 54.918785,9.1927421e-4 C 50.335132,0.02221727 45.957846,0.41313697 42.106285,1.0946693 30.760069,3.0991731 28.700036,7.2947714 28.700035,15.032169 v 10.21875 h 26.8125 v 3.40625 h -26.8125 -10.0625 c -7.792459,0 -14.6157588,4.683717 -16.7499998,13.59375 -2.46181998,10.212966 -2.57101508,16.586023 0,27.25 1.9059283,7.937852 6.4575432,13.593748 14.2499998,13.59375 h 9.21875 v -12.25 c 0,-8.849902 7.657144,-16.656248 16.75,-16.65625 h 26.78125 c 7.454951,0 13.406253,-6.138164 13.40625,-13.625 v -25.53125 c 0,-7.2663386 -6.12998,-12.7247771 -13.40625,-13.9374997 C 64.281548,0.32794397 59.502438,-0.02037903 54.918785,9.1927421e-4 Z"
        eye = Waterfall.centeredCylinder 
            & Waterfall.scale (V3 5 5 30) 
            & Waterfall.translate (V3 40 13 0)
        oneSnake = path 
            & Waterfall.makeShape
            & Waterfall.prism 10
            & (`Waterfall.difference` eye)
            & Waterfall.translate (V3 (-56) (-56) 0)
        twoSnake = (id <> Waterfall.rotate (unit _z) pi) oneSnake
        maskScale = V3 58 58 5
        maskA = Waterfall.unitCylinder & Waterfall.scale maskScale
        maskB = Waterfall.unitSphere 
            & Waterfall.scale maskScale
            & Waterfall.translate (unit _z ^* (maskScale ^. _z))
    in twoSnake `Waterfall.intersection` (maskA <> maskB)
    
circle :: Waterfall.Path2D
circle = Waterfall.pathFrom (unit _x)
                [ Waterfall.arcViaTo (unit _y) (negate $ unit _x)
                , Waterfall.arcViaTo (negate $ unit _y) (unit _x)
                ]

ornament :: Waterfall.Solid
ornament = 
    let joinerA = Waterfall.unitCube 
            & Waterfall.scale (V3 2 10 2)
            & mconcat ([Waterfall.translate (V3 i (-5) 0 ) | i <- [-10, 10]])

        joinerB = Waterfall.unitCube 
            & Waterfall.scale (V3 100 2 2)
            & mconcat ([Waterfall.translate (V3 (-50) i 0 ) | i <- [-20, 20]])
                    
        rawHoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 3 $ circle) (Waterfall.makeShape circle)
        hoopClipped = rawHoop `Waterfall.intersection` (Waterfall.centeredCube & Waterfall.translate (unit _z ^* 0.5) & Waterfall.uScale 10)
        hoopPositioned = hoopClipped 
            & Waterfall.uScale 2
            & Waterfall.translate (unit _y ^* 60)
    in pythonLogo <> joinerA <> joinerB <> hoopPositioned


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "python-ornament.stl" ornament
