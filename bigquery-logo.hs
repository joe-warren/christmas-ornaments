#!/usr/bin/env stack

-- print two of these, one of them mirrored, and then glue them back to back
-- to get a more symetrical ornament

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))
import Waterfall.SVG (parsePath)

toOrthographic :: Waterfall.Solid -> Waterfall.Solid
toOrthographic = 
    Waterfall.rotate (unit _z) (-pi/6)
        . Waterfall.scale  (V3 1 (sqrt 3) 1)
        . Waterfall.rotate (unit _z) (pi/4) 

bigqueryLogo :: Waterfall.Solid
bigqueryLogo = 
    let Right paths = fmap mconcat . sequence $
            [ parsePath "m 52.99,63.104 v 7.21 c 1.066792,1.83144 2.571869,3.369161 4.38,4.475 V 63.104 Z m 8.685,-6.078 v 19.411 c 0.745,0.137 1.507,0.22 2.29,0.22 0.714,0 1.41,-0.075 2.093,-0.189 V 57.026 Z m 9.091,9.074 v 8.562 c 1.83695,-1.168093 3.34525,-2.785849 4.382,-4.7 v -3.861 z m 9.925,12.187 -2.403,2.405 c -0.423782,0.424699 -0.423782,1.112301 0,1.537 l 9.115,9.112 c 0.424699,0.423782 1.112301,0.423782 1.537,0 l 2.403,-2.402 c 0.420935,-0.425483 0.420935,-1.110517 0,-1.536 l -9.116,-9.116 c -0.425036,-0.422023 -1.110964,-0.422023 -1.536,0Z"
            ]
        
        pathPart =  Waterfall.scale (V3 0.5 0.5 1) $ Waterfall.translate (unit _z ^* (-5)) $ Waterfall.translate (V3 (-65) (-65) 0) $
            mconcat . fmap (Waterfall.prism 10 . Waterfall.fromPath) $ paths
        circlePart = Waterfall.centeredCylinder 
            & Waterfall.scale (V3 11.5 11.5 10)  
            & (`Waterfall.difference` (Waterfall.centeredCylinder & Waterfall.scale (V3 8.7 8.7 20)))
    in (pathPart <> circlePart) & Waterfall.rotate (unit _z) (pi)
    
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

joiner :: Waterfall.Solid
joiner = mconcat
                [ Waterfall.centeredCube 
                    & Waterfall.scale (V3 0.4 10 5 )  
                    & Waterfall.translate (unit _y ^* 13)
                    & Waterfall.rotate (unit _z) (i* pi/2)
                | i <- [0 ..3]
                ]

ornament :: Waterfall.Solid
ornament = 
    let 
        
        bevelF (a, b) = if a ^. _xy == b^. _xy then Just 5 else Nothing
        bevel = Waterfall.roundConditionalFillet bevelF
        shape  = hexagon 
                & Waterfall.fromPath 
                & Waterfall.prism 1
                & Waterfall.translate (unit _z ^* (-0.5))
                & Waterfall.scale (V3 30 30 5) 
                & bevel
        hoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 1.5 $ circle) (Waterfall.fromPath circle)
        positionedHoop = hoop &
            Waterfall.uScale 3 &
            (`Waterfall.intersection` (Waterfall.centeredCube & Waterfall.scale (V3 100 100 5))) &
            Waterfall.translate (unit _y ^* 30)
    in ((shape <> positionedHoop) `Waterfall.difference` bigqueryLogo) <> joiner


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "bigquery-ornament.stl" ornament
