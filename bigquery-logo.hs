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
            [ parsePath "M64 40.804c-12.81 0-23.195 10.385-23.195 23.196 0 12.81 10.385 23.195 23.195 23.195S87.194 76.81 87.194 64c0-12.811-10.385-23.196-23.194-23.196m0 40.795c-9.72 0-17.6-7.88-17.6-17.6S54.28 46.4 64 46.4 81.6 54.28 81.6 64 73.72 81.6 64 81.6"
            --, parsePath "M52.99 63.104v7.21a12.794 12.794 0 0 0 4.38 4.475V63.104zM61.675 57.026v19.411c.745.137 1.507.22 2.29.22.714 0 1.41-.075 2.093-.189V57.026zM70.766 66.1v8.562a12.786 12.786 0 0 0 4.382-4.7v-3.861zM80.691 78.287l-2.403 2.405a1.088 1.088 0 0 0 0 1.537l9.115 9.112a1.088 1.088 0 0 0 1.537 0l2.403-2.402a1.092 1.092 0 0 0 0-1.536l-9.116-9.116a1.09 1.09 0 0 0-1.536 0"
            ]
        in Waterfall.scale (V3 0.5 0.5 10) $ Waterfall.translate (unit _z ^* (-5)) $
            mconcat . fmap (Waterfall.prism 10 . Waterfall.fromPath) $ paths
    
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
    in (shape <> positionedHoop) `Waterfall.difference` bigqueryLogo


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "bigquery-ornament.stl" ornament
