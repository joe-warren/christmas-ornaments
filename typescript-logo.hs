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

circle :: Waterfall.Path2D
circle = Waterfall.pathFrom (unit _x)
                [ Waterfall.arcViaTo (unit _y) (negate $ unit _x)
                , Waterfall.arcViaTo (negate $ unit _y) (unit _x)
                ]

ornament :: Waterfall.Font -> Waterfall.Solid
ornament f = 
    let ts = Waterfall.translate (unit _z ^* (-10)) . Waterfall.prism 20 . Waterfall.text f $ "TS"
        bevelF (a, b) = if a ^. _xy == b^. _xy then Just 5 else Nothing
        bevel = Waterfall.roundConditionalFillet bevelF
        cube = Waterfall.centeredCube &
                Waterfall.scale (V3 50 50 5) & 
                bevel
        hoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 1.5 $ circle) (Waterfall.fromPath circle)
        positionedHoop = hoop &
            Waterfall.uScale 3 &
            (`Waterfall.intersection` (Waterfall.centeredCube & Waterfall.scale (V3 100 100 5))) &
            Waterfall.translate (unit _y ^* 27)
    in (cube <> positionedHoop) `Waterfall.difference` (Waterfall.translate (V3 4 (-10) 0) ts)


main :: IO ()
main = do
    f <- Waterfall.fontFromPath "CascadiaCode-Bold.otf" 32
    Waterfall.writeSTL 0.25 "typescript-ornament.stl" (ornament f)
