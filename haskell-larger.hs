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

bevel :: Waterfall.Solid -> Waterfall.Solid
bevel = 
    let bevelF (a, b) = if a ^. _z == 0 &&  b^. _z == 0 then Nothing else Just 0.4
    in  Waterfall.roundConditionalFillet bevelF


bevelLarger :: Waterfall.Solid -> Waterfall.Solid
bevelLarger = 
    let bevelF (a, b) | a ^. _z == 0 && b ^. _z == 0 = Nothing
                      | a ^. _z == b^. _z = Just 0.4
                      | otherwise = Just 0.2
    in  Waterfall.roundConditionalFillet bevelF

haskellLogo :: Waterfall.Solid
haskellLogo = 
    let paths = [
            Waterfall.pathFrom (V2 0 12)
                [ Waterfall.lineTo (V2 4 6)
                , Waterfall.lineTo (V2 0 0)
                , Waterfall.lineTo (V2 3 0)
                , Waterfall.lineTo (V2 7 6)
                , Waterfall.lineTo (V2 3 12)
                ],
            Waterfall.pathFrom (V2 4 0)
                [ Waterfall.lineTo (V2 8 6)
                , Waterfall.lineTo (V2 4 12)
                , Waterfall.lineTo (V2 7 12)
                , Waterfall.lineTo (V2 15 0)
                , Waterfall.lineTo (V2 12 0)
                , Waterfall.lineTo (V2 9.5 3.75)
                , Waterfall.lineTo (V2 7 0)
                ], 
             Waterfall.pathFrom (V2 13.66 3.5)
                [ Waterfall.lineTo (V2 12.333 5.5)
                , Waterfall.lineTo (V2 17 5.5)
                , Waterfall.lineTo (V2 17 3.5)
                ],
            Waterfall.closeLoop $ Waterfall.pathFrom (V2 11.666 6.5)
                [ Waterfall.lineTo (V2 10.333 8.5)
                , Waterfall.lineTo (V2 17 8.5)
                , Waterfall.lineTo (V2 17 6.5)
                ]
            ]
        connectors  = bevel . mconcat $ 
            [ Waterfall.unitCube & Waterfall.scale (V3 10 1 0.75) & Waterfall.translate (V3 5 4 0)
            , Waterfall.unitCube & Waterfall.scale (V3 10 1 0.75) & Waterfall.translate (V3 5 7 0)
            ]
        logo = bevelLarger . mconcat . fmap (Waterfall.prism 2 . Waterfall.fromPath . Waterfall.closeLoop) $ paths
    in logo <> connectors

ornament :: Waterfall.Solid
ornament = 
    let com = Waterfall.centerOfMass haskellLogo
        centeredLogo = Waterfall.translate ( negate $ unit _x ^* (com ^. _x)) haskellLogo

        bridge = Waterfall.unitCube 
            & Waterfall.translate (unit _x ^* (-0.5))
            & Waterfall.scale (V3 1 5 0.75)
            & Waterfall.translate (V3 0 8.5 0)
            & bevel
        hoop = Waterfall.unitCylinder 
            & Waterfall.scale (V3 2 2 0.75)
            & (`Waterfall.difference` (Waterfall.centeredCylinder & Waterfall.scale (V3 1 1 10)))
            & Waterfall.translate (V3 0 15 0)
            & bevel
    in (hoop <> bridge <> centeredLogo)
            & Waterfall.uScale 4
            -- & Waterfall.roundFillet 5 


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "haskell-ornament-larger.stl" ornament
