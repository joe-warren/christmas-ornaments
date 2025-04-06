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

elmLogo :: Waterfall.Solid
elmLogo = 
    let paths = [
            Waterfall.pathFrom (V2 91.783 82.916)
                [ Waterfall.lineRelative (V2 69.866 69.866) 
                , Waterfall.lineRelative (V2 69.865 (-69.866))
                ],
            Waterfall.pathFrom (V2 232.213 70.375)
                [ Waterfall.lineTo (V2 161.838 0)
                , Waterfall.lineTo (V2 8.867 0)
                , Waterfall.lineRelative (V2 70.374 70.375) 
                ],
            Waterfall.pathFrom (V2 246.99905 85.161515)
                [ Waterfall.lineRelative (V2 76.1377 76.137705)
                , Waterfall.lineRelative (V2 (-76.48488) 76.48488)
                , Waterfall.lineRelative (V2 (-76.13771) (-76.1377))
                ],
            Waterfall.pathFrom (V2 179.573 0)
                [ Waterfall.lineTo (V2 323.298 143.724)
                , Waterfall.lineTo (V2 323.298 0)
                ],
            Waterfall.pathFrom (V2 0 314.432)
                [ Waterfall.lineTo (V2 152.781 161.649)
                , Waterfall.lineTo (V2 0 8.868)
                ],
            Waterfall.pathFrom (V2 323.298 178.879)
                [ Waterfall.lineRelative (V2 (-67.776) 67.776)
                , Waterfall.lineRelative (V2 67.776 67.777) 
                ],
            Waterfall.pathFrom (V2 314.43 323.298)
                [ Waterfall.lineTo (V2 161.649 170.517)
                , Waterfall.lineTo (V2 8.869 323.298) 
                ]
            ]
            
        rawLogo = Waterfall.translate (unit _z ^* 3) . Waterfall.rotate (unit _x) pi $
            mconcat . fmap (Waterfall.prism 3 . Waterfall.fromPath . Waterfall.closeLoop) $ paths
        Just (lo, hi) = Waterfall.axisAlignedBoundingBox rawLogo
        scale = 30 / (hi ^. _x - lo ^. _x) 
        scaledLogo = rawLogo &
            Waterfall.translate (negate lo) &
            Waterfall.scale (V3 scale scale 1)
        connector size =
            Waterfall.unitCube & 
            Waterfall.scale (V3 size size 1) &
            (`Waterfall.difference` (Waterfall.centeredCube & Waterfall.uScale (size - 4 ) & Waterfall.translate (V3 (size/2) (size/2) 0))) &
            Waterfall.translate (V3 (15 - size/2) (15 -size /2) 0)
        connectors  = mconcat 
            [ connector 24
            , connector 10
            ]
    in scaledLogo <> connectors

ornament :: Waterfall.Solid
ornament = 
    let 
        bridge = Waterfall.unitCube 
            & Waterfall.translate (unit _x ^* (-0.5))
            & Waterfall.scale (V3 1 5 1)
            & Waterfall.translate (V3 15 29 0)
        hoop = Waterfall.unitCylinder 
            & Waterfall.scale (V3 2 2 1)
            & (`Waterfall.difference` (Waterfall.centeredCylinder & Waterfall.scale (V3 1 1 10)))
            & Waterfall.translate (V3 15 35.5 0)
    in hoop <> bridge <> elmLogo


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "elm-ornament.stl" ornament
