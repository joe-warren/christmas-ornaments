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


swiftLogo :: Waterfall.Solid
swiftLogo = 
    let path = 
            Waterfall.pathFrom (V2 47.0606 36.6607)
                [ Waterfall.bezierRelative (V2 (-0.0014) (-0.0018)) (V2 (-0.0027) (-0.0031)) (V2 (-0.0042) (-0.0048))
                , Waterfall.bezierRelative (V2 0.0657 (-0.2236)) (V2 0.1335 (-0.4458)) (V2 0.191 (-0.675))
                , Waterfall.bezierRelative (V2 2.465 (-9.8209)) (V2 (-3.5511) (-21.4319)) (V2 (-13.7316) (-27.5454))
                , Waterfall.bezierRelative (V2 4.4613 6.0479) (V2 6.4339 13.3733) (V2 4.6813 19.7795)
                , Waterfall.bezierRelative (V2 (-0.1563) 0.5714) (V2 (-0.3442) 1.1198) (V2 (-0.5519) 1.6528)
                , Waterfall.bezierRelative (V2 (-0.2254) (-0.1481)) (V2 (-0.5094) (-0.3162)) (V2 (-0.8908) (-0.5265))
                , Waterfall.bezierRelative (V2 0 0) (V2 (-10.1269) (-6.2527)) (V2 (-21.1028) (-17.3122))
                , Waterfall.bezierRelative  (V2 (-0.288) (-0.2903)) (V2 5.8528 8.777) (V2 12.8219 16.1399)
                , Waterfall.bezierRelative (V2 (-3.2834) (-1.8427)) (V2 (-12.4338) (-8.5004)) (V2 (-18.2266) (-13.8023))
                , Waterfall.bezierRelative (V2 0.7117 1.1869) (V2 1.5582 2.3298) (V2 2.4887 3.4301)
                , Waterfall.bezierRelative (V2 4.8375 6.1349) (V2 11.1462 13.7044) (V2 18.7043 19.5169)
                , Waterfall.bezierRelative (V2 (-5.3104) 3.2498) (V2 (-12.8141) 3.5025) (V2 (-20.2852) 0.0034)
                , Waterfall.bezierRelative (V2 (-1.8479) (-0.866)) (V2 (-3.5851) (-1.9109)) (V2 (-5.1932) (-3.0981))
                , Waterfall.bezierRelative (V2 3.1625 5.0585) (V2 8.0332 9.4229) (V2 13.9613 11.9708)
                , Waterfall.bezierRelative (V2 7.0695 3.0381) (V2 14.0996 2.8321) (V2 19.3356 0.0498)
                , Waterfall.lineRelative (V2 (-0.0041) 0.006)
                , Waterfall.bezierRelative (V2 0.0239 (-0.0151)) (V2 0.0543 (-0.0316)) (V2 0.0791 (-0.0469))
                , Waterfall.bezierRelative (V2 0.215 (-0.1156)) (V2 0.4284 (-0.2333)) (V2 0.6371 (-0.3576))
                , Waterfall.bezierRelative (V2 2.5157 (-1.3058)) (V2 7.4847 (-2.6306)) (V2 10.1518 2.5588)
                , Waterfall.bezierTo (V2 50.7755 49.6699) (V2 52.1635 42.9395) (V2 47.0606 36.6607)
                ]
                
        rawLogo = Waterfall.translate (unit _z ^* 3) . Waterfall.rotate (unit _x) pi $
            Waterfall.prism 3 . Waterfall.makeShape . Waterfall.closeLoop $ path
        Just (lo, hi) = Waterfall.axisAlignedBoundingBox rawLogo
        scale = 30 / (hi ^. _x - lo ^. _x) 
        scaledLogo = rawLogo &
            Waterfall.translate (negate lo) &
            Waterfall.scale (V3 scale scale 1)
    in scaledLogo 

ornament :: Waterfall.Solid
ornament = 
    let com = Waterfall.centerOfMass swiftLogo
        centeredLogo = Waterfall.translate ( negate $ unit _x ^* (com ^. _x)) swiftLogo
        bridge = Waterfall.unitCube 
            & Waterfall.translate (unit _x ^* (-0.5))
            & Waterfall.scale (V3 1 5 1)
            & Waterfall.union Waterfall.unitCylinder
            & Waterfall.union (Waterfall.unitSphere & Waterfall.translate (unit _z))
            & Waterfall.translate (V3 0 27 0)
        hoop = Waterfall.unitCylinder 
            & Waterfall.scale (V3 2 2 1)
            & (`Waterfall.difference` (Waterfall.centeredCylinder & Waterfall.scale (V3 1 1 10)))
            & Waterfall.translate (V3 0 33.5 0)
    in hoop <> bridge <> centeredLogo


main :: IO ()
main = do
    Waterfall.writeSTL 0.1 "swift-ornament.stl" ornament
