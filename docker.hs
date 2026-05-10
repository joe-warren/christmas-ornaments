#!/usr/bin/env stack
{- stack script --resolver lts-23.16 
    --package linear
    --package lens
    --package waterfall-cad
    --package waterfall-cad-svg
    --extra-dep waterfall-cad-svg-0.6.2.1
    --extra-dep waterfall-cad-0.6.2.1
    --extra-dep opencascade-hs-0.6.2.1
-}
import qualified Waterfall as W
import qualified Waterfall.SVG as SVG
import Linear
import Control.Lens
import Data.Function ((&))

filletF :: (V3 Double, V3 Double) -> Maybe Double
filletF (a, b) = 
    if (nearZero (a ^. _z - 5) && nearZero (b ^. _z - 5))
      then Just 4.9
      else Nothing

crossSection :: W.Path
crossSection = 
  let halfPath = 
        W.pathFrom (V3 0 0 5)
          [ W.lineTo (V3 0 (20 - 4.9) 5)
          , W.arcViaRelative (V3 0 (4.9 / sqrt 2) (4.9 * (1 / sqrt 2 - 1))) (V3 0 4.9 (-4.9))
          , W.lineRelative (unit _z ^* negate 0.1)
          , W.splice 
            ( W.arcVia (unit _y) (V3 0 (1/sqrt 2) (negate (1/sqrt 2))) (negate $ unit _z ) 
              & W.scale (V3 25 20 25)
            )
          ]
  in halfPath <> (halfPath & W.mirror (unit _y) & W.reversePath)

frontBody :: W.Solid
frontBody = mconcat 
  [ W.unitSphere 
      & W.scale (V3 25 20 25)
      & (`W.difference` (W.centeredCube & W.translate (unit _z / 2) & W.uScale 100))
  , W.unitCylinder
      & W.scale (V3 25 20 5 )
      & W.roundConditionalFillet filletF
  ] `W.intersection`
  ( W.centeredCube 
    & W.translate (unit _x /2 )
    & W.uScale 100
  )

rearBody :: W.Solid
rearBody = W.pointedLoft 
  Nothing 
  [ crossSection
  , crossSection & W.translate (unit _x ^* (-5))
  , crossSection 
      & W.uScale 0.2 
      & W.translate (V3 (-38) 0 8)
  ]
  (Just (V3 (-40) 0 10))
  
body = frontBody <> rearBody

fin :: W.Solid
fin = 
  let cyl = W.centeredCylinder 
            & (<> (W.unitSphere & W.translate (unit _z / 2)))
            & (<> (W.unitSphere & W.translate (unit _z / negate 2)))
            & W.uScale 10 
      a = cyl & W.translate (unit _x ^* 5)
      b = cyl & W.translate (unit _x ^* negate 5)
      f = (a `W.intersection` b) 
            & W.scale (V3 1 1 0.2)
  in case W.axisAlignedBoundingBox f of
        Nothing -> f
        Just (lo, hi) -> W.translate (unit _y ^* (hi ^. _y)) f


fins :: W.Solid
fins = (fin <> W.rotate (unit _z) (pi/3) fin)
  & W.rotate (unit _z) 0.2
  & W.rotate (unit _x) (pi/2)

cubeLocs :: [V3 Double]
cubeLocs = 
  [V3 x 0 0 | x <- [0..5]]
  <> [V3 x 0 1 | x <- [1..4]]
  <> [V3 4 0 2]
  <> [V3 x 1 0 | x <- [1..4]]
  <> [V3 x (-1) 0 | x <- [1..4]]

oneCube :: W.Solid
oneCube =
  W.centeredCube
   & W.translate (unit _z / 2)
   & W.uScale 7.5
   & W.chamfer 0.8

cubes = mconcat [W.translate (pos ^* 7.5) oneCube | pos <- cubeLocs ]
  & W.rotate (unit _z) (pi)
  & W.translate (V3 14 0 5)
  
eyeHoles = mconcat 
  [ W.translate
       (V3 2.5 (18 * i) (-10)) 
      (W.scale (V3 3 2 3) W.unitSphere)
       | i <- [-1, 1]
  ]

eyes = mconcat 
  [ W.translate
       (V3 2.5 ((18-1.5) * i) (-9.5)) 
      (W.uScale 2.5 W.unitSphere)
       | i <- [-1, 1]
  ]

whale :: W.Solid
whale = 
  ( mconcat
    [ body
    , W.translate (V3 (-37) 0 7) fins
    , cubes
    ] 
    `W.difference` eyeHoles
  ) <> eyes

ornament :: W.Solid
ornament = 
  let com = W.centerOfMass whale
      torus =
        W.torus 4 1.75
          & W.rotate (unit _x) (pi/2)
  in whale <> W.translate (unit _z ^* 23 + unit _x * com) torus
 
halfOrnament :: W.Solid 
halfOrnament = ornament `W.intersection` (W.centeredCube & W.translate (unit _y /2) & W.uScale 200)

main :: IO ()
main = do
  W.writeSTL 0.1 "docker.stl" ornament
  W.writeSTL 0.1 "docker-half.stl" halfOrnament


