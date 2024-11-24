#!/usr/bin/env stack
{- stack script --resolver lts-22.6 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.4.0.0
    --extra-dep opencascade-hs-0.4.0.0
-}

-- short-description: Scala Logo
--
-- description: It's the Scala logo, but it's designed in Haskell
--
-- image: /photos/scala-logo-02.jpg
-- image: /photos/scala-logo-03.jpg

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

-- References:
-- 1. A. Riskus, "Approximation of a Cubic Bezier Curve by Circular Arcs and Vice Versa"
-- 2. Imre Juhasz, "Approximating the helix with rational cubic Bezier curves"
createHelicalArc :: Double -> Double -> Double -> Waterfall.Path 
createHelicalArc r pitch incAngle =
  let alpha = incAngle / 2 -- half included angle
      p = pitch/(2* pi) --  helix height per radian
      ax = r * cos alpha
      ay = r * sin alpha
      b = p * alpha * (r - ax) * (3*r - ax)/(ay * (4*r - ax) * tan alpha)
      b0 = V3 ax (negate ay) (negate alpha*p)
      b1 = V3 ((4*r - ax)/3) (negate $ (r - ax)*(3*r - ax)/(3*ay)) (negate b)
      b2 = V3 ((4*r - ax)/3) ((r - ax)*(3*r - ax)/(3*ay)) b
      b3 = V3 ax ay (alpha*p)
  in Waterfall.bezier b0 b1 b2 b3

scalaLogo :: Waterfall.Solid
scalaLogo = 
    let radius = 15
        pitch = 15
        segmentsPerTurn = 8
        segmentsPerTurn' = fromIntegral segmentsPerTurn
        incAngle = 2 * pi / segmentsPerTurn'
        incHeight = pitch / segmentsPerTurn'
        segment = createHelicalArc radius pitch incAngle
        oneStep = Waterfall.translate (incHeight *^ unit _z) . Waterfall.rotate (unit _z) incAngle
        totalSegments = 5 * segmentsPerTurn `div` 2 
        path = mconcat . take totalSegments . iterate oneStep $ segment
        profile = Waterfall.uScale2D (7.5 *3/4) Waterfall.unitCircle
        mask = Waterfall.scale (V3 radius radius 150) Waterfall.centeredCylinder `Waterfall.difference`
            Waterfall.scale (V3 (radius-2.5) (radius-2.5) 200) Waterfall.centeredCylinder
        logo = Waterfall.rotate (unit _z) (pi + incAngle/2) 
                (Waterfall.sweep path profile `Waterfall.intersection` mask)
        Just (V3 _ _ minZ, _) = Waterfall.axisAlignedBoundingBox logo
    in Waterfall.translate (negate minZ *^ unit _z) logo

ornament :: Waterfall.Solid
ornament = 
    let 
        hole h angle = 
            Waterfall.unitCylinder &
                Waterfall.scale (V3 0.75 0.75 100) &
                Waterfall.rotate (unit _y) (pi/2) &
                Waterfall.rotate (unit _z) angle &
                Waterfall.translate (unit _z ^* h)
                

     in scalaLogo `Waterfall.difference` ((hole 45 (-0.27)) <> (hole 37 (pi - 0.27)))


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "scala-ornament.stl" ornament
