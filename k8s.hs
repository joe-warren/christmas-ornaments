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


plaque :: W.Solid
plaque= W.unitPolygon 7 
  & W.uScale2D 28
  & W.prism 3
  & W.roundConditionalFillet (W.whenNearlyEqual _xy 5)

pathString :: String 
pathString = "m 0,0 h 5e-5 c 8.07696,-8.2e-4 14.62404,7.27591 14.625,16.25 0.005,1.27224 0.075,2.97962 0.0312,4.15625 -0.19262,5.176 -1.3209,9.13749 -2,13.90625 -1.23028,10.20666 -2.29242,18.66736 -1.65625,26.53125 0.57783,3.93564 2.87754,5.49085 4.78125,7.3125 0.0433,1.06008 0.21369,4.58253 0.34375,6.53125 l 2.21875,38.75 4.34375,76.59385 0.0625,0.0312 c 0.11178,2.63636 1.04118,5.24779 2.8125,7.46875 1.92246,2.41032 4.53515,3.94774 7.33183,4.55593 l -21.14433,37.56897 h -11.74995"

spoke :: W.Solid
spoke = SVG.parsePath pathString
  & either (error . show) head
  & W.revolution
  & W.rotate (unit _y) (pi/2) 

positionedSpoke = 
  case W.axisAlignedBoundingBox spoke of
    Nothing -> spoke
    Just (lo, hi) ->
      spoke 
        & W.uScale (20 / (hi ^. _x))
        & W.translate (unit _x ^* (-22))
        & W.rotate (unit _z) (pi/7)

spokes = mconcat . take 7 . iterate (W.rotate (unit _z) (2*pi/7)) $ positionedSpoke

hub = W.prism 6 (W.unitPolygon 7 & W.uScale2D 5.7)
  & (`W.difference` W.prism 8 (W.unitPolygon 7 & W.uScale2D 2))
  & W.translate (unit _z ^* negate 3)
  & W.rotate (unit _z) (pi/7)

logo = spokes <> W.torus 14.5 1.5 <> hub

hole = W.centeredCylinder 
  & W.scale (V3 1.5 1.5 10)
  & W.translate (unit _x ^* 25)

ornament = (plaque <> W.translate (unit _z ^* 3.05) logo)
  `W.difference` hole

main :: IO ()
main = W.writeSTL 0.1 "k8s.stl" ornament


