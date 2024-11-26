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

kotlinLogo :: Waterfall.Solid
kotlinLogo = 
    let logoPath = Waterfall.pathFrom (zero)
          [ Waterfall.lineTo (V2 0 40) 
          , Waterfall.lineTo (V2 20 20)
          , Waterfall.lineTo (V2 40 40)
          , Waterfall.lineTo (V2 40 0)
          ]

        highlightPaths = 
          [ Waterfall.pathFrom (zero)
            [ Waterfall.lineTo (V2 0 40) 
            , Waterfall.lineTo (V2 20 20)
            ]
          , Waterfall.pathFrom (V2 40 0)
            [ Waterfall.lineTo (V2 40 20) 
            , Waterfall.lineTo (V2 20 0)
            ]
          ]
        solidify x = Waterfall.prism x . Waterfall.fromPath . Waterfall.closeLoop 
    in (Waterfall.translate (unit _z ^* 2) $ solidify 36 logoPath) <> mconcat (solidify 40 <$> highlightPaths) 

circle :: Waterfall.Path2D
circle = Waterfall.pathFrom (unit _x)
                [ Waterfall.arcViaTo (unit _y) (negate $ unit _x)
                , Waterfall.arcViaTo (negate $ unit _y) (unit _x)
                ]

ornament :: Waterfall.Solid
ornament = 
  let logo = kotlinLogo & 
          Waterfall.translate (unit _z ^* (-20)) & 
          Waterfall.rotate (unit _y) (-pi/2)
      hoop = Waterfall.sweep (Waterfall.fromPath2D . Waterfall.uScale2D 3 $ circle) (Waterfall.fromPath circle)
      positionedHoop = hoop &
            Waterfall.uScale 2 &
            Waterfall.rotate (unit _x) (pi/2) &
            Waterfall.rotate (unit _z) (pi/2) &
            Waterfall.translate (V3 0 20 (44))
      in logo <> positionedHoop


main :: IO ()
main = do
    Waterfall.writeSTL 0.25 "kotlin-ornament.stl" ornament
