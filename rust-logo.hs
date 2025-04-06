#!/usr/bin/env stack
{- stack script --resolver lts-23.16 
    --package linear
    --package lens
    --package waterfall-cad
    --package waterfall-cad-svg
    --extra-dep waterfall-cad-svg-0.5.0.0
    --extra-dep waterfall-cad-0.5.0.0
    --extra-dep opencascade-hs-0.5.0.0
-}
import qualified Waterfall
import Linear
import Control.Lens ((^.), (.~))
import Data.Function ((&))
import Waterfall.SVG (parsePath)
import Control.Monad (sequence)
import Data.Either (fromRight)

-- print two of these, one of them mirrored, and then glue them back to back
-- to get a more symetrical ornament

rustLogo :: Waterfall.Solid
rustLogo = 
    let makeShapes height = mconcat . fmap (Waterfall.prism height . Waterfall.makeShape) .  mconcat . fromRight (error "bad path") . traverse parsePath
        mainPath = [
            "m 2208.7012,1.308594 c -22.2251,0 -42.9328,12.000134 -54.541,31.716797 L 2038.6055,227.66211 c -18.1584,1.56667 -36.3421,3.46744 -54.4004,5.55078 L 1834.5801,65.830078 c -15.15,-16.912496 -37.6951,-24.472265 -59.4492,-20.009766 -21.8959,4.55 -39.8886,20.50938 -47.6387,42.296876 L 1650.8184,301.9375 c -17.75,5.325 -35.3373,10.94623 -52.8789,16.71289 L 1420.2559,185.28711 c -18.0667,-13.60834 -41.5839,-16.47903 -62.1504,-7.5332 -20.4959,8.8625 -35.2547,28.2638 -38.8047,51.10547 l -35.2539,225.47851 c -16.3084,8.82083 -32.4547,17.8918 -48.5547,27.1543 l -199.3418,-94.7793 c -20.2417,-9.64583 -43.87111,-7.65794 -62.3086,5.27539 -18.52083,12.85 -29.34166,34.85704 -28.5,58.01953 l 7.64648,228.8086 c -14.18749,11.83332 -28.22449,23.9095 -42.13281,36.13867 l -213.66406,-52.5332 c -21.71249,-5.27083 -44.45742,1.46236 -60.16993,17.8457 -15.81249,16.44166 -22.21588,40.12813 -17.13671,62.76562 l 50.32812,222.875 c -11.675,14.44583 -23.22969,29.04627 -34.55469,43.8379 l -219.39453,-7.9707 c -22.14583,-0.6542 -43.27956,10.404 -55.65039,29.7207 -12.39583,19.2208 -14.27187,43.8374 -5.04687,65.0332 l 90.89257,207.9297 c -8.82916,16.7 -17.5996,33.5333 -26.0371,50.625 l -216.17969,36.6875 c -21.9625,3.7 -40.47591,19.0257 -49.01758,40.459 -8.48749,21.4874 -5.73203,45.9708 7.23047,64.8125 l 125.01953,181.0761 c -6.92469,3.2107 -13.65918,6.777 -20.1875,10.6778 -1.925,-0.4458 -3.85405,-0.8788 -5.7832,-1.2246 0,0 -431.79605,211.9704 -185.683598,636.0703 2.320833,21.4042 491.035188,648.6184 628.285158,696.3808 89.82915,31.2584 -82.96436,-536.0009 -239.09766,-939.955 4.33741,-4.9291 9.70809,-10.0535 15.06446,-15.1836 66.60359,27.1643 177.5048,68.5786 328.20703,115.6933 27.91795,157.6245 121.79709,433.6802 437.0215,552.2539 -30.3992,199.25 119.984,411.2658 361.0137,492.9121 117.1707,39.6917 236.0531,41.8716 338.4531,13.3633 -153.7958,-152.1041 -258.3242,-339.1719 -258.3242,-339.1719 0,0 230.0132,30.3962 544.4589,38.1504 27.5417,-197.9625 -122.4178,-407.4678 -361.6718,-488.5136 -189.0767,-64.0449 -382.6191,-30.4723 -504.0879,72.2597 -95.6965,-19.22 -232.9038,-80.1672 -298.5586,-267.4707 567.8341,144.7252 1432.7814,261.8956 2316.3379,-4.4629 -53.7335,157.7027 -157.7689,227.1861 -246.7383,257.5235 -124.7258,-93.8452 -316.6373,-114.9075 -497.7813,-39.9571 -221.3582,91.5875 -352.6543,295.9667 -323.4335,482.6875 174.775,-2.7167 362.251,-14.0464 544.7343,-40.4921 0,0 -107.6556,174.0142 -266.0722,325.8808 106.525,27.4958 230.7622,19.8651 350.3164,-29.6015 228.0611,-94.3615 360.7016,-308.469 320.5117,-499.6172 278.988,-135.6897 356.873,-403.5639 378.6015,-546.4493 86.175,-33.6294 172.2929,-70.1913 258.043,-112.1132 12.7815,-2.2934 24.8129,-5.8805 36.3203,-10.2989 13.6288,7.9068 26.2931,15.7486 35.6992,23.6778 -94.5333,422.6332 -201.6787,1014.4928 -117.4746,970.2636 128.6542,-67.5791 518.9805,-760.3 518.1055,-781.8125 146.0233,-368.7316 -125.8046,-534.3817 -235.1426,-584.289 l 116.1426,-168.2578 c 13.0417,-18.8417 15.707,-43.3251 7.207,-64.8125 -8.5,-21.375 -27.0416,-36.709 -49,-40.459 l -216.1914,-36.6875 c -8.3667,-17.0917 -17.1506,-33.925 -25.959,-50.625 l 90.8223,-207.9297 c 9.3459,-21.1958 7.3697,-45.8124 -5.0137,-65.0332 -12.3374,-19.3167 -33.2919,-30.5999 -55.6211,-29.7207 l -219.3984,7.9707 c -11.3626,-14.79163 -22.9008,-29.4504 -34.6758,-43.8379 l 50.42,-222.875 c 5.1041,-22.63749 -1.3417,-46.32396 -17.0958,-62.76562 -15.675,-16.38334 -38.4532,-23.16237 -60.1074,-17.8457 l -213.7051,52.5332 c -13.8374,-12.22917 -27.9114,-24.30535 -42.1406,-36.13867 l 7.6992,-228.8086 c 0.825,-23.16249 -9.9698,-45.16953 -28.5156,-58.01953 -18.5251,-12.93333 -42.0558,-14.88789 -62.3183,-5.27539 l -199.3614,94.7793 c -16.0584,-9.20833 -32.2009,-18.33347 -48.5176,-27.1543 l -35.2285,-225.47851 c -3.5709,-22.84167 -18.2719,-42.24297 -38.8593,-51.10547 -20.5125,-8.90833 -44.0325,-6.03347 -62.045,7.5332 l -177.7461,133.36328 c -17.5,-5.81666 -35.0786,-11.38789 -52.8828,-16.71289 L 2689.9102,88.117188 c -7.7917,-21.787496 -25.7541,-37.796876 -47.6582,-42.296876 -21.8292,-4.462499 -44.2922,3.09727 -59.4297,20.009766 L 2433.2109,233.21289 c -18.0916,-2.08334 -36.1873,-3.98411 -54.3457,-5.55078 L 2263.3516,33.025391 C 2251.6433,13.308728 2230.9761,1.308594 2208.7012,1.308594 Z"
            ]
        lowerPath = 
            [ "m 735.32975,2008.4224 c 22.08336,-40.125 29.82503,-94.95 17.57085,-151.7709 -21.94168,-101.7334 -99.62508,-171.2918 -173.50847,-155.3543 -18.32501,3.95 -34.68753,12.8 -48.60837,25.4083 -1.175,-0.2416 -2.35,-0.4708 -3.52084,-0.6375 0,0 -234.55019,174.8002 -50.23754,474.6838 3.35417,15.5875 349.37945,453.492 434.62952,482.3462 55.79587,18.8833 -46.86254,-385.5212 -176.32515,-674.6756 z"
            , "m 2189.1809,1221.0426 c -548.4713,0 -1046.7842,65.9376 -1415.70945,173.396 v 917.9091 c 368.92525,107.4584 867.23815,173.3918 1415.70945,173.3918 627.738,0 1189.7301,-86.3709 1567.2263,-222.5085 V 1443.572 C 3378.911,1307.4219 2816.9189,1221.0426 2189.1809,1221.0426 Z"
            , "m 3780.9322,2051.8433 c -16.0709,-44.4292 -19.2875,-100.6417 -5.6708,-154.8418 24.375,-97.0376 93.4209,-150.8251 154.2209,-120.1376 15.075,7.6125 28.225,19.7125 39.1125,35.075 0.9959,0 1.9917,0 2.9792,0.067 0,0 185.6335,221.2752 13.8042,483.3462 -3.7292,14.8709 -318.8919,382.1587 -391.8462,393.8504 -47.75,7.65 62.0459,-374.992 187.4002,-637.3589 z"
            ]
        eyePaths = 
            [ "m 1639.0221,1509.8387 c 0,0 344.2003,-152.4001 438.1421,187.7585 0,0 98.4084,396.4753 -282.6544,418.8545 0,0 -485.9254,-93.5917 -155.4877,-606.613 z"
            , "m 2534.5979,1569.8304 c 0,0 200.6543,-219.7043 401.3128,0 0,0 157.6668,292.9669 0,439.4295 0,0 -257.9877,205.071 -401.3128,0 0,0 -171.996,-161.1334 0,-439.4295 z"
            ]
        glimmerPaths = 
            [ "m 1811.8973,1656.9513 c 0,88.2209 -52.0001,159.7752 -116.1543,159.7752 -64.1292,0 -116.1417,-71.5543 -116.1417,-159.7752 0,-88.2417 52.0125,-159.7751 116.1417,-159.7751 64.1542,0 116.1543,71.5334 116.1543,159.7751 z" 
            , "m 2766.9064,1651.3347 c 0,85.5542 -50.4292,154.9126 -112.6251,154.9126 -62.1834,0 -112.6126,-69.3584 -112.6126,-154.9126 0,-85.5501 50.4292,-154.9168 112.6126,-154.9168 62.1959,0 112.6251,69.3667 112.6251,154.9168 z"
            ]
        holePath = 
            --[ "m 2125.2095,330.14357 c 0,0 84.7448,-92.7905 169.4915,0 0,0 66.5894,123.7324 0,185.5898 0,0 -108.9592,86.6102 -169.4915,0 0,0 -72.6413,-68.0535 0,-185.5898 z"
            [ "m 2165.8185,188.94784 c 0,0 43.4815,-47.60962 86.9639,0 0,0 34.1662,63.48552 0,95.22376 0,0 -55.9055,44.43859 -86.9639,0 0,0 -37.2713,-34.91738 0,-95.22376 z"
            ]
        remove height x = (`Waterfall.difference` (Waterfall.translate (unit _z ^* height). makeShapes 20 $ x))
        add height x = (makeShapes height x <>)
    in makeShapes 4 mainPath
            & add 2 lowerPath
            & remove 3 eyePaths
            & add 3.5 glimmerPaths
            & remove (-1) holePath
            & Waterfall.scale (V3 0.01 0.01 1)

main :: IO ()
main = do
    Waterfall.writeSTL 0.1 "rust-ornament.stl" rustLogo
