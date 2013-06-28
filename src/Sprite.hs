module Sprite
  ( makeDisplayListFromImage
  ) where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Data.List     ((\\), nub)

import qualified Codec.Picture             as JP
import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

makeDisplayListFromImage :: FilePath -> IO GL.DisplayList
makeDisplayListFromImage fp = do
    e <- readRImage fp
    case e of
      Left _ -> error "oh no"  -- this is not a real API yet
      Right rimg ->
          GL.defineNewList GL.Compile $ do
              GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
              drawRImage rimg

--------------------------------------------------------------------------------

drawRImage :: RImage -> IO ()
drawRImage rimg = do
    GL.shadeModel GL.$= GL.Flat
    normal 0 0 1
    forM_ (rImageRGs rimg) $ \rg -> do
        let rcolor = rgColor rg
            rs = realToFrac (colorR rcolor) / 255 :: GL.GLfloat
            gs = realToFrac (colorG rcolor) / 255 :: GL.GLfloat
            bs = realToFrac (colorB rcolor) / 255 :: GL.GLfloat
        GL.color $ GL.Color4 rs gs bs 1
        GL.renderPrimitive GL.Quads $
          forM_ (rgRs rg) $ \r -> do
              let rrow = realToFrac $ rRow    r
                  rcol = realToFrac $ rColumn r
                  rw   = realToFrac $ rWidth  r
                  rh   = realToFrac $ rHeight r
                  imgw = realToFrac $ rImageWidth rimg
                  imgh = realToFrac $ rImageHeight rimg
                  imgwd2 = imgw / 2
                  imghd2 = imgh / 2
                  project row col =
                      let x = (col - imgwd2)
                          y = (imghd2 - row)
                      in (x, y)
                  (ulx, uly) = project  rrow      rcol
                  (urx, ury) = project  rrow     (rcol+rw)
                  (lrx, lry) = project (rrow+rh) (rcol+rw)
                  (llx, lly) = project (rrow+rh)  rcol
              vertex ulx uly 0
              vertex urx ury 0
              vertex lrx lry 0
              vertex llx lly 0

vertex :: Double -> Double -> Double -> IO ()
vertex x y z =
    GL.vertex $ mkVertex x y z

mkVertex :: Double -> Double -> Double -> GL.Vertex3 GL.GLfloat
mkVertex x y z =
    GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

normal :: Double -> Double -> Double -> IO ()
normal x y z =
    GL.normal $ mkNormal x y z

mkNormal :: Double -> Double -> Double -> GL.Normal3 GL.GLfloat
mkNormal x y z =
    GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

--------------------------------------------------------------------------------

data Color = Color
    { colorR :: Int
    , colorG :: Int
    , colorB :: Int
    , colorA :: Int
    } deriving Eq

data Image = Image
    { imageWidth  :: Int
    , imageHeight :: Int
    , imageColors :: [Color]
    }

--------------------------------------------------------------------------------

data RImage = RImage
    { rImageWidth  :: Int
    , rImageHeight :: Int
    , rImageRGs    :: [RG]
    }

data RG = RG
    { rgColor :: Color
    , rgRs    :: [R]
    }

data R = R
    { rRow    :: Int
    , rColumn :: Int
    , rWidth  :: Int
    , rHeight :: Int
    }

--------------------------------------------------------------------------------

readRImage :: FilePath -> IO (Either String RImage)
readRImage fp = do
    e <- JP.readImage fp
    return $ case e of
      Left  s  -> Left s
      Right di -> Right $ makeRImage di

makeRImage :: JP.DynamicImage -> RImage
makeRImage di =
    let image = toImage di
        uniqueColors = getUniqueColors image
        rgs = map (getRG image) uniqueColors
    in RImage (imageWidth image) (imageHeight image) rgs

toImage :: JP.DynamicImage -> Image
toImage (JP.ImageRGBA8 i) =
    let w = JP.imageWidth  i
        h = JP.imageHeight i
        ps = map (\(row, col) -> JP.pixelAt i col row) (genCoords w h)
        cs = map toColor ps
    in Image w h cs
  where
    toColor (JP.PixelRGBA8 r g b a) =
        let r' = fromIntegral r
            g' = fromIntegral g
            b' = fromIntegral b
            a' = fromIntegral a
        in Color r' g' b' a'

getUniqueColors :: Image -> [Color]
getUniqueColors image =
    nub $ filter isOpaque (imageColors image)

isOpaque :: Color -> Bool
isOpaque c = colorA c == 255

getRG :: Image -> Color -> RG
getRG image color =
    let w      = imageWidth  image
        h      = imageHeight image
        colors = imageColors image
        -- coordinates of all pixels of the given color
        coords = map fst $ filter (\(_, c) -> c == color) $ zip (genCoords w h) colors
        rs     = getRs coords
    in RG color rs

genCoords :: Int -> Int -> [(Int, Int)]
genCoords w h =
    [(row, col) | row <- [0..pred h], col <- [0..pred w]]

getRs :: [(Int, Int)] -> [R]
getRs =
    help []
  where
    help rs [] =
        reverse rs
    help rs ((row, col):cs) =
        let (cov, r) = growHoriz [] 1
        in help (r:rs) (cs\\cov)
      where
        growHoriz hcov w =
            let ctr = (row, col+w)
            in if ctr `elem` cs
              then growHoriz (ctr:hcov) (succ w)
              else growVert hcov 1
          where
            growVert vcov h =
                let ncs = map (\n -> (row+h, n)) [col..col+w-1]
                in if all (`elem` cs) ncs
                     then growVert (vcov++ncs) (succ h)
                     else (vcov, R row col w h)
