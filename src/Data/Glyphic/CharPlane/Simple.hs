{- |

Module      : Data.Glyphic.CharPlane.Simple
Description : Simple plane container for text.
Copyright   : (c) Art Yerkes 2020
License     : BSD
Maintainer  : art.yerkes@gmail.com
Stability   : experimental

-}
module Data.Glyphic.CharPlane.Simple
  ( SimpleCharPlane (..)
  , simpleCharPlaneFromString
  )
where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

import Data.Glyphic.CharPlane

-- | A simple object implementing CharPlane for use with Data.Glyph.
data SimpleCharPlane = SimpleCharPlane
  { maxX :: Int
  , rows :: Vector (Vector Char)
  }

instance CharPlane SimpleCharPlane where
  getMaxX SimpleCharPlane {..} = maxX
  getMaxY SimpleCharPlane {..} = Vector.length rows
  getCharAt = getCharOnSimplePlane

-- | Parse a multiline string into a SimpleCharPlane.
simpleCharPlaneFromString :: String -> SimpleCharPlane
simpleCharPlaneFromString str =
  let
    rows = List.lines str
    maxx = List.foldl' max 0 $ List.length <$> rows
  in
  SimpleCharPlane
    { maxX = maxx + 1
    , rows = Vector.fromList $ Vector.fromList <$> rows
    }

getCharOnSimplePlane :: SimpleCharPlane -> Int -> Int -> Char
getCharOnSimplePlane SimpleCharPlane {..} x y =
  Maybe.fromMaybe ' ' $ ((flip (Vector.!?)) x) =<< rows !? y
