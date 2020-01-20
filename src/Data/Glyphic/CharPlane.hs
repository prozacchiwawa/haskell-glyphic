{-|

Module      : Data.Glyphic.CharPlane
Description : Simple plane container for text.
Copyright   : (c) Art Yerkes 2020
License     : BSD
Maintainer  : art.yerkes@gmail.com
Stability   : experimental

-}

module Data.Glyphic.CharPlane
  ( CharPlane (..)
  )
where

-- | Interface used by Data.Glyphic.Glyph to read ascii art drawings.
class CharPlane c where
  -- | Give 1 greater than the maximum X coordinate whose char /= ' ' on the plane.
  getMaxX :: c -> Int
  -- | Give 1 greater than the maximum Y coordainte whose char /= ' ' on the plane.
  getMaxY :: c -> Int
  -- | Retrieve a character from the plane or ' ' for any blank or not present characters.
  getCharAt :: c -> Int -> Int -> Char
