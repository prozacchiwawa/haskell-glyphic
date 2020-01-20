module Data.Glyphic.Rect
  ( Rect (..)
  , coordInRect
  )
where

data Rect =
  Rect
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }
    deriving (Show, Eq, Ord)

coordInRect :: Rect -> Int -> Int -> Bool
coordInRect Rect {..} j i = j >= x && j < x + w && i >= y && i < y + h
