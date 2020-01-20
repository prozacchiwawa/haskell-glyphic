{-|
Module      : Data.Glyphic.Glyph
Description : An ascii art drawing parser for interfacing between humans and programs.
Copyright   : (c) Art Yerkes (2020).
License     : BSD
Maintainer  : art.yerkes@gmail.com
Stability   : experimental

Data.Glyphic is a simple library which parses an ascii drawing of boxes, box contents and
connections between boxes as a collection of Glyphs and Nets that describe the drawing.

This is a valid drawing containing 2 actual glyphs and 3 nets.

@
           +--------------------+--+
           |                    |  |
     ,-----I-J--------.         |  |
     | id: mainbox    |  JUNK   |  |
     Z Test: box      |         |  |
     | Contains: foo, |  ,--.  ,Q--I---.
     `-----X-Y--------'  |  |  | D: M  |
           | |           \`__'  | id: A |
           | |                 \`---O--R'
           +-@---------------------+  |
             |                        |
             +------------------------+
        ,-------------.
        |             |
        |      ,------'
        |      |
        `------'
@

Connected nets exists between
- mainbox.I, A.Q, A.I
- mainbox.X, A.O
- mainbox.Y, A.R

Make glyphs like this:

@
    comma                  dot
    |                      |
    v                      v
    ,----------------------.
    |                      |
    |  Data: value         |  <-- glyphs contain text.
    |  Key: val            |
    |                      Q  <-- letters or numbers embedded in the frame have meaning.
    `------I-----M---------'
    ^                      ^
    |                      |
    backtick               single quote
@

-}
module Data.Glyphic.Glyph
  ( GlyphId (..)
  , GlyphContent (..)
  , GlyphVertex (..)
  , GlyphDrawing (..)
  , getDrawing
  )
where

import qualified Data.Char as Char
import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Data.Glyphic.CharPlane
import Data.Glyphic.Rect

data GlyphContent a = GlyphContent
  { gData :: a
  , gPorts :: Map Char (Int,Int)
  , gRect :: Rect
  }
  deriving (Show, Eq)

instance Functor GlyphContent where
  fmap f gc = gc { gData = f (gData gc) }

data GlyphId
  = GlyphId String
  | GlyphRect Rect
  deriving (Show, Eq, Ord)

data GlyphVertex = GlyphVertex
  { gToGlyph :: GlyphId
  , gConn :: Char
  }
  deriving (Show, Eq, Ord)

data GlyphDrawing a = GlyphDrawing
  { gGlyphs :: Map GlyphId (GlyphContent a)
  , gNets :: Map GlyphVertex (Set GlyphVertex)
  }
  deriving (Show, Eq)

instance Functor GlyphDrawing where
  fmap f gd =
    let
      newGlyphs = Map.map (fmap f) $ gGlyphs gd
    in
    gd { gGlyphs = newGlyphs }

instance Foldable GlyphDrawing where
  foldMap f gc = foldMap f (gData <$> gGlyphs gc)

-- | Find outside box commas that could be the upper left corners of glyphs.
commas :: CharPlane c => c -> [(Int,Int)]
commas plane =
  let
    xcoords :: [Int] = [0..(getMaxX plane - 1)]
    coords :: [(Int,Int)] =
      concat $ (\y -> (,y) <$> xcoords) <$> [0..(getMaxY plane - 1)]
  in
  filter (\(x,y) -> ',' == getCharAt plane x y) coords

-- | Measure the top or left line segments of a glyph starting at the given coordinate
-- in the char plane.  Return Nothing if there's not enough sensible geometry.
detectLineSegment
  :: CharPlane c
  => c
  -> ((Int,Int) -> (Int,Int))
  -> (Char -> Bool)
  -> (Char -> Bool)
  -> Int
  -> (Int,Int)
  -> Maybe (Int,Int)
detectLineSegment plane nextCoord validChar isEndChar minLength at =
  let
    coord@(x,y) = nextCoord at
    charThere = getCharAt plane x y
  in
  if validChar charThere then
    detectLineSegment plane nextCoord validChar isEndChar (minLength-1) coord
  else if minLength < 1 && isEndChar charThere then
    Just coord
  else
    Nothing

minWidth :: Int
minWidth = 5

minHeight :: Int
minHeight = 1

detectTopSegment :: CharPlane c => c -> (Int,Int) -> Maybe (Int,Int)
detectTopSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '.')
    minWidth
    at

detectLeftSegment :: CharPlane c => c -> (Int,Int) -> Maybe (Int,Int)
detectLeftSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '`')
    minHeight
    at

detectBottomSegment :: CharPlane c => c -> (Int,Int) -> Maybe (Int,Int)
detectBottomSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '\'')
    minWidth
    at

detectRightSegment :: CharPlane c => c -> (Int,Int) -> Maybe (Int,Int)
detectRightSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '\'')
    minHeight
    at

detectGlyph :: CharPlane c => c -> (Int,Int) -> Maybe Rect
detectGlyph plane at@(x,y) =
  let
    topSeg = detectTopSegment plane at
    leftSeg = detectLeftSegment plane at
    botSeg = leftSeg >>= detectBottomSegment plane
    rightSeg = topSeg >>= detectRightSegment plane
    matchTup = (topSeg, leftSeg, botSeg, rightSeg)
  in
  case matchTup of
    (Just tr, Just bl, Just br, Just brc@(ex,ey)) ->
      if br /= brc then
        Nothing
      else
        Just $ Rect x y (ex - x + 1) (ey - y + 1)
    _ -> Nothing

-- | Detect whether each location represents a glyph and ensure that subsequent coords within
-- the glyph are not tried.
detectGlyphs :: CharPlane c => c -> [(Int,Int)] -> [Rect]
detectGlyphs _ [] = []
detectGlyphs plane (coord:coords) =
  let
    detected = detectGlyph plane coord
    following = Maybe.maybe id (\r -> filter (\(x,y) -> not $ coordInRect r x y)) detected
  in
  Maybe.maybeToList detected ++ detectGlyphs plane (following coords)

trimEnd :: String -> String
trimEnd str = trimEndStr 0 str
  where
    trimEndStr :: Int -> String -> String
    trimEndStr _ [] = []
    trimEndStr n (' ':tl) = trimEndStr (n+1) tl
    trimEndStr n (hd:tl) = take n (repeat ' ') ++ hd:(trimEndStr 0 tl)

leastColumn :: String -> Maybe Int
leastColumn [] = Nothing
leastColumn (' ':tl) = ((+) 1) <$> leastColumn tl
leastColumn (_:tl) = Just 0

-- | Get glyph text
-- Returns a little string formatted as though it was a standalone text file containing the
-- trimmed contents of the rectangle.
getGlyphText :: (CharPlane c) => c -> Rect -> String
getGlyphText plane Rect {..} =
  let
    rowNums = [y+1..(y+h)-2]
    colNums = [x+1..(x+w)-2]
    rawRows = (\i -> (\j -> getCharAt plane j i) <$> colNums) <$> rowNums
    leastCol = List.foldl' min (getMaxX plane) $ Maybe.catMaybes $ leastColumn <$> rawRows
    rows = (drop leastCol . trimEnd) <$> rawRows
  in
  List.intercalate "\n" rows

coordsOnRect :: Rect -> [(Int,Int)]
coordsOnRect Rect {..} =
  topCoords x w ++ leftCoords y h ++ rightCoords y h ++ botCoords x w
  where
    topCoords j l = if l == 0 then [] else (j,y):topCoords (j+1) (l-1)
    botCoords j l = if l == 0 then [] else (j,y+h):botCoords (j+1) (l-1)
    leftCoords i l = if l == 0 then [] else (x,i):leftCoords (i+1) (l-1)
    rightCoords i l = if l == 0 then [] else (x+w,i):rightCoords (i+1) (l-1)

getGlyphPorts :: CharPlane c => c -> Rect -> Map Char (Int,Int)
getGlyphPorts plane Rect {..} =
  let
    rectPoints = coordsOnRect (Rect x y (w-1) (h-1))
    pairs = (\(j,i) -> (getCharAt plane j i, (j,i))) <$> rectPoints
    onlySymbols = filter (Char.isAlphaNum . fst) pairs
  in
  Map.fromList onlySymbols

colonSplit :: String -> (String,String)
colonSplit str =
  let
    name = trimEnd $ List.takeWhile ((/=) ':') str
    val = trimEnd $ dropWhile ((==) ':') $ dropWhile ((/=) ':') str
  in
  (name, val)

glyphTextToBindings :: String -> Map String String
glyphTextToBindings str =
  Map.fromList $ colonSplit <$> lines str

-- | Given a char plane, find a path from c1 to c2 through |, -, + and @ chars.
-- The path must change direction in a + char, and must not change direction in an @ char.
findPath :: CharPlane c => c -> Set (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
findPath plane rectpts pt other =
  runOne Set.empty $ Set.singleton pt
  where
    runOne :: Set (Int,Int) -> Set (Int,Int) -> Bool
    runOne visited forefront =
      let
        nextStep = advance visited forefront
      in
      if null nextStep then -- No remaining steps to be taken
        False
      else
        if Set.member other nextStep then
          True
        else
          runOne (Set.union visited forefront) nextStep

    advance :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
    advance visited forefront =
      Set.difference
        (Set.unions $ validNeighbors <$> Set.toList forefront)
        (Set.unions $ [visited, forefront, Set.difference rectpts (Set.singleton other)])

    validNeighbors :: (Int,Int) -> Set (Int,Int)
    validNeighbors (x,y) =
      let
        leftN = nextList '-' (\(j,i) -> (j-1,i))
        rightN = nextList '-' (\(j,i) -> (j+1,i))
        topN = nextList '|' (\(j,i) -> (j,i-1))
        botN = nextList '|' (\(j,i) -> (j,i+1))

        nextList want nextStep =
          let
            toward@(tx,ty) = nextStep (x,y)
            thatChar = getCharAt plane tx ty
            crawl =
              if thatChar == want then
                Set.singleton toward
              else if thatChar == '@' then
                let
                  following@(fx,fy) = nextStep toward
                in
                if getCharAt plane fx fy == want then
                  Set.singleton following
                else
                  Set.empty
              else if thatChar == '+' then
                Set.filter ((/=) (x,y)) $ validNeighbors toward
              else
                Set.empty
          in
          if toward == other then
            Set.singleton toward
          else
            crawl

      in
      Set.unions [leftN, rightN, topN, botN]

createNets
  :: (Eq a, CharPlane c)
  => c
  -> [((Int,Int), GlyphContent a, GlyphVertex)]
  -> Map GlyphVertex (Set GlyphVertex)
createNets plane inputs =
  List.foldl'
    (\vmap inp@(pt,GlyphContent {..},vt) ->
       let
         insetRect = Rect (x gRect) (y gRect) ((w gRect) - 1) ((h gRect) - 1)
         inotme = filter ((/=) inp) inputs
         matches =
           filter
             (\(otherpt,_,_) ->
                findPath plane (Set.fromList $ coordsOnRect insetRect) pt otherpt
             )
             inotme
       in
       if not $ null matches then
         Map.insert vt (Set.fromList $ (\(_,_,v) -> v) <$> matches) vmap
       else
         vmap
    )
    Map.empty
    inputs

{- | Parse text into a drawing.

getDrawing parser getId plane -> Either [e] (GlyphDrawing v)

- parser produces Either e v from String, and will be applied to the trimmed contents of the
  text in the glyph boxes.  The text is trimmed on the right, since the box needs spaces to
  secure the right edge, but is not trimmed on the left, allowing whitespace significant
  languages, such as yaml to be parsed.  All parse errors are returned on failure.

- getId produces Maybe String, which is present if the text in the glyph provides a string by
  which the glyph can be identified in the drawing.  If none is provided, the glyph is uniquely
  identified in the drawing by its rectangle, which can't overlap any other glyph.

- c is an object implementing CharPlane, which can be queried for character content.
-}
getDrawing
  :: forall c e v.
     (Eq v, CharPlane c)
  => (String -> Either e v)
  -> (v -> Maybe String)
  -> c
  -> Either [e] (GlyphDrawing v)
getDrawing decoder getId plane =
  let
    cs = commas plane
    glyphRects = detectGlyphs plane cs

    rawGlyphs =
      (\r ->
         let
           glyphYaml :: Either e v = decoder $ getGlyphText plane r
         in
         (\gy -> (r, gy, getGlyphPorts plane r)) <$> glyphYaml
      ) <$> glyphRects

    eitherLabeledGlyphs :: [Either e (GlyphId, GlyphContent v)] =
      (fmap
        (\(rect, bindings, ports) ->
           let
             gid =
               maybe
                 (GlyphRect rect)
                 GlyphId
                 (getId bindings)
           in
           (gid, GlyphContent bindings ports rect)
        )
      ) <$> rawGlyphs

    (lgErrors, labeledGlyphs) = partitionEithers eitherLabeledGlyphs

    glyphPorts :: [((Int,Int), GlyphContent v, GlyphVertex)] =
      concat $
        (\(gid, gc@GlyphContent {..}) ->
           let
             portPts = Map.toList gPorts
           in
           (\(ch,pt) -> (pt,gc,GlyphVertex gid ch)) <$> portPts
        ) <$> labeledGlyphs

    glyphNets = createNets plane glyphPorts
  in
  if null lgErrors then
    Right $ GlyphDrawing
      { gGlyphs = Map.fromList labeledGlyphs
      , gNets = glyphNets
      }
  else
    Left lgErrors
