import Control.Exception
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Hspec
import Test.Hspec.Expectations

import Data.Glyphic.CharPlane
import Data.Glyphic.CharPlane.Simple
import Data.Glyphic.Glyph
import Data.Glyphic.Rect

main :: IO ()
main = hspec $ do
  describe "Data.Glyphic" $ do
    it "should parse a drawing" $ do
      let
        testGraph =
          [ "       +--------------------+--+"
          , "       |                    |  |"
          , " ,-----I-J--------.         |  |"
          , " | id: mainbox    |  JUNK   |  |"
          , " Z Test: box      |         |  |"
          , " | Contains: foo, |  ,--.  ,Q--I---."
          , " `-----X-Y--------'  |  |  | D: M  |"
          , "       | |           `__'  | id: A |"
          , "       | |                 `---O--R'"
          , "       +-@---------------------+  |"
          , "         |                        |"
          , "         +------------------------+"
          , "    ,-------------."
          , "    |             |"
          , "    |      ,------'"
          , "    |      |"
          , "    `------'"
          ]

        rectA = Rect 1 2 18 5
        rectB = Rect 27 5 9 4
        expectedGlyphs =
          [ rectA
          , rectB
          ]

        expectedTexts =
          [ "id: mainbox\nTest: box\nContains: foo,"
          , "D: M\nid: A"
          ]

        plane = simpleCharPlaneFromString $ List.intercalate "\n" testGraph
        drawing = getDrawing Right (const Nothing) plane

        identifiedGlyphs :: Either String [GlyphId] =
          (Map.keys . gGlyphs) <$> drawing
        extractedTexts :: Either String [String] =
          (fmap gData) <$> (Map.elems . gGlyphs) <$> drawing
        extractedNets :: Either String (Map GlyphVertex (Set GlyphVertex)) =
          gNets <$> drawing

        vertexA_I = GlyphVertex (GlyphRect rectA) 'I'
        vertexA_X = GlyphVertex (GlyphRect rectA) 'X'
        vertexA_Y = GlyphVertex (GlyphRect rectA) 'Y'
        vertexB_Q = GlyphVertex (GlyphRect rectB) 'Q'
        vertexB_I = GlyphVertex (GlyphRect rectB) 'I'
        vertexB_O = GlyphVertex (GlyphRect rectB) 'O'
        vertexB_R = GlyphVertex (GlyphRect rectB) 'R'

        expectedNets :: Map GlyphVertex (Set GlyphVertex) =
          Map.fromList
            [ (vertexA_I, Set.fromList [vertexB_Q, vertexB_I])
            , (vertexA_X, Set.singleton vertexB_O)
            , (vertexA_Y, Set.singleton vertexB_R)
            , (vertexB_I, Set.fromList [vertexA_I, vertexB_Q])
            , (vertexB_Q, Set.fromList [vertexA_I, vertexB_I])
            , (vertexB_O, Set.singleton vertexA_X)
            , (vertexB_R, Set.singleton vertexA_Y)
            ]

      identifiedGlyphs `shouldBe` Right (GlyphRect <$> expectedGlyphs)
      extractedTexts `shouldBe` Right expectedTexts
      extractedNets `shouldBe` Right expectedNets
