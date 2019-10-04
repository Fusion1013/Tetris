-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck
  
-- * Shapes
  
type Square = Maybe Colour
  
data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)
  
-- | A geometric rows is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
  
data Shape = S [Row] deriving (Eq)
type Row = [Square]
  
rows :: Shape -> [Row]
rows (S rs) = rs
  
-- * Showing shapes
  
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)
  
instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r
  
  
-- * The shapes used in the Tetris game
  
-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
    where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
                "I",
                "I",
                "I"],
              [" J",
                " J",
                "JJ"],
              [" T",
                "TT",
                " T"],
              ["OO",
                "OO"],
              [" Z",
                "ZZ",
                "Z "],
              ["LL",
                " L",
                " L"],
              ["S ",
                "SS",
                " S"]]
  
-- * Some simple functions
  
-- ** A01
-- Creates an empty rows with the width and height specified
emptyShape :: (Int,Int) -> Shape
emptyShape (width, height) = S (replicate height (createRow width))
  
-- Creates a row with the given length
createRow :: Int -> Row
createRow width = replicate width Nothing
  
-- ** A02
-- | The size (width and height) of a rows
shapeSize :: Shape -> (Int,Int)
shapeSize (S rows) = (length (head rows), length rows)
  
-- ** A03
-- | Count how many non-empty squares a rows contains
blockCount :: Shape -> Int
blockCount (S rows) = length (concat rows) - emptySquares rows
  where emptySquares rows = (length . filter (==Nothing)) (concat rows)
-- * The Shape invariant
  
-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S rows) | null rows = False
                    | isSameLength rows = True
                    | otherwise = False
  where isSameLength rows = all (==length(head rows)) (map length rows)

  
-- * Test data generators
-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]
  
instance Arbitrary Colour where
  arbitrary = rColour
  
-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes
  
instance Arbitrary Shape where
  arbitrary = rShape
  
-- * Transforming shapes
  
-- ** A07
-- | Rotate a rows 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S rows) = S (map reverse (transpose rows))
  
-- ** A08
-- | shiftShape adds empty squares above and to the left of the rows
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (a, b) (S rows) = S (shiftRight a (shiftDown b rows))
  where
    shiftRight amount =
          map (replicate amount Nothing ++ )
    shiftDown amount rows = 
          replicate amount (replicate (length (head rows)) Nothing) ++ rows

-- ** A09
-- | padShape adds empty sqaure below and to the right of the rows
padShape :: (Int,Int) -> Shape -> Shape
padShape (a, b) (S rows) = S (shiftLeft a (shiftUp b rows))
  where
    shiftLeft amount =
      map (++ replicate amount Nothing)
    shiftUp amount rows = 
      rows ++ replicate amount (replicate (length (head rows)) Nothing)

-- ** A10
-- | pad a rows to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (a, b) shape = padShape (a - width shape, b - height shape) shape
  where width  shape = fst (shapeSize shape)
        height shape = snd (shapeSize shape)
  
-- * Comparing and combining shapes
  
-- ** B01
  
-- | Test if two shapes overlap

overlaps :: Shape -> Shape -> Bool
(S r1) `overlaps` (S r2) = r1 `overlaps'` r2

(x:xs) `overlaps'` (y:ys) | x `rowsOverlap` y = True
                          | otherwise         = xs `overlaps'` ys
_      `overlaps'` _                          = False

rowsOverlap :: Row -> Row -> Bool
(x:xs) `rowsOverlap` (y:ys) | isNothing x || isNothing y = xs `rowsOverlap` ys
                            | otherwise                  = True
_      `rowsOverlap` _                                   = False

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S s1) (S s2) = S (zipWith (zipWith f) s1 s2)

blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2  
  where clash :: Square -> Square -> Square 
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black


-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting rows will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = blackClashes (pad s1 s2) (pad s2 s1)
  where 
    pad s1 s2 = padShapeTo (width s1 s2, height s1 s2) s1
    height (S s1) (S s2) = max (length s1) (length s2)
    width  (S s1) (S s2) = max (length (head s1)) (length (head s2))

shape1 = allShapes !! 1
shape2 = shiftShape (2,0) (allShapes !! 2)
