-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- Tetris (SHAPE POSITION, FALLING SHAPE) WELL [INFINITE RANDOM SHAPES]

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (v,s) w r) = prop_Shape s && wellSize == shapeSize w


-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls shape = botLeftWalls (topRightWalls shape)

topRightWalls (S rows) = S (rightWalls (topWalls rows))
  where
    rightWalls rows =
          map ([Just Black]++) rows
    topWalls rows = 
          [replicate (length (head rows)) (Just Black)] ++ rows

botLeftWalls (S rows) = S (leftWalls (botWalls rows))
  where
    leftWalls rows =
      map (++ [Just Black]) rows
    botWalls rows = 
      rows ++ [replicate (length (head rows)) (Just Black)]

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls ((shiftShape v p) `combine` w)


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris a t | a == Tick = tick t
stepTetris _ t = Just (0,t)

-- Moves the tetris piece according to the vector v1
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2,p) w r) = Tetris (v1 `vAdd` v2, p) w r

-- Gets called when a certain amount of time has passed
-- Calls the function move and moves the piece one step down
tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, move (0, 1) t)
