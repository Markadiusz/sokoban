-- Arkadiusz Czarkowski
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.String

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

drawSingleChar :: Char -> Picture
drawSingleChar c d x y = if x == 0 && y == 0 then c else d x y

drawStringHelper :: Integer -> String -> Picture
drawStringHelper x [] = id
drawStringHelper x (h:t) = pictures [translated x 0 $ drawSingleChar h, drawStringHelper (x + 1) t]

drawString :: String -> Picture
drawString s = drawStringHelper (-((listLength s - 1) `div` 2)) s

wall, ground, storage, box, empty :: Picture
wall    = drawSingleChar '#'
ground  = drawSingleChar '.'
storage = drawSingleChar 'x'
box     = drawSingleChar 'o'
empty   = drawSingleChar ' '

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = empty

data Direction = R | U | L | D deriving Eq

data Coord = C { cX :: Integer, cY :: Integer } deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] c = c
moveCoords (h:t) c = adjacentCoord h $ moveCoords t c

player :: Direction -> Picture
player U = drawSingleChar '^'
player L = drawSingleChar '<'
player D = drawSingleChar 'v'
player R = drawSingleChar '>'

data State = S {
  stPlayer :: Coord,
  stDir    :: Direction,
  stBoxes  :: [Coord],
  stLevel :: Integer,
  stMoveCount :: Integer
} deriving Eq

initialLevel :: Integer
initialLevel = 0

initialPlayer :: Coord
initialPlayer = getPlayer $ nth mazes initialLevel

initialDir :: Direction
initialDir = U

isBox :: Tile -> Bool
isBox Box = True
isBox _   = False

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _       = False

isEmpty :: Tile -> Bool
isEmpty Ground = True
isEmpty Storage = True
isEmpty _ = False

getBoxes :: Maze -> [Coord]
getBoxes maze = getTile maze Box

nextLevel :: State -> State
nextLevel s =
  if 1 + stLevel s == listLength mazes
  then
    s {stLevel = 1 + stLevel s}
  else
      s {
        stPlayer    = getPlayer $ nth mazes $ 1 + stLevel s,
        stDir       = initialDir,
        stBoxes     = getBoxes $ nth mazes $ 1 + stLevel s,
        stLevel     = 1 + stLevel s,
        stMoveCount = 0
      }

initialState :: State
initialState = S {
    stPlayer = initialPlayer,
    stDir    = initialDir,
    stBoxes  = getBoxes $ nth mazes initialLevel,
    stLevel  = initialLevel,
    stMoveCount = 0
    }

removeBoxes :: Maze -> Maze
removeBoxes (Maze initial maze) =
    Maze initial (\c ->
      if elemList c $ getBoxes (Maze initial maze)
      then Ground
      else maze c
    )

data Maze = Maze Coord (Coord -> Tile)

addBoxes :: [Coord] -> Maze -> Maze
addBoxes l (Maze initial maze) = Maze initial (\c -> if elemList c l then Box else maze c)

getMaze :: Maze -> (Coord -> Tile)
getMaze (Maze i m) = m

getPlayer :: Maze -> Coord
getPlayer (Maze i m) = i

currentMaze :: State -> Maze
currentMaze s = addBoxes (stBoxes s) $ removeBoxes $ nth mazes $ stLevel s

translated :: Integer -> Integer -> Picture -> Picture
translated tx ty p d x y = p (\dx dy -> d (dx + tx) (dy + ty)) (x - tx) (y - ty)

drawTileAt :: State -> Coord -> Picture
drawTileAt s (C r c) = translated r c (drawTile $ (getMaze $ currentMaze s) $ C r c)

pictures :: [Picture] -> Picture
pictures [] d    = d
pictures (h:t) d = h $ pictures t d

pictureOfMaze :: State -> Picture
pictureOfMaze s = pictures [drawTileAt s $ C x y | x <- [-10..10], y <- [-10..10]]

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated x y pic

drawState :: State -> Picture
drawState s = pictures [atCoord (stPlayer s) (player $ stDir s), pictureOfMaze s]

moveBox :: Direction -> Coord -> Coord -> Coord
moveBox d c b
  | b == c = adjacentCoord d b
  | otherwise = b

updateBoxes :: Direction -> Coord -> [Coord] -> [Coord]
updateBoxes d c l = map (moveBox d c) l

tryMove :: State -> Direction -> State
tryMove s d = let {
  newPlayerCoord = adjacentCoord d $ stPlayer s;
  newBoxCoord = adjacentCoord d newPlayerCoord;
  } in case (getMaze $ currentMaze s) newPlayerCoord of
    Ground  -> s { stPlayer = newPlayerCoord }
    Storage -> s { stPlayer = newPlayerCoord }
    Box     -> case (getMaze $ currentMaze s) newBoxCoord of
      Ground  -> s {stBoxes = updateBoxes d newPlayerCoord (stBoxes s), stPlayer = newPlayerCoord}
      Storage -> s {stBoxes = updateBoxes d newPlayerCoord (stBoxes s), stPlayer = newPlayerCoord}
      _       -> s
    _       -> s

handleEvent1 :: Event -> State -> State
handleEvent1 (KeyPress key) s
  | key == "d" = tryMove s R
  | key == "w" = tryMove s U
  | key == "a" = tryMove s L
  | key == "s" = tryMove s D
handleEvent1 _ s   = s

handleEvent2 :: Event -> State -> State
handleEvent2 (KeyPress key) s
  | key == "d" = handleEvent1 (KeyPress key) $ s {stDir = R, stMoveCount = 1 + stMoveCount s}
  | key == "w" = handleEvent1 (KeyPress key) $ s {stDir = U, stMoveCount = 1 + stMoveCount s}
  | key == "a" = handleEvent1 (KeyPress key) $ s {stDir = L, stMoveCount = 1 + stMoveCount s}
  | key == "s" = handleEvent1 (KeyPress key) $ s {stDir = D, stMoveCount = 1 + stMoveCount s}
handleEvent2 e s   = handleEvent1 e s

startScreen :: Picture
startScreen = drawString "Sokoban!"

data SSState world = StartScreen | Running world deriving Eq

data Activity world = Activity {
        actState  :: world,
        actHandle :: (Event -> world -> world),
        actDraw   :: (world -> Screen)
        }

data Event = KeyPress String deriving Eq
type Screen = String

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "\ESC" = state0
        handle' e s = handle e s

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = pictureToScreen startScreen
    draw' (Running s) = draw s

turnOffBuffering :: IO()
turnOffBuffering = hSetBuffering stdin NoBuffering

clearScreen :: IO()
clearScreen = putStr "\ESCc"

write :: Screen -> IO()
write s = putStr s

runActivity :: Activity s -> IO()
runActivity (Activity state handle draw) =
    do
        turnOffBuffering
        clearScreen
        write $ draw state
        go state
        where go currentState = do
                input <- getChar
                let newState = handle (KeyPress [input]) currentState
                clearScreen
                write $ draw newState
                go newState

isWinning :: State -> Bool
isWinning s = andList $ mapList isStorage $ mapList (getMaze $ nth mazes $ stLevel s) $ stBoxes s

isFinished :: State -> Bool
isFinished s = stLevel s == listLength mazes

endScreen :: Picture
endScreen = drawString "Gra ukończona, gratulacje!"

completedLevel :: Integer -> Picture
completedLevel moves = drawString $ "Poziom ukończony, liczba ruchów: " ++ show moves

screenWidth, screenHeight :: Integer
screenWidth  = 41
screenHeight = 23

emptyScreen :: DrawFun
emptyScreen x y = ' '

pictureToScreenHelper :: Integer -> Integer -> Picture -> Screen
pictureToScreenHelper x y p =
    if y == screenHeight
    then
        ""
    else if x == screenWidth
    then
        '\n':(pictureToScreenHelper 0 (y + 1) p)
    else
        let {px = x - (screenWidth + 1) `div` 2 + 1; py = (screenHeight + 1) `div` 2 - y - 1} in
        (p emptyScreen px py):(pictureToScreenHelper (x + 1) y p)

pictureToScreen :: Picture -> Screen
pictureToScreen p = pictureToScreenHelper 0 0 p

draw :: State -> Screen
draw s
  | isFinished s = pictureToScreen endScreen
  | isWinning s  = pictureToScreen $ completedLevel $ stMoveCount s
  | otherwise    = pictureToScreen $ drawState s

handleEvent :: Event -> State -> State
handleEvent e s
  | isFinished s = s
  | e == (KeyPress "n") = nextLevel s
  | isWinning s =
    if e == (KeyPress " ")
    then
      nextLevel s
    else s
  | otherwise = handleEvent2 e s

interaction :: Activity State
interaction = Activity {
    actState = initialState,
    actHandle = handleEvent,
    actDraw = draw
    }

game :: Activity (WithUndo (SSState State))
game = resettable $ withUndo $ withStartScreen interaction

etap5 :: IO()
etap5 = runActivity game

main :: IO()
main = etap5

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

mazes :: [Maze]
mazes = [
  Maze (C (-2) (-2)) easy_testMaze_GN,
  Maze (C 1 (-1))    easy_spiralMaze_DM,
  Maze (C 0 0)       easy_decoratedMaze_BS,
  Maze (C 1 1)       medium_maze4_GN,
  Maze (C 0 1)       medium_maze3_GN,
  Maze (C 1 (-3))    hard_maze2_GN
  ]

easy_testMaze_GN :: Coord -> Tile
easy_testMaze_GN (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

hard_maze2_GN :: Coord -> Tile
hard_maze2_GN (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

medium_maze3_GN :: Coord -> Tile
medium_maze3_GN (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

medium_maze4_GN :: Coord -> Tile
medium_maze4_GN (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

easy_spiralMaze_DM :: Coord -> Tile
easy_spiralMaze_DM (C x y)
  | abs x >  4 || abs y > 4      = Blank
  | abs x == 4                   = Wall
  | abs y == 4                   = Wall
  | x ==  2 && y <=  1           = Wall
  | x >= -1 && x <=  2 && y == 1 = Wall
  | x == -1 && y >= -1 && y <= 1 = Wall
  | x ==  0 && y == -1           = Box
  | x ==  3 && y == -3           = Storage
  | otherwise                    = Ground

easy_decoratedMaze_BS :: Coord -> Tile
easy_decoratedMaze_BS (C x y)
  | abs x > 6  || abs y > 4          = Blank
  | abs x == 6 || abs y == 4         = Wall
  | abs x + abs y > 6                = Box
  | abs x == 4 || abs y == 2         = Wall
  | elem (x, y) [(-3, 1), (3, -1)]   = Box
  | x == 0     && abs y == 1         = Box
  | elem (x, y) [(-3, -1), (3, 1)]   = Storage
  | elem (x, y) [(-2, 1), (2, -1)]   = Wall
  | abs x > 1 && abs x < 4 && y == 0 = Wall
  | otherwise                        = Ground

badMazes :: [Maze]
badMazes = [
  Maze (C (-2) (-2)) badTestMaze_BS,
  Maze (C 1 (-1))    cutOffStorageMaze_DM,
  Maze (C (-1) 0)    holeInTheWallMaze_BS
  ]

badTestMaze_BS :: Coord -> Tile
badTestMaze_BS (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2                  = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

cutOffStorageMaze_DM :: Coord -> Tile
cutOffStorageMaze_DM (C x y)
  | abs x > 7 || abs y > 7                            = Blank
  | abs x == 7                                        = Wall
  | abs y == 7                                        = Wall
  | x >= 4 && y == 4                                  = Wall
  | x == 4 && y >= 4                                  = Wall
  | x >= 5 && y >= 5                                  = Storage
  | elem (x, y) [(-6, 6), (-6, -6), (6, -6), (6, -5)] = Storage
  | x == 0 && elem y [-4 .. 2]                        = Box
  | otherwise                                         = Ground

holeInTheWallMaze_BS :: Coord -> Tile
holeInTheWallMaze_BS (C x y)
  | abs x > 2 || abs y > 1   = Blank
  | x == -2 && y == 0        = Ground
  | abs x == 2 || abs y == 1 = Wall
  | x == 0 && y == 0         = Box
  | x == 1 && y == 0         = Storage
  | otherwise                = Ground

elemList :: Eq a => a -> [a] -> Bool
elemList e [] = False
elemList e (h:t) = e == h || elemList e t

appendList :: [a] -> [a] -> [a]
appendList [] b = b
appendList (h:t) b = h:(appendList t b)

listLength :: [a] -> Integer
listLength [] = 0
listLength (h:t) = 1 + listLength t

filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (h:t) =
  if f h
  then h:(filterList f t)
  else filterList f t

nth :: [a] -> Integer -> a
nth (h:t) 0 = h
nth (h:t) x = nth t (x - 1)

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (h:t) = (f h):(mapList f t)

andList :: [Bool] -> Bool
andList [] = True
andList (h:t) = h && andList t

allList :: (a -> Bool) -> [a] -> Bool
allList f [] = True
allList f (h:t) = f h && allList f t

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f acc [] = acc
foldList f acc (h:t) = foldList f (f h acc) t

getAllReachable :: Eq a => a -> (a -> [a]) -> [a] -> [a]
getAllReachable current neighbours visited =
  if elemList current visited
  then
    visited
  else
    foldList
      (\neighbour accVisited -> getAllReachable neighbour neighbours accVisited)
      (current:visited)
      (neighbours current)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = allList isOk $ getAllReachable initial neighbours []

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v $ getAllReachable initial neighbours []

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs

getNeighbours :: (Coord -> Tile) -> (Coord -> [Coord])
getNeighbours maze (C x y) =
  if maze (C x y) == Blank
  then
    []
  else
    filterList (\c -> maze c /= Wall) $ [C x (y + 1), C x (y - 1), C (x + 1) y, C (x - 1) y]

isClosed :: Maze -> Bool
isClosed (Maze initial maze) =
  (isEmpty $ maze initial) && (allList (\t -> maze t /= Blank) $ getAllReachable initial (getNeighbours maze) [])

getTile :: Maze -> Tile -> [Coord]
getTile (Maze initial maze) tile =
  filterList (\c -> maze c == tile) $ getAllReachable initial (getNeighbours maze) []

countTile :: Maze -> Tile -> Integer
countTile maze tile =
  listLength $ getTile maze tile

isSane :: Maze -> Bool
isSane maze = countTile maze Storage >= countTile maze Box
