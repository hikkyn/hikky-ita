module Main where
 
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
 
import Control.Arrow
import Data.List
import Data.Function
 
import System.Random (randoms)
import System.Random.Mersenne.Pure64 (pureMT)
 
import Data.Time
import Data.Time.Clock.POSIX
 
import System.Environment
 
import Debug.Trace
 
data StoneColor = White | Black | Empty
  deriving (Eq,Show)
 
reversed White = Black
reversed Black = White
reversed Empty = Empty
 
type Position = (Int,Int)
type Stone = (StoneColor,Position)
data ReversiTable = ReversiTable {
  size   :: (Int,Int)
  , stones :: [Stone]
}
 
type Solver = (GameState -> Maybe Position)
data Player =
    Man String StoneColor 
  | AI  String StoneColor Solver
 
data GameState = GameState {
  fps :: Int
  , seeds      :: [Int]
  , step       :: Int
  , windowSize :: (Int,Int)
  , skipped    :: Bool
  , finished   :: Bool
  , table      :: ReversiTable
  , players    :: [Player]
}
 
baseState = GameState {
    fps = 12
    , seeds = [0]
    , step = 0
    , windowSize = (1366,768)
    , skipped = False
    , finished = False
    , table = initTable (10,10)
    , players = []
  }
  where
    initTable size = ReversiTable { size = size, stones = initStones }
      where
        (dx,dy) = tmap ((flip (-) $ 1) . (`div` 2)) size
        initStones = [ (color, (dx + x,dy + y)) | x <- [0 .. 1], y <- [0 .. 1],
          let color = if (x + y) `mod` 2 == 0 then White else Black ]
 
 
stoneColor :: GameState -> StoneColor
stoneColor state = colorOf . head . players $ state
colorOf (AI  _ color _) = color
colorOf (Man _ color)   = color
 
getPlayers "cpu" = [ (AI "CPU1"    Black randomAI), (AI  "CPU2"   White randomAI) ]
getPlayers "rev" = [ (AI "CPU"     Black randomAI), (Man "Player" White) ]
getPlayers _     = [ (Man "Player" Black),          (AI  "CPU"    White randomAI) ]
nextPlayers players = take (length players) (tail . cycle $ players)
 
main = do
  argv <- getArgs
  let proc = if null argv then "man" else head argv
  time    <- getPOSIXTime  
  initialState <- return $ baseState {
      seeds   = randoms . pureMT $ round (time * 1000),
      players = getPlayers proc
    }
 
  play (InWindow "Reversi" (fwsize initialState) (20,20)) green (fps initialState) initialState drawTable eventHandler looper
 
fwsize          = tmap fromIntegral . windowSize
 
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) state
  | not (isPlayer state)  = state
  | finished state        = state
  | null (movables state) = state
  | not (elem stonePos (movables state)) = state
  | otherwise = nextState
    where
      (w,h)    = tmap fromIntegral $ windowSize state
      (nx,ny)  = tmap fromIntegral $ size . table $ state
      stonePos = tmap floor ( (0.5 + (x/w))*nx,(0.5+ (y/h))*ny)
      nextState = state {
        step = 0
        , skipped  = False
        , finished = False
        , players  = nextPlayers (players state)
        , table    = makeMove state stonePos
      }
 
eventHandler _ state = state
 
isPlayer :: GameState -> Bool
isPlayer state = case head (players state) of
  Man _ _   -> True
  AI  _ _ _ -> False
 
 
looper :: Float -> GameState -> GameState
looper _ state
  | finished state = state
  | step state < (fps state `div` 10) = nextStep
  | (null $ movables state) = skipState
  | otherwise   = nextState
    where 
      nextStep  = state    {step = step state + 1}
      baseState = nextStep {
          step = 0
          , players = nextPlayers (players state)
        }
 
      skipState   = baseState {skipped = True, finished = skipped state}
      nextAiState ai = case ai state of
          Nothing       -> skipState
          Just stonePos -> baseState {
              skipped     = False
              , finished = False
              , table    = makeMove state stonePos
            }
 
      nextState = case (head . players $ state) of
        Man _ _    -> state {step = 0}
        AI  _ _ ai -> nextAiState ai
 
tmap f = f *** f 
 
tablePict :: (Int,Int) -> (Int,Int) -> Picture
tablePict (w,h) (sx,sy) = Pictures . map (Color black . Line . flr) $ lines
  where
    flr = map $ tmap fromIntegral
    axesX = [ ((0, y):(sx, y):[]) | t <- [ 1 .. (w-1)], let y = (t * sy) `div` h ]
    axesY = [ ((x, 0):(x, sy):[]) | t <- [ 1 .. (h-1)], let x = (t * sx) `div` w ]
    lines = concat (axesX:axesY:[])
 
stonePict :: (Int,Int) -> (Int,Int) -> Stone -> Picture
stonePict (w,h) (sx,sy) (color,(x,y)) = 
  Translate tx ty circle
  where
    ((fx,fy):(fsx,fsy):(fw,fh):[]) = map (tmap fromIntegral) ((x,y):(sx,sy):(w,h):[])
 
    (ux,uy) = (fsx/fw, fsy/fh)
    (tx,ty) = (ux * (fx + 0.5), uy * (fy + 0.5))
    (cw,ct) = ((ux + uy) / 16, (ux + uy) / 8)
    circle = circleColor $ ThickCircle cw ct
    circleColor = case color of
      White -> Color white
      Black -> Color black
 
drawTable :: GameState -> Picture
drawTable state =
  -- (step,sp@(sx,sy),_,tbl@(tableSize,ls)) =
  if finished state then Pictures (playScreen:getWinnerText state:[]) else playScreen
  where
    playScreen = Translate tx ty $ Pictures (frameLine:stonePics)
    sp         = windowSize state
    (tx,ty)    = tmap ((* (-1)) . (/ 2). fromIntegral ) sp
    frameLine  = tablePict (size . table $ state) sp
    stonePics  = map (stonePict (size . table $ state) sp) (stones . table $ state)
 
getWinnerText :: GameState -> Picture
getWinnerText state = Color violet . Scale 0.5 0.5 . Text $ winnerText
  where
    count table pl = length $ filter ((== colorOf pl) . fst) table
    counter x      = (count (stones . table $ state) x, x)
    countList      = map counter $ players state
 
    winner         = maximumBy (compare `on` fst) countList
    winnerText     = nameOf (snd winner)  ++ " Win"
    nameOf pl = case pl of
      AI  name _ _ -> name
      Man name _   -> name
 
 
makeMove :: GameState -> Position -> ReversiTable
makeMove state pos = put nextTable (color,pos)
  where
    (tbl,color) = (table state, stoneColor state)
    nextTable   = foldl doReverse tbl posList
    posList     = map snd $ allReversibleStoneList tbl color pos
 
get :: ReversiTable -> Position -> Stone
get table pos = case found of
  Just os -> os
  Nothing -> (Empty,pos)
  where
    found = find ((== pos) . snd) (stones table)
 
doReverse :: ReversiTable -> Position -> ReversiTable
doReverse table pos = case get table pos of
  (Empty,_)    -> table
  (stone,dpos) -> put table (reversed stone,dpos)
 
put :: ReversiTable -> Stone -> ReversiTable
put table (color,pos) = table { stones = ((color,pos):rest) }
  where
    rest = case get table pos of
      (Empty,_)    -> stones table
      (color,dpos) -> filter ((/= dpos) . snd) (stones table)
 
reversibleStoneListByLine :: ReversiTable -> StoneColor -> Position -> Position -> [Stone]
reversibleStoneListByLine table color pos direction@(nx,ny)
  | null rest || (fst (head rest) /= color) = []
  | otherwise  = revs
  where
    (w,h) = size table
    target = map (get table) $ targetPosList pos
    (revs,rest) = span ((== reversed color) . fst) target
    targetPosList (px,py) = if (0 <= px) && (px < w) && (0 <= py) && (py < h)
      then let npos = (px + nx, py + ny) in (npos: targetPosList npos) else []
 
allReversibleStoneList :: ReversiTable -> StoneColor -> Position -> [Stone]
allReversibleStoneList table color pos =
  concat . map (reversibleStoneListByLine table color pos) $ dirs
  where
    dirs = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)] 
 
 
getEmptyList :: ReversiTable -> [Position]
getEmptyList table = [
    (x,y) | let (w,h) = size table, x <- [0 .. (w-1)], y <- [0 .. (h-1)], 
            not $ elem (x,y) (map snd $ stones table) ]
 
 
randomAI :: Solver
randomAI state =
  if null target then Nothing else Just chosen
  where
    chosen = head . drop ((head $ seeds state) `mod` (length target)) $ target
    target = movables state
 
movables :: GameState -> [Position]
movables state = filter (movable tbl color) (getEmptyList tbl)
  where
    color = stoneColor state
    tbl   = table state
 
 
movable :: ReversiTable -> StoneColor -> Position -> Bool
movable tbl color pos = not . null $ allReversibleStoneList tbl color pos
