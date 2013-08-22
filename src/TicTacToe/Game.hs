{-# LANGUAGE MultiParamTypeClasses #-}

module TicTacToe.Game (Position, start, move, whoWon, playerAt, getBoard, isFinished) where

import Test.QuickCheck
import Data.List
import qualified Data.Set as S
import Control.Monad.State
import Debug.Trace

data Player = One | Two
  deriving (Eq, Show)

data Position = NE | NC | NW |
                E  | C  | W  |
                SE | SC | SW 
  deriving (Eq, Ord, Show)

data EmptyBoard = EmptyBoard
  deriving (Eq, Show)

data InPlayBoard = InPlayBoard [(Player,Position)]
  deriving (Eq, Show)

data FinishedBoard = FinishedBoard [(Player, Position)]
  deriving (Eq, Show)

data Board = EB' EmptyBoard | IB' InPlayBoard | FB' FinishedBoard deriving (Eq, Show)

data Outcome = IB InPlayBoard | FB FinishedBoard deriving (Eq, Show)

data PositionOccupiedError = PositionOccupiedError Position deriving (Eq, Show)

data Result' = Draw | Won Player deriving (Eq, Show)

-- |
-- 
-- >>> start NE 
-- InPlayBoard [(One,NE)]
start :: Position -> InPlayBoard
start p = InPlayBoard [(One, p)]

-- |
-- 
-- >>> move NE (InPlayBoard [(Two,NE),(One,C)])
-- Left (PositionOccupiedError NE)
--
-- >>> move SW (InPlayBoard [(Two,NE),(One,C)])
-- Right (IB (InPlayBoard [(One,SW),(Two,NE),(One,C)]))
-- 
-- >>> move NC (InPlayBoard [(Two, C),(One, NW),(Two, W), (One, NE)])
-- Right (FB (FinishedBoard [(One,NC),(Two,C),(One,NW),(Two,W),(One,NE)]))
--
-- prop> not (isOccupied' bd pos) ==> any (\f -> f (move pos bd)) [isFinished, isInPlay] 
--
-- prop> (isOccupied' bd pos) ==> isError (move pos bd) 
move :: Position -> InPlayBoard -> Either PositionOccupiedError Outcome
move p (InPlayBoard xs) = if isOccupied xs p then Left (PositionOccupiedError p)
                                             else let pl = if fst (head xs) == One then Two else One 
                                                      bd = (pl,p):xs in
                                                      Right (if isFinished' bd then FB (FinishedBoard bd)
                                                                               else IB (InPlayBoard bd))

playerAt :: Position -> Board -> Maybe Player
playerAt _ (EB' _) = Nothing
playerAt po (IB' (InPlayBoard b)) = playerAt' po b
playerAt po (FB' (FinishedBoard b)) = playerAt' po b

playerAt' ::  Eq a => a -> [(b, a)] -> Maybe b
playerAt' po' b' = fmap fst (find (\(_,p) -> p == po') b')

takeBack :: Outcome -> Either EmptyBoard InPlayBoard 
takeBack (FB (FinishedBoard b)) = Right (InPlayBoard (init b))
takeBack (IB (InPlayBoard b))   = let mv = init b in
                                      if null mv then Left EmptyBoard
                                                 else Right (InPlayBoard mv)

pprint :: Outcome -> String
pprint (IB (InPlayBoard b)) = draw b
pprint (FB (FinishedBoard b)) = draw b

draw :: [(Player, Position)] -> String
draw xs = let ch po = case playerAt' po xs of
                        Nothing   ->  "   "
                        Just One  -> " X "
                        Just Two  -> " O "
                        in
         intercalate "\n" [(ch NE) ++ "|" ++ (ch NC) ++ "|" ++ (ch NW), 
                            "===.===.===",
                            (ch E) ++ "|" ++ (ch C) ++ "|" ++ (ch W),
                            "===.===.===",
                            (ch SE) ++ "|" ++ (ch SC) ++ "|" ++ (ch SW)] 
  
-- |
--
-- >>> whoWon (FinishedBoard [(One,NC),(Two,C),(One,NW),(Two,W),(One,NE)])
-- Won One
whoWon :: FinishedBoard -> Result'
whoWon (FinishedBoard fb) = if isWinner fb then Won ((fst . head) fb)
                                           else Draw  
isFinished :: Either PositionOccupiedError Outcome -> Bool
isFinished (Right (FB _)) = True
isFinished _ = False

isInPlay :: Either PositionOccupiedError Outcome -> Bool
isInPlay (Right (IB _)) = True
isInPlay _ = False

isError :: Either PositionOccupiedError Outcome -> Bool
isError (Left (PositionOccupiedError _)) = True
isError _ = False

getBoard :: Outcome -> Maybe InPlayBoard
getBoard (IB x) = Just x
getBoard _      = Nothing

{-foldEBoard :: (EmptyBoard -> c) -> (InPlayBoard -> c) -> Board -> c
foldEBoard f _ (EB a) = f a
foldEBoard _ g (IB a) = g a-}

-- |
-- 
-- >>> isOccupied [(One,NC)] NW
-- False
--
-- >>> isOccupied [(One, NC)] NC
-- True
--
-- prop> z /= y ==> isOccupied [(x, y)] z == False
-- 
-- prop> z == y ==> isOccupied [(x,y)] z == True
isOccupied :: [(Player,Position)] -> Position -> Bool
isOccupied ps po = any (\(_,p) -> p == po) ps

isOccupied' :: InPlayBoard -> Position -> Bool
isOccupied' (InPlayBoard x) = isOccupied x

-- |
-- 
-- >>> isFinished' [(One, NC),(Two, C),(One, NW),(Two, W), (One, NE)]
-- True
--
-- >>> isFinished' [(Two,E),(One,SE),(Two,SC),(One,NC),(Two,NE),(One, C)]
-- False
isFinished' :: [(Player, Position)] -> Bool
isFinished' ps = (length ps == 9) || isWinner ps 

isWinner :: [(Player, Position)] -> Bool
isWinner ps = let pl = (fst . head) ps
                  mv = filter (\(x,_) -> x == pl) ps in
                  any (all (\w -> elem w (map snd mv))) winningList

winningList :: [[Position]]
winningList = [[NE,NC,NW],
               [E,C,W],
               [SE,SC,SW],
               [NE,E,SE],
               [NC,C,SC],
               [NW,W,SW],
               [NE,C,SW],
               [NW,C,SE]]

instance Arbitrary Position where 
  arbitrary = elements [NE,NC,NW,E,C,W,SE,SC,SW]

instance Arbitrary Player where
  arbitrary = elements [One, Two]

instance Arbitrary InPlayBoard where
  arbitrary = do
    pstart <- arbitrary
    fc_bds 6 (start pstart)

-- X - 1
-- 6
-- O - 2
-- 5
-- X - 3
-- 4
-- O - 4
-- 3
-- X - 5 
-- 2
-- O - 6
-- 1
-- X - 7
-- 0
fc_bds 0 b = return b
fc_bds n b = do ps <- arbitrary
                case move ps b of
                  Left _ -> fc_bds n b
                  Right (FB _) -> fc_bds n b 
                  Right (IB b') -> fc_bds (n-1) b'
