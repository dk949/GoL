{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Random

type Board = [[Int]]

roll :: Int -> Int -> IO Board
roll w h = replicateM h (replicateM w (randomRIO (1, 0 :: Int)))

for :: [a] -> (a -> b) -> [b]
for = flip map

lifeInit :: Int -> IO Board
lifeInit scale = roll (scale * 2) scale

sumUpCells :: Board -> Int -> Int -> Int
sumUpCells b x y =
  ((b !! xM1) !! yM1)
    + ((b !! x) !! yM1)
    + ((b !! xP1) !! yM1)
    + ((b !! xM1) !! y)
    + ((b !! xP1) !! y)
    + ((b !! xM1) !! yP1)
    + ((b !! x) !! yP1)
    + ((b !! xP1) !! yP1)
  where
    fix = flip mod (length b)
    xM1 = fix (x - 1)
    xP1 = fix (x + 1)
    yM1 = fix (y - 1)
    yP1 = fix (y + 1)

lifeIter :: Board -> Board
lifeIter b =
  for
    [0 .. length b - 1]
    ( \x ->
        for
          [0 .. length b - 1]
          ( \y ->
              case sumUpCells b x y of
                3 -> 1
                2 -> (b !! x) !! y
                _ -> 0
          )
    )

showCell :: Int -> Char
showCell i = case i of
  1 -> '◉'
  0 -> ' '
  _ -> undefined

showBoard :: Board -> IO ()
showBoard = foldr ((>>) . (putStrLn . map showCell)) (return ())

clear :: IO ()
clear = putStr "\ESC[2J"

play :: Board -> IO ()
play b = do
  threadDelay 500000
  clear
  liftA2 (>>) showBoard play (lifeIter b)

main :: IO ()
main = lifeInit 50 >>= play
