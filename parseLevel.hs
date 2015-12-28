module ParseLevel where

import System.IO
import Codec.BMP
import GUI
import GameLogic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import EnemyBase

toParseLevel fName = do
  fileLevel <- readFile fName
  let levLines = lines fileLevel
  let  path = levLines !! 0
  levBackground@(Bitmap _ _ _ _) <- loadBMP path
  let listOfPoints = words $ levLines !! 1
  let listPoints = map (\x -> (tail.init) x) listOfPoints
  let listP = map (\x -> ( read(takeWhile (/= ',' ) x) ::Float, read(tail $ (dropWhile (/= ',' ) x))::Float) ) listPoints
  let listPFinal = foldl (\x y -> x ++ [y]) [] listP
  let wavesFile = tail $ tail $ levLines
  let mapWL = map (words) wavesFile
  let pa = map (\x -> (read (x !!0)::Float ,    map (\y -> getEnemy y ) $(tail x)        ))  mapWL
  return $ (Level levBackground listPFinal pa) 

parseEnemy fName = do  
  fileLevel <- readFile fName
  let levLines = lines fileLevel
  enemyPic@(Bitmap _ _ _ _) <- loadBMP $ levLines !! 2
  let positionEnemy = (\x -> ( read(takeWhile (/= ',' ) x) ::Float, read(tail $ (dropWhile (/= ',' ) x))::Float) ) $ levLines !! 1  
  let healthPoints = read (levLines !! 3)::Float
  let speedPoints = read (levLines !! 4)::Float
  let powerPoints = read (levLines !! 5)::Float
  return $ (Enemy (levLines!!0) positionEnemy undefined undefined healthPoints speedPoints powerPoints undefined)