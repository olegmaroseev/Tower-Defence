module ParseLevel where

import System.IO
import Codec.BMP
import GUI
import GameLogic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List

toParseLevel fName = do
	fileLevel <- readFile fName
	let levLines = lines fileLevel
	let	path = levLines !! 0
	levBackground@(Bitmap _ _ _ _) <- loadBMP path
	let listOfPoints = words $ levLines !! 1
	let listPoints = map (\x -> (tail.init) x) listOfPoints
	let listP = map (\x -> ( read(takeWhile (/= ',' ) x) ::Float, read(tail $ (dropWhile (/= ',' ) x))::Float) ) listPoints
	let listPFinal = foldl (\x y -> x ++ [y]) [] listP
	putStrLn $ path
	
parseEnemy fName = do	
	fileLevel <- readFile fName
	let levLines = lines fileLevel
	enemyPic@(Bitmap _ _ _ _) <- loadBMP $ levLines !! 2
	let listOfPoints = words $ levLines !! 3
	let listPoints = map (\x -> (tail.init) x) listOfPoints
	let listP = map (\x -> ( read(takeWhile (/= ',' ) x) ::Float, read(tail $ (dropWhile (/= ',' ) x))::Float) ) listPoints
	let listPFinal = foldl (\x y -> x ++ [y]) [] listP
	let positionEnemy = (\x -> ( read(takeWhile (/= ',' ) x) ::Float, read(tail $ (dropWhile (/= ',' ) x))::Float) ) $ levLines !! 1	
	let healthPoints = read (levLines !! 5)::Float
	let speedPoints = read (levLines !! 4)::Float
	return $ (Enemy (levLines!!0) positionEnemy enemyPic listPFinal healthPoints speedPoints undefined)
	
strToInt::String -> Int
strToInt s = read s
	
map2 = map (\x -> ( read(takeWhile (/= ',' ) x) ::Int, read(tail $ (dropWhile (/= ',' ) x))::Int) )
ffun = foldl (\x y -> x ++ [y]) [] $ map2 ["1,2","3,4"]