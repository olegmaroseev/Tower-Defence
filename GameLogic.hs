{-# LANGUAGE RecordWildCards #-}
module GameLogic where

import GUI
import Data.Ord
import Data.List
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.Typeable
import Data.Maybe

type Tower = GameObject
type Enemy = GameObject

data GameObject =
  Tower {   name::String,
            position::Point, 
            render::Picture,
            sellCost::Float, 
            upgradeCost::Float, 
            nextUpgrade::Tower, 
            range::Float,
            cooldown::Float,
            lastShot::Float,
            target::String,
            update::Float->[GameObject]->[GameObject]} |
  Enemy {   name::String,
            position::Point, 
            render::Picture,
            path::[Point],
            speed::Float,
            hitpoints::Float,
            power::Float,
            update::Float->[GameObject]->[GameObject]} |
  Bullet {  name::String,
            position::Point,
            render::Picture,
            speed::Float,
            target::String,
            power::Float,
            lifeTime::Float,
            update::Float->[GameObject]->[GameObject]} 


          
--Wave is (time to wait before starting after previous one, enemies of this wave)
type Wave = (Float, [Enemy])

data Level = Level {levelPicture::Picture, levelPath::[Point], levelWaves::[Wave]}

data GameState = GameState { level :: Level,
                   nextWaves :: [Wave],
                   money :: Int,
                   isPaused :: Bool,
                   selectedTower :: String,
                   lives :: Float,
                   objects :: [GameObject],
                   placingTower :: Maybe GameObject
                   }
                   
data Game = Game Point   
                 Integer 
                 Integer 
                 GameState
                 deriving Typeable
                 
instance GUIObject Game where
  renderObject g = renderGame g
  updateObject time g = updateGame time g
  eventHandler event g = handleGameEvents event g
  
--TODO: Actually use w and h parameters (Resize level? And everything else?)
renderGame :: Game -> Picture
renderGame (Game (x, y) w h GameState{..}) = Translate x y $ Pictures $ (levelP : objectsP ++ placingP)
  where
    levelP = levelPicture level
    objectsP = map render objects -- TODO: special render for selected tower
    placingP = map render $ maybeToList placingTower 

objectsOrder :: GameObject -> Int
objectsOrder Tower{..}  = 1
objectsOrder Enemy{..}  = 2
objectsOrder Bullet{..} = 3

updateObjects :: Float -> [GameObject] -> [GameObject]
updateObjects time xs = foldl (\acc x -> update x time acc) xs xs
    
updateGame :: Float -> Game -> Game
updateGame time (Game (x, y) w h gs@GameState{..}) = (Game (x, y) w h gs { objects = globalUpdates})
  where
    individualUpdates = updateObjects time objects
    globalUpdates = undefined -- TODO: wave spawning, check if enemy reached the end

--TODO: Check if tower is on the path or collides with other towers
isPlacementCorrect :: Point -> GameState -> Bool
isPlacementCorrect pos GameState{..} = undefined

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = let dx = x2 - x1 
                                 dy = y2 - y1 in sqrt (dx * dx + dy * dy)

gameObjectHittest :: GameObject -> Point -> Bool
gameObjectHittest t p = 20 > distance p (position t)

isTower :: GameObject -> Bool
isTower Tower{..} = True
isTower _ = False

handleGameEvents :: Event -> Game -> Game
handleGameEvents (EventMotion mpos) (Game (x, y) w h gs@GameState{..}) = (Game (x, y) w h gs { placingTower = newPlacingTower})
  where
    newPlacingTower = maybe Nothing (\t ->Just t {position = mpos}) placingTower
handleGameEvents (EventKey (MouseButton LeftButton) Up _ pos) (Game (x, y) w h gs@GameState{..}) = (Game (x, y) w h gs { objects = newObjects, selectedTower = newSelectedName, placingTower = newPlacing})
  where
    place = isPlacementCorrect pos gs
    possibleSelect = maybe selectedTower name $ find (\x -> gameObjectHittest x pos && isTower x) objects
    newSelectedName = if isJust placingTower then selectedTower else possibleSelect
    newObjects 
      | place && isJust placingTower = sortBy (comparing objectsOrder) $ (fromJust placingTower) : objects
      | otherwise = objects
    newPlacing 
      | place && isJust placingTower = Nothing
      | otherwise = placingTower

--TODO: tower upgrading, tower selling, pausing game
