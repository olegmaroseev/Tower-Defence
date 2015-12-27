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
import Config

type Tower = GameObject
type Enemy = GameObject

data GameObject =
  Tower {   name::String,
            position::Point, 
            render::Picture,
            sellCost::Float, 
            upgradeCost::Float, 
            nextUpgrade::Maybe Tower, 
            range::Float,
            cooldown::Float,
            lastShot::Float,
            power::Float,
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

basicTower :: GameObject
basicTower = Tower {
            render = undefined, -- TODO: load a picture of it
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just basicTowerUpgrade1, 
            range = 10,
            cooldown = 5,
            lastShot = 0,
            power = 5,
            target = "",
            update = basicTowerShoot}

basicTowerUpgrade1 :: GameObject
basicTowerUpgrade1 = Tower {
            render = undefined, -- TODO: load a picture of it
            sellCost = 10, 
            upgradeCost = 20, 
            nextUpgrade = Just basicTowerUpgrade2, 
            range = 15,
            cooldown = 3,
            lastShot = 0,
            power = 7,
            target = "",
            update = basicTowerShoot}

basicTowerUpgrade2 :: GameObject
basicTowerUpgrade2 = Tower {
            render = undefined, -- TODO: load a picture of it
            sellCost = 20, 
            upgradeCost = 0, 
            nextUpgrade = Nothing, 
            range = 20,
            cooldown = 2,
            lastShot = 0,
            power = 10,
            target = "",
            update = basicTowerShoot}
            
basicTowerShoot :: GameObject -> Float -> [GameObject] -> [GameObject]
basicTowerShoot t time obj = undefined
          
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
renderGame (Game (x, y) w h GameState{..}) = Translate x (y + fromIntegral Config.controlPanelHeight) 
                                                $ Pictures $ (levelP : objectsP ++ placingP)
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
    globalUpdates = individualUpdates --undefined -- TODO: wave spawning, check if enemy reached the end

--TODO: Check if tower is on the path or collides with other towers
isPlacementCorrect :: Point -> GameState -> Bool
isPlacementCorrect pos GameState{..} = True --undefined

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = let dx = x2 - x1 
                                 dy = y2 - y1 in sqrt (dx * dx + dy * dy)

gameObjectHittest :: GameObject -> Point -> Bool
gameObjectHittest t p = 20 > distance p (position t)

isTower :: GameObject -> Bool
isTower Tower{..} = True
isTower _ = False

isEnemy :: GameObject -> Bool
isEnemy Enemy{..} = True
isEnemy _ = False


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
handleGameEvents _ g = g


setPlacingTower :: Game -> GameObject -> Game
setPlacingTower (Game (x,y) w h gs) t@Tower{..} = Game (x,y) w h gs {placingTower = Just t} 
--TODO: tower upgrading, tower selling, pausing game

