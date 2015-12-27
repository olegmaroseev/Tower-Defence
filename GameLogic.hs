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
import qualified Data.Map as Map
import Config

type Tower = GameObject
type Enemy = GameObject

data GameObject =
  Tower {   name::String,
            position::Point, 
            render::AssetLibrary->Picture,
            sellCost::Float, 
            upgradeCost::Float, 
            nextUpgrade::Maybe Tower, 
            range::Float,
            cooldown::Float,
            lastShot::Float,
            power::Float,
            target::String,
            update::GameObject->Float->[GameObject]->[GameObject]} |
  Enemy {   name::String,
            position::Point, 
            render::AssetLibrary->Picture,
            path::[Point],
            speed::Float,
            hitpoints::Float,
            power::Float,
            update::GameObject->Float->[GameObject]->[GameObject]} |
  Bullet {  name::String,
            position::Point,
            render::AssetLibrary->Picture,
            speed::Float,
            target::String,
            power::Float,
            lifeTime::Float,
            update::GameObject->Float->[GameObject]->[GameObject]} 

basicTower :: GameObject
basicTower = Tower {
            name = "",
            position = (0,0), 
            render = getAsset "tower1", -- TODO: load a picture of it
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just basicTowerUpgrade1, 
            range = 10,
            cooldown = 5,
            lastShot = 0,
            power = 5,
            target = "",
            update = basicTowerShoot}

getAsset :: String -> AssetLibrary -> Picture
getAsset name assets = maybe Blank id $ Map.lookup name assets
            
basicTowerUpgrade1 :: GameObject
basicTowerUpgrade1 = Tower {
            name = "",
            position = (0,0), 
            render = getAsset "tower1", -- TODO: load a picture of it
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
            name = "",
            position = (0,0), 
            render = getAsset "tower1", -- TODO: load a picture of it
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
basicTowerShoot t time obj = obj--undefined
          
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

type AssetLibrary = Map.Map String Picture

data Game = Game Point   
                 Integer 
                 Integer 
                 AssetLibrary
                 GameState
                 deriving Typeable
                 
instance GUIObject Game where
  renderObject g = renderGame g
  updateObject time g = updateGame time g
  eventHandler event g = handleGameEvents event g
  
--TODO: Actually use w and h parameters (Resize level? And everything else?)
renderGame :: Game -> Picture
renderGame (Game (x, y) w h assets GameState{..}) = Translate x (y + fromIntegral Config.controlPanelHeight) 
                                                $ Pictures $ (levelP : objectsP ++ placingP)
  where
    levelP = levelPicture level
    objectsP = map (flip render assets) objects -- TODO: special render for selected tower
    placingP = map (flip render assets) $ maybeToList placingTower 

objectsOrder :: GameObject -> Int
objectsOrder Tower{..}  = 1
objectsOrder Enemy{..}  = 2
objectsOrder Bullet{..} = 3

updateObjects :: Float -> [GameObject] -> [GameObject]
updateObjects time xs = foldl (\acc x -> update x x time acc) xs xs
    
updateGame :: Float -> Game -> Game
updateGame time (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { objects = globalUpdates})
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
handleGameEvents (EventMotion mpos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { placingTower = newPlacingTower})
  where
    newPlacingTower = maybe Nothing (\t ->Just t {position = mpos}) placingTower
handleGameEvents (EventKey (MouseButton LeftButton) Up _ pos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { objects = newObjects, selectedTower = newSelectedName, placingTower = newPlacing})
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
setPlacingTower (Game (x,y) w h assets gs) t@Tower{..} = Game (x,y) w h assets gs {placingTower = Just t} 
setPlacingTower g _ = g
--TODO: tower upgrading, tower selling, pausing game

