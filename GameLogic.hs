{-# LANGUAGE RecordWildCards #-}
module GameLogic where

import GUI
import Data.Ord
import Data.List
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.Typeable

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
                   objects :: [GameObject]
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
renderGame (Game (x, y) w h GameState{..}) = Translate x y $ Pictures $ (levelP : objectsP)
  where
    levelP = levelPicture level
    objectsP = map render objects

updateOrder :: GameObject -> Int
updateOrder Tower{..} = 1
updateOrder Enemy{..} = 2
updateOrder Bullet{..} = 3

updateObjects :: Float -> [GameObject] -> [GameObject]
updateObjects time xs = updated
  where
    sorted = sortBy (comparing updateOrder) xs
    updated = foldl (\acc x -> update x time acc) xs xs
    
updateGame :: Float -> Game -> Game
updateGame time (Game (x, y) w h gs@GameState{..}) = (Game (x, y) w h gs { objects = (updateObjects time objects)})

--TODO: Tower select, something else?
handleGameEvents :: Event -> Game -> Game
handleGameEvents event (Game (x, y) w h GameState{..}) = undefined

--TODO: Tower placement, tower upgrading, tower selling, pausing game
