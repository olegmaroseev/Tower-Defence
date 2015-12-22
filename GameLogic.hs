module GameLogic where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Data.Map


class GameObject a where
  update :: Float -> GameState -> a -> a
  base :: a -> BaseObject
  render :: a -> Picture
  render go = renderBase $ base go

renderBase :: BaseObject -> Picture
renderBase (BaseObject pic (x, y) _) = Translate x y pic
  
data BaseObject = BaseObject { picture :: Picture,
                               position :: Point,
                               index :: Int }
instance GameObject BaseObject where
  update _ _ = id
  base = id
                               
data BulletProperties = BulletProperties {  bulletSpeed::Float,
                                            radius::Float,
                                            radiusSpeed::Float,
                                            lifeTime::Float,
                                            power::Float }
                                            deriving (Show, Eq)
                                  
                
data Bullet = Bullet { properties :: BulletProperties, 
                       targetIndex :: Int }



data Tower = Tower { bullet::BulletProperties,
                     sellPrice::Int,
                     upgradePrice::Int}

data Enemy = Enemy { path :: [Point],
                     hp :: Float,
                     enemySpeed :: Float }

type Wave = (Float, [Enemy])

type Level = (Picture, [Point], [Wave])

data GameState = GameState { level :: Level,
                   currentWave :: Int,
                   totalWaves :: Int,
                   money :: Int,
                   objectIndex :: Int,
                   towers :: Map Int Tower,
                   enemies :: Map Int Enemy,
                   bullets :: Map Int Bullet}