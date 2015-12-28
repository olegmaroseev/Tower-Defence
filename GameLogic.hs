{-# LANGUAGE RecordWildCards#-}
module GameLogic where

import GUI
import Data.Ord (comparing)
import Data.List
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Typeable
import Data.Maybe
import qualified Data.Map as Map
import Config
import Graphics.Gloss.Data.Point

type Tower = GameObject
type Enemy = GameObject

data GameObject =
  Tower {   name::String,
            position::Point, 
            price::Int,
            render::GameObject -> AssetLibrary->Picture,
            sellCost::Int, 
            upgradeCost::Int, 
            nextUpgrade::Maybe Tower, 
            range::Float,
            cooldown::Float,
            lastShot::Float,
            power::Float,
            target::String,
            bulletsCount::Int,
            update::GameObject->Float->[GameObject]->[GameObject]} |
  Enemy {   name::String,
            position::Point, 
            render::GameObject -> AssetLibrary->Picture,
            path::[Point],
            speed::Float,
            hitpoints::Float,
            maxHitpoints::Float,
            power::Float,
            update::GameObject->Float->[GameObject]->[GameObject]} |
  Bullet {  name::String,
            position::Point,
            render::GameObject -> AssetLibrary->Picture,
            speed::Float,
            target::String,
            power::Float,
            update::GameObject->Float->[GameObject]->[GameObject]} 

basicTower :: GameObject
basicTower = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "tower1",
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just basicTowerUpgrade1, 
            range = 300,
            cooldown = 5,
            lastShot = 0,
            power = 5,
            target = "",
            bulletsCount = 0,
            update = basicTowerShoot}


basicTowerUpgrade1 :: GameObject
basicTowerUpgrade1 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "tower1",
            sellCost = 10, 
            upgradeCost = 20, 
            nextUpgrade = Just basicTowerUpgrade2, 
            range = 500,
            cooldown = 3,
            lastShot = 0,
            power = 7,
            target = "",
            bulletsCount = 0,
            update = basicTowerShoot}

basicTowerUpgrade2 :: GameObject
basicTowerUpgrade2 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "tower1", 
            sellCost = 20, 
            upgradeCost = 0, 
            nextUpgrade = Nothing, 
            range = 700,
            cooldown = 1,
            lastShot = 0,
            power = 10,
            target = "",
            bulletsCount = 0,
            update = basicTowerShoot}
            
basicTowerShoot :: GameObject -> Float -> [GameObject] -> [GameObject]
basicTowerShoot t time obj =     if  target t /= "" 
                                    && lastShot t <= 0
                                 then sortBy (comparing objectsOrder) $ (basicBullet{name = bulletName, position = position t, target = target t}):(replaceGameObject (name t) (t{lastShot = cooldown t, bulletsCount = (1 + bulletsCount t)}) obj)
                                 else
                                  if target t /= "" then
                                    replaceGameObject (name t) t{lastShot = lastShot t - time} obj
                                  else if length newTargets /= 0 then
                                     replaceGameObject (name t) newTower obj
                                  else 
                                    replaceGameObject (name t) (t{lastShot = lastShot t - time}) obj
                                      where
                                        newTargets = sortBy (comparing snd) $ filter (\(_, d) -> d>=0 && d<=range t) $ map (mapDistance (fst $ position t) (snd $ position t)) obj
                                        newTower = t{target = (fst $ head newTargets), lastShot = lastShot t - time}
                                        bulletName = name t ++ (show $ bulletsCount t) ++ target t
                                 
mapDistance :: Float -> Float -> GameObject -> (String, Float)                               
mapDistance x y t@Enemy{..} = (name, dist)
                                where
                                  dx = x - (fst position)
                                  dy = y - (snd position)
                                  dist = sqrt (dx*dx + dy*dy)
mapDistance _ _ _ = ("", -1)

basicBullet :: GameObject
basicBullet = Bullet {
               name = ""
              ,position = (0,0)
              ,render = getAsset "bullet1"
              ,speed = 100
              ,target = ""
              ,power = 1
              ,update = basicBulletUpdate
              }
              
basicBulletUpdate :: GameObject -> Float -> [GameObject] -> [GameObject]
basicBulletUpdate b time objs = if isJust mB && isNothing shooted then
                                    replaceGameObject (name b) (fromJust mB) objs
                                else
                                    if isJust shooted then
                                        replaceGameObject (name shEn) (shEn{hitpoints = hitpoints shEn - power b}) delBullet
                                    else
                                        delBullet
                            where
                             mB = moveBullet time b objs
                             shooted = (targetShooted b objs)
                             shEn = (fromJust shooted)
                             delBullet = filter ((/= name b).name) objs

targetShooted :: GameObject -> [GameObject] -> Maybe GameObject
targetShooted b@Bullet{..} objs
       | isJust mTarget && dist <= speed = mTarget
       | otherwise = Nothing
           where
            (bx, by) = position
            (tx, ty) = getPos $ fromJust mTarget
            dx = tx - bx
            dy = ty - by
            dist = sqrt (dx*dx + dy*dy)
            mTarget = find (\x -> target == getName x) objs

moveBullet :: Float -> GameObject -> [GameObject] -> Maybe GameObject
moveBullet delta b@Bullet{..} objs
  | null target = Nothing
  | otherwise = maybe Nothing (const (Just movedBullet)) mTarget
  where
    (bx, by) = position
    (tx, ty) = getPos $ fromJust mTarget
    dx = tx - bx
    dy = ty - by
    dist = sqrt (dx*dx + dy*dy)
    dir = atan2 dy dx
    movedBullet = b { position = (bx + speed * delta * cos dir, by + speed * delta * sin dir) } 
    mTarget = find (\x -> target == getName x) objs

getName :: GameObject -> String
getName = name

getPos :: GameObject -> Point
getPos = position

getAsset :: String -> GameObject -> AssetLibrary -> Picture
getAsset name go assets = Translate (fst $ position go) (snd $ position go) $ maybe Blank id $ Map.lookup name assets
            

basicEnemy :: GameObject
basicEnemy = Enemy {
            name="",
            position=(0,0), 
            render = renderBasicEnemy "tower1",--"enemy1",
            path=[],
            speed=50,
            hitpoints=10,
            maxHitpoints=10,
            power=1,
            update=basicEnemyUpdate
            }

renderBasicEnemy :: String -> GameObject -> AssetLibrary -> Picture
renderBasicEnemy an e@Enemy{..} assets = Translate (fst position) (snd position) pic
  where
    dir = -(enemyDir e) * 180 / pi
    pic = Pictures $ [asset, healthBar]
    asset = maybe Blank (Rotate dir) $ Map.lookup an assets
    all = Color (makeColor 0 0 0 1) $ Polygon [((-Config.hpHalfWidth), (-Config.hpHalfHeight)),(Config.hpHalfWidth, (-Config.hpHalfHeight)),(Config.hpHalfWidth, Config.hpHalfHeight),((-Config.hpHalfWidth), Config.hpHalfHeight)]
    ratio = hitpoints / maxHitpoints
    health = Color (makeColor 1 1 1 1) $ Polygon [((-Config.hpHalfWidth) * ratio + Config.hpMargin, (-Config.hpHalfHeight) + Config.hpMargin),(Config.hpHalfWidth * ratio - Config.hpMargin, (-Config.hpHalfHeight) + Config.hpMargin),(Config.hpHalfWidth * ratio - Config.hpMargin, Config.hpHalfHeight - Config.hpMargin),((-Config.hpHalfWidth) * ratio + Config.hpMargin, Config.hpHalfHeight - Config.hpMargin)]
    healthBar = Translate 0 Config.hpTranslate $ Pictures $ [ all, health ]
            
replace :: (a -> Bool) -> a -> [a] -> [a]
replace pred new xs = map (\x -> if pred x then new else x) xs

replaceGameObject :: String -> GameObject -> [GameObject] -> [GameObject]
replaceGameObject n obj xs = replace ((n==).name) obj xs

enemyDir :: GameObject -> Float
enemyDir Enemy{..} = dir
  where
    (cx, cy) = position
    (nx, ny) = head path
    dx = nx - cx
    dy = ny - cy
    dir = atan2 dy dx
enemyDir _ = 0

moveEnemy :: Float -> GameObject -> GameObject
moveEnemy delta e@Enemy{..}
  | null path = e
  | dist < 1 = e { position = (nx, ny), path = tail path }
  | otherwise = movedEnemy
  where
    (cx, cy) = position
    (nx, ny) = head path
    dx = nx - cx
    dy = ny - cy
    dist = sqrt (dx*dx + dy*dy)
    dir = atan2 dy dx
    movedEnemy = e { position = (cx + speed * delta * cos dir, cy + speed * delta * sin dir) }

basicEnemyUpdate :: GameObject -> Float -> [GameObject] -> [GameObject]
basicEnemyUpdate e time objs = replaceGameObject (name e) newEnemy objs
  where
    newEnemy = moveEnemy time e
            
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
                   placingTower :: Maybe GameObject,
                   lastIndex :: Int
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
renderGame (Game (x, y) w h assets GameState{..}) = Translate x y 
                                                $ Pictures $ (levelP : objectsP ++ placingP)
  where
    levelP = levelPicture level
    objectsP = map (\x -> Pictures $ ((render x x assets) : if (selectedTower == name x) then [renderSelected x] else [] )) objects 
    placingP = maybe [] (\x -> [render x x assets, renderSelected x]) placingTower 

filledCircle :: Int -> Float -> Picture
filledCircle n r = Polygon p
  where
    (p, _) = foldl step ([], 0) [1..n]
    step (acc, angle) _ = ((r * cos angle, r * sin angle):acc, angle + 2*pi / fromIntegral n)
    
renderSelected :: GameObject -> Picture
renderSelected Tower{..} = Translate (fst position) (snd position) $ Color (makeColor 1 1 1 0.5) $ filledCircle 500 range
renderSelected _ = Blank
    
objectsOrder :: GameObject -> Int
objectsOrder Tower{..}  = 1
objectsOrder Enemy{..}  = 2
objectsOrder Bullet{..} = 3

updateObjects :: Float -> [GameObject] -> [GameObject]
updateObjects time xs = foldl (\acc x -> update x x time acc) xs xs
    
updateGame :: Float -> Game -> Game
updateGame delta (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { objects = globalUpdates, level = level {levelWaves = nw}, lives = lives - livesDelta, lastIndex = newIndex})
  where
    individualUpdates = updateObjects delta objects
    (ne, nw) = updateWaves delta $ levelWaves level
    lpath = map (toGameCoords (x,y) w h) (levelPath level)
    newName = "Enemy" ++ show lastIndex
    newIndex
      | isJust ne = lastIndex + 1
      | otherwise = lastIndex
    afterSpawn = case ne of
                Just e -> sortBy (comparing objectsOrder) $ (initEnemy e newName lpath) : individualUpdates
                Nothing -> individualUpdates
    (livesDelta, afterDeath) = getFinishedEnemies afterSpawn
    globalUpdates = afterDeath 
    
enemyReachedEnd :: Enemy -> Bool
enemyReachedEnd Enemy{..} = null path
    
getFinishedEnemies :: [GameObject] -> (Float, [GameObject])
getFinishedEnemies xs = foldr step (0, []) xs
  where
    step x@Enemy{..} (d, es)
      | enemyReachedEnd x = (d + power, es)
      | otherwise = (d, x:es)
    step x (d, es) = (d, x:es)
    
initEnemy :: Enemy -> String -> [Point] -> Enemy
initEnemy e@Enemy{..} n p = e {path = p, position = head p, name = n}
    
updateWaves :: Float -> [Wave] -> (Maybe Enemy, [Wave])
updateWaves _ [] = (Nothing, [])
updateWaves time (w:ws) = case updateWave time w of
                              (Nothing, nw) -> (Nothing, nw:ws)
                              (Just e, (_, [])) -> (Just e, ws)
                              (Just e, nw) -> (Just e, nw:ws)

    
updateWave :: Float -> Wave -> (Maybe Enemy, Wave)
updateWave dt (spawn, e : es)
  | spawn > dt = (Nothing, (spawn - dt, e:es) )
  | otherwise = (Just e, (1, es) )  
    
--TODO: Check if tower is on the path or collides with other towers
isPlacementCorrect :: Point -> GameState -> Bool
isPlacementCorrect pos gs@GameState{..} = (not $ pathCollision (levelPath level) pos)

pathCollision (x:y:[]) p = (pointInBox p x y)
pathCollision (x:y:xs) p = (pointInBox p x y) || pathCollision (y:xs) p

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

toGameCoords :: Point -> Integer -> Integer -> Point -> Point
toGameCoords (gx, gy) w h (x, y) = (x - gx, y - gy)

handleGameEvents :: Event -> Game -> Game
handleGameEvents (EventMotion rpos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { placingTower = newPlacingTower })
  where
    mpos = toGameCoords (x, y) w h rpos
    newPlacingTower = maybe Nothing (\t ->Just t {position = mpos}) placingTower
handleGameEvents (EventKey (MouseButton LeftButton) Down _ rpos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { objects = newObjects, selectedTower = newSelectedName, placingTower = newPlacing, lastIndex = newIndex})
  where
    pos = toGameCoords (x, y) w h rpos
    possibleSelect = maybe "" name $ find (\x -> gameObjectHittest x pos && isTower x) objects
    newSelectedName = if isJust placingTower then selectedTower else possibleSelect
    newName = show lastIndex
    placeTower = isPlacementCorrect pos gs && isJust placingTower
    newIndex
      | placeTower = lastIndex + 1
      | otherwise = lastIndex
    newObjects 
      | placeTower = sortBy (comparing objectsOrder) $ ((fromJust placingTower) {name = newName}) : objects
      | otherwise = objects
    newPlacing 
      | placeTower = Nothing
      | otherwise = placingTower
handleGameEvents (EventKey (MouseButton RightButton) Down _ rpos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { placingTower = Nothing})
handleGameEvents _ g = g

setPlacingTower :: Game -> Point -> GameObject -> Game
setPlacingTower (Game (x,y) w h assets gs) pos t@Tower{..} | price <= (money gs) = ( Game (x,y) w h assets gs {placingTower = Just t {position = toGameCoords (x,y) w h pos}, selectedTower = "", money = (money gs) - price} )
setPlacingTower g _ _ = g

deleteCurrentTower :: Game -> Game
deleteCurrentTower (Game (x,y) w h assets gs@GameState{..}) =  Game (x,y) w h assets gs { objects = (filter (\x -> (name x) /= (selectedTower)) (objects) ) , money = money + sellCost ( (filter (\x -> (name x) == (selectedTower)) (objects))!!0) }

upgradeCurrentTower::Game -> Game
upgradeCurrentTower (Game (x,y) w h assets gs@GameState{..})
          | upgradeCost ( (filter (\x -> (name x) == (selectedTower)) (objects))!!0) <= (money) = deleteCurrentTower $ Game (x,y) w h assets gs {  money = money - upgradeCost ( (filter (\x -> (name x) == (selectedTower)) (objects))!!0),  objects = objects ++ [(( fromJust  (nextUpgrade ( (filter (\x -> (name x) == (selectedTower)) (objects))!!0)) ){ position = (position ((filter (\x -> (name x) == (selectedTower)) (objects))!!0 )) , name = (name ((filter (\x -> (name x) == (selectedTower)) (objects)) !!0 )) } )]  }
          | otherwise = (Game (x,y) w h assets gs)


--TODO: pausing game

