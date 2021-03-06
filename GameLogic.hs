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
            reward::Int,
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
            render = getAsset "Tower1-1",
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just basicTowerUpgrade1, 
            range = 200,
            cooldown = 0.5,
            lastShot = 0,
            power = 3,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot basicBullet)}


basicTowerUpgrade1 :: GameObject
basicTowerUpgrade1 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower1-2",
            sellCost = 10, 
            upgradeCost = 20, 
            nextUpgrade = Just basicTowerUpgrade2, 
            range = 250,
            cooldown = 0.3,
            lastShot = 0,
            power = 4,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot basicBullet)}

basicTowerUpgrade2 :: GameObject
basicTowerUpgrade2 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower1-3", 
            sellCost = 15, 
            upgradeCost = 0, 
            nextUpgrade = Nothing, 
            range = 350,
            cooldown = 0.2,
            lastShot = 0,
            power = 5,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot basicBullet)}

magicTower :: GameObject
magicTower = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower2-1",
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just magicTowerUpgrade1, 
            range = 200,
            cooldown = 3,
            lastShot = 0,
            power = 15,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot magicBullet)}


magicTowerUpgrade1 :: GameObject
magicTowerUpgrade1 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower2-2",
            sellCost = 10, 
            upgradeCost = 20, 
            nextUpgrade = Just magicTowerUpgrade2, 
            range = 220,
            cooldown = 3,
            lastShot = 0,
            power = 20,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot magicBullet)}

magicTowerUpgrade2 :: GameObject
magicTowerUpgrade2 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower2-3", 
            sellCost = 20, 
            upgradeCost = 0, 
            nextUpgrade = Nothing, 
            range = 240,
            cooldown = 0.2,
            lastShot = 3,
            power = 25,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot magicBullet)}            

archerTower :: GameObject
archerTower = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower3-1",
            sellCost = 5, 
            upgradeCost = 10, 
            nextUpgrade = Just archerTowerUpgrade1, 
            range = 250,
            cooldown = 0.5,
            lastShot = 0,
            power = 2,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot archerBullet)}


archerTowerUpgrade1 :: GameObject
archerTowerUpgrade1 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower3-2",
            sellCost = 10, 
            upgradeCost = 20, 
            nextUpgrade = Just archerTowerUpgrade2, 
            range = 300,
            cooldown = 0.3,
            lastShot = 0,
            power = 4,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot archerBullet)}

archerTowerUpgrade2 :: GameObject
archerTowerUpgrade2 = Tower {
            name = "",
            position = (0,0), 
            price = 5,
            render = getAsset "Tower3-3", 
            sellCost = 20, 
            upgradeCost = 0, 
            nextUpgrade = Nothing, 
            range = 350,
            cooldown = 0.2,
            lastShot = 0,
            power = 6,
            target = "",
            bulletsCount = 0,
            update = (basicTowerShoot archerBullet)} 
            
basicTowerShoot :: GameObject-> GameObject -> Float -> [GameObject] -> [GameObject]
basicTowerShoot bType t time obj =     if  target t /= "" 
                                    && lastShot t <= 0
                                 then sortBy (comparing objectsOrder) $ (bType{name = bulletName, position = position t, target = target t}):(replaceGameObject (name t) (t{lastShot = cooldown t, bulletsCount = (1 + bulletsCount t)}) obj)
                                 else
                                  if target t /= "" && isJust oldtarget then
                                    replaceGameObject (name t) t{lastShot = lastShot t - time} obj
                                  else if length newTargets /= 0 then
                                     replaceGameObject (name t) newTower obj
                                  else 
                                    replaceGameObject (name t) (t{target = "", lastShot = lastShot t - time}) obj
                                      where
                                        oldtarget = find (\x -> target t == name x) obj
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

magicBullet :: GameObject
magicBullet = Bullet {
               name = ""
              ,position = (0,0)
              ,render = getAsset "bullet3"
              ,speed = 50
              ,target = ""
              ,power = 10
              ,update = basicBulletUpdate
              }
 
archerBullet :: GameObject
archerBullet = Bullet {
               name = ""
              ,position = (0,0)
              ,render = getAsset "bullet2"
              ,speed = 150
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
       | isJust mTarget && dist <= Config.enemyRadius = mTarget
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
            render = renderBasicEnemy "enemy1",--"enemy1",
            path=[],
            speed=50,
            hitpoints=10,
            maxHitpoints=10,
            power=1,
            reward = 10,
            update=basicEnemyUpdate
            }

basicUpdatedEnemy :: GameObject
basicUpdatedEnemy = Enemy {
            name="",
            position=(0,0), 
            render = renderBasicEnemy "enemy2",--"enemy2",
            path=[],
            speed=80,
            hitpoints=20,
            maxHitpoints=20,
            power=3,
            reward = 15,
            update=basicEnemyUpdate
            }

middleEnemy :: GameObject
middleEnemy = Enemy {
            name="",
            position=(0,0), 
            render = renderBasicEnemy "enemy3",--"enemy2",
            path=[],
            speed=100,
            hitpoints=30,
            maxHitpoints=30,
            power=4,
            reward = 20,
            update=basicEnemyUpdate
            }
			
strongEnemy :: GameObject
strongEnemy = Enemy {
            name="",
            position=(0,0), 
            render = renderBasicEnemy "enemy4",--"enemy2",
            path=[],
            speed=120,
            hitpoints=50,
            maxHitpoints=50,
            power=6,
            reward = 40,
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
updateGame delta (Game (x, y) w h assets gs@GameState{..})
  | isPaused = (Game (x, y) w h assets gs)
  | otherwise = (Game (x, y) w h assets gs { objects = globalUpdates, level = level {levelWaves = nw}, lives = lives - livesDelta, lastIndex = newIndex, money = money + moneyDelta})
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
    (livesDelta, moneyDelta, afterDeath) = getFinishedEnemies afterSpawn
    globalUpdates = afterDeath 
    
enemyReachedEnd :: Enemy -> Bool
enemyReachedEnd Enemy{..} = null path
    
getFinishedEnemies :: [GameObject] -> (Float, Int, [GameObject])
getFinishedEnemies xs = foldr step (0, 0, []) xs
  where
    step x@Enemy{..} (d, m, es)
      | hitpoints <= 0 = (d, m + reward, es)
      | enemyReachedEnd x = (d + power, m, es)
      | otherwise = (d, m, x:es)
    step x (d, m, es) = (d, m, x:es)
    
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
isPlacementCorrect x y w h pos gs@GameState{..} = (not $ pathCollision (map (toGameCoords (x,y) w h) (levelPath level)) pos) && (not $ towerCollision (map (position) objects) pos) && (not $ pointInBox pos ((fromIntegral w) / 2,-(fromIntegral h) / 2) (-(fromIntegral w) / 2, -300))

pathCollision (x:y:[]) p = (pointInBox p x y)
pathCollision (x:y:xs) p = (pointInBox p x y) || pathCollision (y:xs) p

towerCollision ((x,y):[]) p = (pointInBox p (x-70,y-70) (x+70, y+70))
towerCollision ((x,y):z) p = (pointInBox p (x-70,y-70) (x+70, y+70)) || towerCollision z p
towerCollision _ _ = False


distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = let dx = x2 - x1 
                                 dy = y2 - y1 in sqrt (dx * dx + dy * dy)

gameObjectHittest :: GameObject -> Point -> Bool
gameObjectHittest t p = Config.towerRadius > distance p (position t)

isTower :: GameObject -> Bool
isTower Tower{..} = True
isTower _ = False

isEnemy :: GameObject -> Bool
isEnemy Enemy{..} = True
isEnemy _ = False

toGameCoords :: Point -> Integer -> Integer -> Point -> Point
toGameCoords (gx, gy) w h (x, y) = (x - gx, y - gy)

gameHittest :: Point -> Integer -> Integer -> Point -> Bool
gameHittest (gx, gy) w h p = pointInBox p rightBot leftTop
  where
    halfW = (fromIntegral w) / 2
    halfH = (fromIntegral h) / 2
    leftTop = (gx-halfW,gy+halfH)
    rightBot = (gx+halfW, gy-halfH)

handleGameEvents :: Event -> Game -> Game
handleGameEvents (EventMotion rpos) (Game (x, y) w h assets gs@GameState{..}) = (Game (x, y) w h assets gs { placingTower = newPlacingTower })
  where
    mpos = toGameCoords (x, y) w h rpos
    newPlacingTower = maybe Nothing (\t ->Just t {position = mpos}) placingTower
handleGameEvents (EventKey (MouseButton LeftButton) Down _ rpos) (Game (x, y) w h assets gs@GameState{..})
  | gameHittest (x,y) w h rpos = (Game (x, y) w h assets gs { objects = newObjects, selectedTower = newSelectedName, placingTower = newPlacing, lastIndex = newIndex})
  | otherwise = Game (x, y) w h assets gs
  where
    pos = toGameCoords (x, y) w h rpos
    possibleSelect = maybe "" name $ find (\x -> gameObjectHittest x pos && isTower x) objects
    newSelectedName = if isJust placingTower then selectedTower else possibleSelect
    newName = show lastIndex
    placeTower = isPlacementCorrect x y w h pos gs && isJust placingTower
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
deleteCurrentTower (Game (x,y) w h assets gs@GameState{..}) 
  |selectedTower /= "" = Game (x,y) w h assets gs { objects = (filter (\x -> (getName x) /= (selectedTower)) (objects) ) , 
                                                                                          money = money + sellCost ( head (filter (\x -> (getName x) == (selectedTower)) (objects))) }
  | otherwise = (Game (x,y) w h assets gs)

upgradeCurrentTower::Game -> Game
upgradeCurrentTower (Game (x,y) w h assets gs@GameState{..})
           | selectedTower /= "" && (isJust mselectedT) && (isJust mnewTower) && (upgradeCost selectedT) <= money = Game (x,y) w h assets gs {  money = money - upgradeCost selectedT, objects = replaceGameObject selectedTower newTower objects }
           | otherwise = (Game (x,y) w h assets gs)
      where
        mselectedT = listToMaybe $ filter ((selectedTower==).name) objects
        selectedT = fromJust mselectedT
        mnewTower = nextUpgrade selectedT 
        newTower = (fromJust mnewTower) { name = name selectedT, position = position selectedT }

wavesLeft :: Game -> Int
wavesLeft (Game (x,y) w h assets gs@GameState{..}) = length $ levelWaves level

pauseGame :: Bool -> Game -> Game
pauseGame pause (Game (x,y) w h assets gs@GameState{..}) = (Game (x,y) w h assets gs {isPaused = pause} )


data GameResult = Win | Lose | Playing
  deriving (Show, Eq)

gameResult :: Game -> GameResult
gameResult (Game (x,y) w h assets gs@GameState{..})
  | lives <= 0 = Lose
  | null (levelWaves level) && null (filter isEnemy objects) = Win
  | otherwise = Playing
