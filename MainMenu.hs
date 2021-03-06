{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Codec.BMP
import System.Environment
import GUI
import Config
import GameLogic
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import ParseLevel

level1min = "pic/level1.bmp"
level2min = "pic/level2.bmp"
level3min = "pic/level3.bmp"
level4min = "pic/level4.bmp"
level5min = "pic/level5.bmp"
butQuitMin = "pic/but.bmp"

lev1path = "data/level1.txt"
lev2path = "data/level2.txt"
lev3path = "data/level3.txt"
lev4path = "data/level4.txt"
lev5path = "data/level5.txt"

assetPic :: String -> IO Picture
assetPic picName = do
        let path = snd $ head $ filter (\(s, p) -> s == picName) picBase
        curIcon@(Bitmap _ _ _ _) <- loadBMP path
        return curIcon

replaceLevel :: [(String, GUIElem)] -> Level -> [(String, GUIElem)]
replaceLevel els l = map update els
  where
    update ("Game", gel)
      | Just (Game (x, y) w h picLib gs) <- unpackCast gel = ("Game", GUIElem (Game (x, y) w h picLib gs {level = l}))
    update x = x
        
main :: IO()
main = do 
    level1ico@(Bitmap _ _ _ _) <- loadBMP level1min
    level2ico@(Bitmap _ _ _ _) <- loadBMP level2min
    level3ico@(Bitmap _ _ _ _) <- loadBMP level3min
    level4ico@(Bitmap _ _ _ _) <- loadBMP level4min
    level5ico@(Bitmap _ _ _ _) <- loadBMP level5min
    butQuit@(Bitmap _ _ _ _) <- loadBMP butQuitMin
    towerIcon1_1 <- assetPic "Tower1-1"
    towerIcon1_2 <- assetPic "Tower1-2"
    towerIcon1_3 <- assetPic "Tower1-3"
    towerIcon2_1 <- assetPic "Tower2-1"
    towerIcon2_2 <- assetPic "Tower2-2"
    towerIcon2_3 <- assetPic "Tower2-3"
    towerIcon3_1 <- assetPic "Tower3-1"
    towerIcon3_2 <- assetPic "Tower3-2"
    towerIcon3_3 <- assetPic "Tower3-3"
    enemyIcon1 <- assetPic "Enemy1"
    enemyIcon2 <- assetPic "Enemy2"
    enemyIcon3 <- assetPic "Enemy3"
    enemyIcon4 <- assetPic "Enemy4"
    bulletIcon1 <- assetPic "Bullet1"
    bulletIcon2 <- assetPic "Bullet2"
    bulletIcon3 <- assetPic "Bullet3"
    upgradeIcon <- assetPic "Upgrade"
    sellIcon <- assetPic "Sell"
    bacgroundPic <- assetPic "Background1"
    let picLib = (Map.fromList [("Tower1-1", towerIcon1_1)
                               ,("Tower1-2", towerIcon1_2)
                               ,("Tower1-3", towerIcon1_3)
                               ,("Tower2-1", towerIcon2_1)
                               ,("Tower2-2", towerIcon2_2)
                               ,("Tower2-3", towerIcon2_3)
                               ,("Tower3-1", towerIcon3_1)
                               ,("Tower3-2", towerIcon3_2)
                               ,("Tower3-3", towerIcon3_3)
                               ,("bullet1", bulletIcon1)
                               ,("bullet2", bulletIcon2)
                               ,("bullet3", bulletIcon3)
                               ,("enemy1", enemyIcon1)
                               ,("enemy2", enemyIcon2)
                               ,("enemy3", enemyIcon3)
                               ,("enemy4", enemyIcon4)
                               ,("Background1", bacgroundPic)
                               ])
    --let level1 = Level bacgroundPic [(-200,-200), (-100, 0), (0, 100)] [(1, [basicEnemy, basicEnemy]), (5, [basicEnemy, basicEnemy])]
    level1 <- toParseLevel "data/level1.txt"
    level2 <- toParseLevel "data/level2.txt"
    level3 <- toParseLevel "data/level3.txt"
    level4 <- toParseLevel "data/level4.txt"
    level5 <- toParseLevel "data/level5.txt"
    let initGame = [("Menu", GUIElem (SpecialTextButton (-0,-360) 204 50 0.3 (greyN 0.5) "Main Menu" False ([("ButtonQuit", GUIElem (IconButton (0, -120) 100 50 butQuit False)),("Button1", GUIElem (SpecialIconButton (0, 120) 90 90 level1ico False (replaceLevel initGame level1))),("Button2", GUIElem (SpecialIconButton (-130, 120) 90 90 level1ico False (replaceLevel initGame level2))), ("Button3", GUIElem (SpecialIconButton (130, 120) 90 90 level1ico False (replaceLevel initGame level3))),("Button4", GUIElem (SpecialIconButton (70, 0) 90 90 level1ico False (replaceLevel initGame level4))),("Button5", GUIElem (SpecialIconButton (-70, 0) 90 90 level1ico False (replaceLevel initGame level5)))])))
          ,("Stats", GUIElem (TextBox (-385,-360) 500 150 (greyN 0.5) 30 ["Stats:", "Wave: 1", "Health = 100", "Coins = 100"]))
          ,("Tower1", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 720)), -360) 150 150 towerIcon3_1 False))
          ,("Tower2", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 880)), -360) 150 150 towerIcon2_1 False))
          ,("Tower3", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1040)), -360) 150 150 towerIcon1_1 False))
          ,("Update", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1455)), -322) 72 72 upgradeIcon False))
          ,("Sell", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1455)), -397) 72 72 sellIcon False))
          ,("Game", GUIElem (Game (0, 0 + (fromIntegral Config.controlPanelHeight)) (fromIntegral width) (fromIntegral height) picLib
              (GameState
                   (Level Blank [] [])
                   10
                   False
                   ""
                   10
                   [] 
                   Nothing
                   0)
            )
           )
         ]
    let initMainMenu = [("ButtonQuit", GUIElem (IconButton (0, -120) 100 50 butQuit False)),("Button1", GUIElem (SpecialIconButton (0, 120) 90 90 level2ico False (replaceLevel initGame level2))),("Button2", GUIElem (SpecialIconButton (-130, 120) 90 90 level1ico False (replaceLevel initGame level1))), ("Button3", GUIElem (SpecialIconButton (130, 120) 90 90 level3ico False (replaceLevel initGame level3))),("Button4", GUIElem (SpecialIconButton (70, 0) 90 90 level5ico False (replaceLevel initGame level5))),("Button5", GUIElem (SpecialIconButton (-70, 0) 90 90 level4ico False (replaceLevel initGame level4)))]
    runGUI (InWindow "Tower Defence" 
          (width, (height + (controlPanelHeight * 2) + menuPanelHeight))
          (100,  100))
          (greyN 0.25) 
          60
          (WindowParams width (height + (controlPanelHeight * 2) + menuPanelHeight) 1 1)
          initMainMenu
          handleEvents
          Main.updateObjects

handleEvents :: Event -> [(String, GUIElem)] -> [(String, GUIElem)]
handleEvents (EventKey (MouseButton LeftButton) Down _ pos) xs = if isJust clickedSpecial then (fromJust clickedSpecial) else map update xs
  where
    update ("Game",a ) | Just g <- unpackCast a =
             case clickedB of "Tower1" -> ("Game", GUIElem $ setPlacingTower g pos archerTower)
                              "Tower2" -> ("Game", GUIElem $ setPlacingTower g pos magicTower)
                              "Tower3" -> ("Game", GUIElem $ setPlacingTower g pos basicTower)
                              "Update" -> ("Game", GUIElem $ upgradeCurrentTower g)
                              "Sell" -> ("Game", GUIElem $ deleteCurrentTower g)
                              _ -> ("Game", a)
    update other = other
    clickedSpecial = foldl stSpecial Nothing xs
    stSpecial acc cur = if isJust checked then checked else acc
      where
        checked = case unpackCast (snd cur) of 
          Just (SpecialIconButton (x, y) w h icon hl it) -> if hl then Just it else Nothing
          Nothing -> case unpackCast (snd cur) of 
                        Just (SpecialTextButton (x, y) w h sf clr txt hl it) -> if hl then Just it else Nothing
                        Nothing -> Nothing
    clickedB = foldl st "None" xs
    st acc cur = if checked then (fst cur) else acc
      where
        checked = case unpackCast (snd cur) of 
          Just (IconButton (x, y) w h icon hl) -> hl
          Nothing -> False
handleEvents _ xs = xs

updateObjects :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
updateObjects time objs = map update newObjs
  where
    newObjs = maybe objs (\g -> if  (gameResult g) /= Playing then (newGO g) else objs) $ (lookup "Game" objs) >>= unpackCast
              where
                newGO g = nobjs ++ [("EndText", GUIElem $ TextBox (0, 0) 500 100 (greyN 0.5) 20 ["Game over!" , "You " ++ (show $ gameResult g)])]
                nobjs = map st objs
                st ("Game", a) | Just g <- unpackCast a = ("Game", GUIElem $ pauseGame True g)
                st a = a
    update ("Stats", _) = ("Stats", GUIElem $ (TextBox (-385,-360) 500 150 (greyN 0.5) 30 (info ++ ["" ++ p1 ++ p2 ++ p3 ++ p4 ++ p5] )))
            where
              curObj = snd $ fromJust $ find (\(n, ob)-> n == "Game") objs
              Just game@(Game (x, y) w h assets GameState{..}) = unpackCast curObj
              
              curTower1Obj = snd $ fromJust $ find (\(n, ob)->  n == "Tower1") objs
              Just button1@(IconButton (_,_) _ _ _ hl1) = unpackCast curTower1Obj
              p1 = if hl1 then "Build price: " ++ (show $ price basicTower) else ""
              
              curTower2Obj = snd $ fromJust $ find (\(n, ob)->  n == "Tower2") objs
              Just button2@(IconButton (_,_) _ _ _ hl2) = unpackCast curTower2Obj
              p2 = if hl2 then "Build price: " ++ (show $ price magicTower) else ""
              
              curTower3Obj = snd $ fromJust $ find (\(n, ob)->  n == "Tower3") objs
              Just button3@(IconButton (_,_) _ _ _ hl3) = unpackCast curTower3Obj
              p3 = if hl3 then "Build price: " ++ (show $ price magicTower) else ""
              p4 = ""
              p5 = ""
              {-
              
              p4 = if (length objects /= 0) && hl4 then "Update price: " ++ (show $ price) else ""
                  where
                    price = upgradeCost $ head (filter (\x -> (getName x) == (selectedTower)) (objects))
                    curUpdateObj = snd $ fromJust $ find (\(n, ob)->  n == "Update") objs
                    Just button4@(IconButton (_,_) _ _ _ hl4) = unpackCast curUpdateObj
              
              p5 = if (length objects /= 0) && hl5 then "Sell price: " ++ (show $ price) else ""
                  where
                    price = sellCost $ head (filter (\x -> (getName x) == (selectedTower)) (objects))
                    curSellObj = snd $ fromJust $ find (\(n, ob)->  n == "Sell") objs
                    Just button5@(IconButton (_,_) _ _ _ hl5) = unpackCast curSellObj
              -}
              
              info = ["Waves left: " ++ show (wavesLeft game), "Coins: " ++ (show money), "Life: " ++ (show lives)]
    update other = other
