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
    butQuit@(Bitmap _ _ _ _) <- loadBMP butQuitMin
    towerIcon1 <- assetPic "Tower1"
    towerIcon2 <- assetPic "Tower2"
    towerIcon3 <- assetPic "Tower3"
    enemyIcon1 <- assetPic "Enemy1"
    enemyIcon2 <- assetPic "Enemy2"
    enemyIcon3 <- assetPic "Enemy3"
    enemyIcon4 <- assetPic "Enemy4"
    bulletIcon1 <- assetPic "Bullet1"
    bulletIcon2 <- assetPic "Bullet2"
    bulletIcon3 <- assetPic "Bullet3"
    bacgroundPic <- assetPic "Background1"
    let picLib = (Map.fromList [("tower1", towerIcon1)
                               ,("tower2", towerIcon2)
                               ,("tower3", towerIcon3)
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
    let initGame = [("Menu", GUIElem (TextButton (-0,-360) 204 50 0.3 (greyN 0.5) "Main Menu" False))
          ,("Stats", GUIElem (TextBox (-385,-360) 500 150 (greyN 0.5) 30 ["Stats:", "Wave: 1", "Health = 100", "Coins = 100"]))
          ,("Tower1", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 720)), -360) 150 150 towerIcon1 False))
          ,("Tower2", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 880)), -360) 150 150 towerIcon2 False))
          ,("Tower3", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1040)), -360) 150 150 towerIcon3 False))
          ,("Game", GUIElem (Game (0, 0 + (fromIntegral Config.controlPanelHeight)) (fromIntegral width) (fromIntegral height) picLib
              (GameState
                   (Level Blank [] [])
                   []
                   1000 
                   False
                   ""
                   100
                   [] 
                   Nothing
                   0)
            )
           )
         ]
    let initMainMenu = [("ButtonQuit", GUIElem (IconButton (0, -120) 100 50 butQuit False)),("Button1", GUIElem (SpecialIconButton (0, 120) 90 90 level1ico False (replaceLevel initGame level1))),("Button2", GUIElem (IconButton (-130, 120) 90 90 level1ico False)), ("Button3", GUIElem (IconButton (130, 120) 90 90 level1ico False)),("Button4", GUIElem (IconButton (70, 0) 90 90 level1ico False)),("Button5", GUIElem (IconButton (-70, 0) 90 90 level1ico False))]
    runGUI (InWindow "Tower Defence" 
          (width, (height + (controlPanelHeight * 2) + menuPanelHeight))
          (100,  100))
          (greyN 0.25) 
          60
          initMainMenu
          handleEvents
          updateObjects
  where
    handleEvents :: Event -> [(String, GUIElem)] -> [(String, GUIElem)]
    handleEvents (EventKey (MouseButton LeftButton) Down _ pos) xs = if isJust clickedSpecial then (fromJust clickedSpecial) else map update xs
      where
        update ("Game",a ) | Just g <- unpackCast a =
                 case clickedB of "Tower1" -> ("Game", GUIElem $ setPlacingTower g pos basicTower)
                                  "None" -> ("Game", a)
        update other = other
        clickedSpecial = foldl stSpecial Nothing xs
        stSpecial acc cur = if isJust checked then checked else acc
          where
            checked = case unpackCast (snd cur) of 
              Just (SpecialIconButton (x, y) w h icon hl it) -> Just it
              Nothing -> Nothing
        clickedB = foldl st "None" xs
        st acc cur = if checked then (fst cur) else acc
          where
            checked = case unpackCast (snd cur) of 
              Just (IconButton (x, y) w h icon hl) -> hl
              Nothing -> False
    handleEvents _ xs = xs
    updateObjects :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
    updateObjects time objs = map update objs
      where
        update ("Stats", _) = ("Stats", GUIElem $ (TextBox (-385,-360) 500 150 (greyN 0.5) 30 (["Stats:"] ++ info)))
                where
                  curObj = snd $ fromJust $ find (\(n, ob)-> n == "Game") objs
                  Just game@(Game (x, y) w h assets GameState{..}) = unpackCast curObj
                  info = ["Coins: " ++ (show money), "Life: " ++ (show lives)]
        update other = other
