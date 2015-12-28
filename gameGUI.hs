{-# LANGUAGE RecordWildCards #-}

--module GameGUI where

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

main = initMainWindow

assetPic :: String -> IO Picture
assetPic picName = do
        let path = snd $ head $ filter (\(s, p) -> s == picName) picBase
        curIcon@(Bitmap _ _ _ _) <- loadBMP path
        return curIcon



initMainWindow :: IO()
initMainWindow = do
        towerIcon1 <- assetPic "Tower1"
        towerIcon2 <- assetPic "Tower2"
        towerIcon3 <- assetPic "Tower3"
        bulletIcon1 <- assetPic "Bullet1"
        bacgroundPic <- assetPic "Background1"
        let picLib = (Map.fromList [("tower1", towerIcon1)
                                   ,("tower2", towerIcon2)
                                   ,("tower3", towerIcon3)
                                   ,("bullet1", bulletIcon1)
                                   ,("Background1", bacgroundPic)
                                   ])
        runGUI 
         (InWindow "Tower Defence" 
         (width, (height + (controlPanelHeight * 2) + menuPanelHeight))
         (100,  100))
         
         (greyN 0.25) 
         60
         [("Menu", GUIElem (TextButton (-0,-360) 204 50 0.3 (greyN 0.5) "Main Menu" False))
          ,("Stats", GUIElem (TextBox (-385,-360) 500 150 (greyN 0.5) 30 ["Stats:", "Wave: 1", "Health = 100", "Coins = 100"]))
          ,("Tower1", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 720)), -360) 150 150 towerIcon1 False))
          ,("Tower2", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 880)), -360) 150 150 towerIcon2 False))
          ,("Tower3", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1040)), -360) 150 150 towerIcon3 False))
          ,("Game", GUIElem (Game (0, 0 + (fromIntegral Config.controlPanelHeight)) (fromIntegral width) (fromIntegral height) picLib
              (GameState
                   (Level bacgroundPic [(-200,-200), (-100, 0), (0, 100)] [(1, [basicEnemy, basicEnemy]), (5, [basicEnemy, basicEnemy])])
                   []
                   10000 
                   False
                   ""
                   100
                   [] 
                   Nothing
                   0)
            )
           )
         ]
          (handleEvents)
          (Main.updateObjects)

handleEvents :: Event -> [(String, GUIElem)] -> [(String, GUIElem)]
handleEvents (EventKey (MouseButton LeftButton) Down _ pos) xs = map update xs
  where
    update ("Game",a ) | Just g <- unpackCast a =
             case clickedB of "Tower1" -> ("Game", GUIElem $ setPlacingTower g pos basicTower)
                              "None" -> ("Game", a)
    update other = other
    
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

