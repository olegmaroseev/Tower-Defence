--module GameGUI where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Codec.BMP
import System.Environment
import GUI
import Config
import GameLogic

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
        bacgroundPic <- assetPic "Background1"
        runGUI 
         (InWindow "Tower Defence" 
         (width, (height + (controlPanelHeight * 2) + menuPanelHeight))
         (100,  100))
         
         (greyN 0.25) 
         30
         [("Menu", GUIElem (TextButton (-0,-360) 204 50 0.3 (greyN 0.5) "Main Menu" False))
          ,("Stats", GUIElem (TextBox (-385,-360) 500 150 (greyN 0.5) 30 ["Stats:", "Wave: 1", "Health = 100", "Coins = 100"]))
          ,("Tower1", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 720)), -360) 150 150 towerIcon1 False))
          ,("Tower2", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 880)), -360) 150 150 towerIcon2 False))
          ,("Tower3", GUIElem (IconButton (((fromIntegral width) - (fromIntegral 1040)), -360) 150 150 towerIcon3 False))
          ,("Game", GUIElem (Game (0, 0) (fromIntegral width) (fromIntegral height)
              (GameState
                   (Level bacgroundPic [(0,0)] [])
                   []
                   0 
                   False
                   ""
                   100
                   [] 
                   Nothing)
            )
           )
         ]
          (\_ -> id)
          updateAll

