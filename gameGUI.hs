--module GameGUI where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Codec.BMP
import System.Environment
import GUI

controlPanelHeight = 80 :: Int
menuPanelHeight = 0 :: Int
backgroundPath = "pic/background.bmp"
tower1Path = "pic/tower1.bmp"
tower2Path = "pic/tower2.bmp"
tower3Path = "pic/tower3.bmp"

main = initMainWindow backgroundPath tower1Path tower2Path tower3Path

{-
main = play (InWindow "Tower Defence""
                 (windowSizeOfWorld world) (10, 10))
        black 
        10 -- Number of simulation steps to take for each second of real time.
        (initState world)
        drawState
        (handleInput world)
        (\_ -> id)
-}

initMainWindow :: String -> String -> String -> String -> IO()
initMainWindow fileName towerIcon1Filename towerIcon2Filename towerIcon3Filename
 = do   picture@(Bitmap width height _ _) <- loadBMP fileName
        towerIcon1@(Bitmap _ _ _ _) <- loadBMP towerIcon1Filename
        towerIcon2@(Bitmap _ _ _ _) <- loadBMP towerIcon2Filename
        towerIcon3@(Bitmap _ _ _ _) <- loadBMP towerIcon3Filename
        
        animate (
         InWindow "Tower Defence" 
         (width, (height + (controlPanelHeight * 2) + menuPanelHeight)) 
         (100,  100))
         (greyN 0.25) 
         (frame 
            picture 
            (TextButton (-0,-360) 100 30 "Main Menu")
            (TextButton (-385,-360) 500 150 "Stats")
            (IconButton (((fromIntegral width) - (fromIntegral 720)), -360) 150 150 towerIcon1)
            (IconButton (((fromIntegral width) - (fromIntegral 880)), -360) 150 150 towerIcon2)
            (IconButton (((fromIntegral width) - (fromIntegral 1040)), -360) 150 150 towerIcon3)
           )

frame :: Picture -> TextButton -> TextButton -> IconButton -> IconButton -> IconButton -> Float -> Picture
frame picture txtBtn txtMenuBtn iconBtn1 iconBtn2 iconBtn3 t = Pictures 
                [
                (translate 0 ( fromIntegral (controlPanelHeight - menuPanelHeight)) picture), 
                (renderTButton txtBtn),
                (renderTButton txtMenuBtn),
                (renderIButton iconBtn1),
                (renderIButton iconBtn2),
                (renderIButton iconBtn3)
                ]

