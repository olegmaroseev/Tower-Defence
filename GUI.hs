module GUI where

import Graphics.Gloss

data TextButton = TextButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             String  --text
                             deriving Show

data IconButton = IconButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Picture --icon
                             deriving Show

data TextBox = TextBox Point   --centerPoint
                             Integer --width
                             Integer --height
                             [String] --contents
                             deriving Show

renderTButton :: TextButton -> Picture
renderTButton (TextButton (x, y) w h txtStr) = 
    let halfW = fromIntegral w / 2
        halfH = fromIntegral h / 2
        fr = 3
    in Translate x y $ Pictures [Color (greyN 0.0) $ Polygon [(-halfW - fr, -halfH - fr), (halfW + fr, -halfH - fr), 
                                                              (halfW + fr, halfH + fr), (-halfW - fr, halfH + fr)], 
                 Color (greyN 0.5) $ Polygon [(-halfW, -halfH), (halfW, -halfH), (halfW, halfH), (-halfW, halfH)],
                 Translate ((-6) * (fromIntegral $ length txtStr)) (-10) 
                   $ Scale ((fromIntegral (w ) / 800) ) (0.2) $ Color black $ text txtStr]
                 --scale ((fromIntegral w) / (fromIntegral $ length txtStr) * 3) (15 / (fromIntegral h)) $ Color black $ text txtStr]
    
renderIButton :: IconButton -> Picture
renderIButton (IconButton (x, y) w h icon) = do
     Pictures [Translate x y $ scale 1 1 icon]
{-
renderTextBox :: TextBox -> Picture
renderTextBox (TextBox (x, y) w h strs) = do
     let halfW = fromIntegral w / 2
         halfH = fromIntegral h / 2
     in Translate x y $ Pictures [Color (greyN 0.5) $ Polygon [(-halfW, -halfH), (halfW, -halfH), (halfW, halfH), (-halfW, halfH)], 
                 Translate ((-6) * (fromIntegral $ length txtStr)) (-10) 
                   $ Scale ((fromIntegral (w ) / 800) ) (0.2) $ Color black $ text txtStr]
-}
