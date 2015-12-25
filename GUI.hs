{-# LANGUAGE  ExistentialQuantification, ScopedTypeVariables #-}

module GUI where

import Data.Typeable
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Codec.BMP


type EventHandler a = Event -> a -> a

defaultHandler :: EventHandler a
defaultHandler _ = id

class GUIObject a where
  renderObject :: a -> Picture
  updateObject :: Float -> a -> a
  updateObject _ = id
  eventHandler :: EventHandler a
  eventHandler = defaultHandler

data GUIElem = forall a. (Typeable a, GUIObject a) => GUIElem a

runGUI :: Display -> Color -> Int -> [(String, GUIElem)] ->
                          (Event -> [(String, GUIElem)] -> [(String, GUIElem)]) -> 
                          (Float -> [(String, GUIElem)] -> [(String, GUIElem)]) -> IO ()
runGUI display backColor simResolution
        objects handleEvents updateWorld = 
        play display backColor simResolution objects renderGUI
        (\event -> (handleEvents event). (handleGUIEvents event))
        (\time -> (updateWorld time) . (updateGUI time))
        
renderGUI :: [(String, GUIElem)] -> Picture
renderGUI objects = Pictures $ map (renderElem.snd) objects
        
handleGUIEvents :: Event -> [(String, GUIElem)] -> [(String, GUIElem)]
handleGUIEvents event objects = map (eventHandlerElem event) objects
        
updateGUI :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
updateGUI time objects = map (updateElem time) objects

tower1Path = "pic/tower1.bmp"

renderElem :: GUIElem -> Picture
renderElem (GUIElem a) = renderObject a

updateElem :: Float -> (String, GUIElem) -> (String, GUIElem)
updateElem time (name, GUIElem a) = (name, GUIElem $ updateObject time a)

eventHandlerElem :: EventHandler (String, GUIElem)
eventHandlerElem event (name, GUIElem a) = (name, GUIElem $ eventHandler event a)

testGUI :: IO()
testGUI = do
        towerIcon1@(Bitmap _ _ _ _) <- loadBMP tower1Path
        runGUI 
         (InWindow "Tower Defence" 
         (600, 400) 
         (100,  100))
         (greyN 0.25) 
         30
         [("TestButton1", GUIElem (TextButton (0, 0) 100 50 (greyN 0.5) "HELLO" False)),
         ("TestButton2", GUIElem (IconButton (100, 0) 150 150 towerIcon1))]
         (\_ -> id)
         updateAll--(\_ -> id)

updateAll :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
updateAll time xs = map update xs
  where
    update ("TestButton1", GUIElem a) = ("TestButton1", GUIElem $ TextButton (x-time*2, y) w h c t hl)
      where
        Just (TextButton (x, y) w h c t hl) = cast a
    update other = other
         
data TextButton = TextButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Color   --color
                             String  --text
                             Bool -- Highlighted
                             deriving (Show, Typeable)

data IconButton = IconButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Picture --icon
                             deriving (Show, Typeable)

data TextBox = TextBox Point   --centerPoint
                       Integer --width
                       Integer --height
                       String  --contents
                       deriving (Show, Typeable)
contains :: Point -> (Integer, Integer) -> Point -> Bool
contains (x, y) (w, h) (tx, ty) = (ty >= bottom) && (ty <= top) && (tx >= left) && (tx <= right)
  where
    halfW = (fromIntegral w) / 2
    halfH = (fromIntegral h) / 2
    bottom = y - halfH
    left = x - halfW
    top = y + halfH
    right = x + halfW
                       
instance GUIObject TextButton where
  renderObject = renderTButton
  eventHandler (EventMotion ep) tb@(TextButton p w h c t hl)
    | (not hl) && contains p (w,h) ep = TextButton p w h c t True
    | hl && (not $ contains p (w,h) ep) = TextButton p w h c t False
    | otherwise = tb
  eventHandler _ o = o

instance GUIObject IconButton where
  renderObject = renderIButton
                       
renderTButton :: TextButton -> Picture
renderTButton (TextButton (x, y) w h c txtStr hl) = 
    let halfW = fromIntegral w / 2
        halfH = fromIntegral h / 2
        fr = 3
    in Translate x y $ Pictures $ [Color c $ Polygon [(-halfW - fr, -halfH - fr), (halfW + fr, -halfH - fr), 
                                                              (halfW + fr, halfH + fr), (-halfW - fr, halfH + fr)], 
                 Color c $ Polygon [(-halfW, -halfH), (halfW, -halfH), (halfW, halfH), (-halfW, halfH)],
                 Translate ((-6) * (fromIntegral $ length txtStr)) (-10) 
                   $ Scale ((fromIntegral (w ) / 800) ) (0.2) $ Color black $ text txtStr] ++ (if hl then
                   [Color (makeColor 1 1 1 0.5) $ Polygon [(-halfW - fr, -halfH - fr), (halfW + fr, -halfH - fr), 
                                                              (halfW + fr, halfH + fr), (-halfW - fr, halfH + fr)]]
                                                              else [])
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
