module Config where

picBase = [("Tower1", "pic/tower1.bmp")
          ,("Tower2", "pic/tower2.bmp")
          ,("Tower3", "pic/tower3.bmp")
          ,("Enemy1", "pic/enemy1.bmp")
          ,("Enemy2", "pic/enemy2.bmp")
          ,("Enemy3", "pic/enemy3.bmp")
          ,("Enemy4", "pic/enemy4.bmp")
          ,("Background1", "pic/background.bmp")
          ,("Bullet1", "pic/bullet1.bmp")
          ,("Bullet2", "pic/bullet2.bmp")
          ,("Bullet3", "pic/bullet3.bmp")
          ]

--Main Window params
controlPanelHeight = 80 :: Int
menuPanelHeight = 0 :: Int
width = 1280 :: Int
height = 720 :: Int
hpHalfWidth = 100 :: Float
hpHalfHeight = 10 :: Float
hpTranslate = -50 :: Float
hpMargin = 2 :: Float
