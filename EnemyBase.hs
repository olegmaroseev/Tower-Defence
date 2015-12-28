module EnemyBase where

import GameLogic

getEnemy::String -> Enemy
getEnemy "basicEnemy" = basicEnemy
getEnemy "basicUpdatedEnemy" = basicUpdatedEnemy
getEnemy "middleEnemy" = middleEnemy
