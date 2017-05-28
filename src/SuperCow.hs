module SuperCow where

import Interface
import Graphics.Gloss.Interface.Pure.Game
import Game.Player
import Game.Bonus
import Game.Obstacle
import Game.Universe
import Type
import Const
import System.Random

-- | Запустить игру «Super Cow»
runSuperCow :: Images -> IO ()
runSuperCow images = do
  g <- newStdGen
  play display bgColor fps (applyGameMode $ initUniverse g) (drawUniverse images)
    handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60     -- кол-во кадров в секунду

-- | Инициализация вселенной
-- | Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeMap = moveObstacles (initMap g) 1500
  , universeCow = initCow
  , universeScore = 0
  , universeLife  = 5
  , universeStop = False
  , universeGameOver = False
  , universeBackground = initBackground
  , universeMode = OrdinaryMode 1
  }

initBackgroundPicture :: Offset -> BackgroundPicture
initBackgroundPicture offset = BackgroundPicture
  { backgroundPicturePosition = (offset, screenTop)
  -- , backgroundPictureSize = (2560, 1440)
  }

initBackground :: Background
initBackground = Background
  { mapBackgroundPicture = map initBackgroundPicture backgroundPicturePositions
  , backgroundPictureSpeed = backgroundSpeed
  }
  where
    backgroundPicturePositions = [screenLeft, screenLeft + 800 ..]

applyGameMode :: Universe -> Universe 
applyGameMode u = case universeMode u of
  (OrdinaryMode _) -> u
  (NightmareMode _) -> u 
    { universeMap = (universeMap u) 
      { mapClovers = []
      , mapBonusItems = [] 
      }
    }
  (BossMode _) -> u 
    { universeMap = (universeMap u) 
      { mapGoodBirds = []
      , mapBadBirds = [] 
      }
    }
  (NoBonusMode _) -> u 
    { universeMap = (universeMap u) { mapBonusItems = [] } }
   
-- | Взаимодействия c игровой вселенной
-- | Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) = updateSpeedCow
  (goCowUpDown (subtract cowSpeed) (+cowAngelDefault) 1)
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) = updateSpeedCow
  (goCowUpDown (+cowSpeed) (subtract cowAngelDefault) 2)
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) = updateSpeedCow
  (goCowUpDown (+cowSpeed) (subtract cowAngelDefault) 0)
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) = updateSpeedCow
  (goCowUpDown (subtract cowSpeed) (+cowAngelDefault) 0)
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = updateSpeedCow
  (goCowLeftRight (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = updateSpeedCow
  (goCowLeftRight (subtract cowSpeed))
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) = updateSpeedCow
  (goCowLeftRight (subtract cowSpeed))
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) = updateSpeedCow
  (goCowLeftRight (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = toggleGame
handleUniverse _ = id



toggleGameHardness :: Universe -> Int -> Universe
toggleGameHardness u _ = u

toggleObstacleHardness :: Obstacle o => [o] -> (o -> o) -> [o]
toggleObstacleHardness os _ = os

-- | Проверка на конец игры
negativeLifeBalance :: Universe -> Bool
negativeLifeBalance u = life <= 0
  where
    life = (universeLife u)

-- | Изменить активность игры
toggleGame :: Universe -> Universe
toggleGame u
  | universeLife u > 0 = u
    { universeStop = not stopFlag
    , universeGameOver = False
    }
  | otherwise = u
    { universeStop = not stopFlag
    , universeLife = 5
    , universeScore = 0
    , universeGameOver = not gameOverFlag
    , universeMap = moveObstacles m
    { obstacleSpeedGoodBird = originSpeedGoodBird
    , obstacleSpeedBadBird = originSpeedBadBird
    , obstacleSpeedClover = originSpeedClover
    , obstacleSpeedBonusItem = originSpeedBonusItem
    } 1000
    , universeCow = cow
    { cowPosition = (cowInitOffset, cowInitHeight)
    , cowBonus = InvincibleBonus Invincible { invincibleLife = 5, invincibleTime = defaultCollapseTime }
    }
    }
  where
    stopFlag = (universeStop u)
    m = (universeMap u)
    cow = (universeCow u)
    gameOverFlag = (universeGameOver u)

updateBackground :: Float -> Background -> Background
updateBackground dt background = background
  { mapBackgroundPicture = updateBackgrounds dt (mapBackgroundPicture background)
                            (backgroundPictureSpeed background)
  }

-- | Обновить фон
updateBackgrounds :: Float -> [BackgroundPicture] -> Speed -> [BackgroundPicture]
updateBackgrounds _ [] _ = []
updateBackgrounds dt backgrounds speed =
  dropWhile (\o -> fst (backgroundPicturePosition o) < screenLeft)
    (map (\o -> o { backgroundPicturePosition = (coordX o - dx, coordY o)}) backgrounds)
  where
    coordX = fst . backgroundPicturePosition
    coordY = snd . backgroundPicturePosition
    dx  = dt * speed

-- | Обновить счет
updateScore :: Float -> Score -> Score
updateScore _ score = score + 1

-- | Обновить жизни
updateLife :: Float -> Universe -> Universe
updateLife _ u
  | collisionMulti cow (mapClovers obstacleMap) = u
  { universeLife = checkLife life
  , universeMap = collisionHandle obstacleMap cow (cowBonus cow)
  }
  | collisionMulti cow (mapGoodBirds obstacleMap) = u
    { universeLife = life - (tryChangeLife (cowBonus cow) 1)
    , universeMap = collisionHandle obstacleMap cow (cowBonus cow)
    , universeCow = cow { cowBonus = tryAddInvincibleBonus (cowBonus cow) (life - 1) }
    }
  | collisionMulti cow (mapBadBirds obstacleMap) = u
    { universeLife = (life - (tryChangeLife (cowBonus cow) 2))
    , universeMap = collisionHandle obstacleMap cow (cowBonus cow)
    , universeCow = cow { cowBonus = tryAddInvincibleBonus (cowBonus cow) (life - 2) }
    }
  | collisionMulti cow (mapBonusItems obstacleMap) = u
    { universeMap = collisionHandle obstacleMap cow (cowBonus cow)
    , universeCow = cow { cowBonus = initBonus (bonusItemType (getCollisionObstacle cow (mapBonusItems obstacleMap))) u }
    }
  | otherwise = u
  where
    life = (universeLife u)
    cow = (universeCow u)
    obstacleMap = (universeMap u)
    checkLife 5 = 5
    checkLife l = l + (tryChangeLife (cowBonus cow) 1)
    tryChangeLife (InvincibleBonus _) _ = 0
    tryChangeLife _ n = n

-- | Обновление игровой вселенной
-- | Обновить состояние игровой вселенной
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | gameStopped == True = u
  | negativeLifeBalance u = toggleGame u
  | otherwise = applyBonus dt (updateLife dt (u
    { universeMap  = updateMap dt (universeMap u) (cowBonus (universeCow u))
    , universeCow = updateCow dt (universeCow u)
    , universeScore  = updateScore dt (universeScore u)
    , universeBackground = updateBackground dt (universeBackground u)
    }))
  where
    gameStopped = (universeStop u)
