module SuperCow where

import Bonus
import Type
import Const
import Interface
import System.Random
import Graphics.Gloss.Interface.Pure.Game
   
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

-- | Инициализировать клевер
initClover :: Position -> Clover
initClover position = Clover
  { cloverPosition = position
  , cloverSize = defaultCloverSize
  }

-- | Инициализировать плохую птичку
initBadBird :: Position -> BadBird
initBadBird position = BadBird
  { badBirdPosition = position
  , badBirdSize = defaultBadBirdSize
  }

-- | Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird position = GoodBird
  { goodBirdPosition = position
  , goodBirdSize = defaultGoodBirdSize
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

-- | Инициализировать карту препятствий
initMap :: StdGen -> Map
initMap g = Map
  { mapGoodBirds = map initGoodBird goodBirdPositions
  , mapClovers = map initClover cloverPositions
  , mapBadBirds = map initBadBird badBirdPositions
  , mapBonusItems = map initBonusItem bonusItemTypePositions
  , obstacleSpeedGoodBird = originSpeedGoodBird
  , obstacleSpeedBadBird = originSpeedBadBird
  , obstacleSpeedClover = originSpeedClover
  , obstacleSpeedBonusItem = originSpeedBonusItem
  }
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (g9, g10) = split g4
    (g11, g12) = split g5
    (g13, g14) = split g6
    goodBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g14)) (randomRs obstacleHeightRange g7)
    cloverPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g8)) (randomRs obstacleHeightRange g9)
    badBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g10)) (randomRs obstacleHeightRange g11)
    bonusItemTypePositions = zipWith (\(x,y) z -> (x,y,z)) (zip
      (zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g12)) (randomRs obstacleHeightRange g13))
      (map intToBonusType (randomRs typeRange g))) (randomRs (0, 5) g)

-- | Инициализировать корову
initCow :: Cow
initCow = Cow
  { cowPosition = (cowInitOffset, cowInitHeight)
  , cowSize = defaultCowSize
  , cowSpeedUp = 0
  , cowSpeedLeft = 0
  , cowAngel = 0
  , cowSpeedAngel = 0
  , cowPushed = 0
  , cowBonus = InvincibleBonus Invincible { invincibleTime = 200, invincibleLife = 5 }
  }


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

-- | Передвижение коровы вверх и вниз, если можно.
goCowUpDown :: (Speed -> Speed) -> (Float -> Float) -> Int -> Cow -> Cow
goCowUpDown f fAngel flagPushed cow = cow { cowSpeedUp = f $ cowSpeedUp cow
                               , cowSpeedAngel = fAngel $ cowSpeedAngel cow
                               , cowPushed = flagPushed
                               }

-- | Передвижение коровы влево и вправо, если можно.
goCowLeftRight :: (Speed -> Speed) -> Cow -> Cow
goCowLeftRight f cow = cow { cowSpeedLeft = f $ cowSpeedLeft cow }

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

-- | Обновление коровы
updateCow :: Float -> Cow -> Cow
updateCow dt c = c
  { cowPosition = ((max screenLeft (min screenRight (coordX - dx)))
  ,(max screenBottom (min screenTop (coordY - dy))))
  , cowBonus = updateBonus dt (cowBonus c)
  , cowAngel = max minAngle (min maxAngle (cowAngel c + da))
  }
  where
    coordX = fst (cowPosition c)
    coordY = snd (cowPosition c)
    dx  = dt * (cowSpeedLeft c)
    dy = dt * (cowSpeedUp c)
    da = dt * (cowSpeedAngel c)


-- | Обновить карту игровой вселенной
updateMap :: Float -> Map -> Bonus -> Map
updateMap dt obstacleMap bonus = obstacleMap
  { mapGoodBirds = updateObstacles dt (mapGoodBirds obstacleMap)
    goodBirdSpeed
  , mapBadBirds = updateObstacles dt (mapBadBirds obstacleMap)
    badBirdSpeed
  , mapClovers = updateObstacles dt (mapClovers obstacleMap)
    cloverSpeed
  , mapBonusItems = updateObstacles dt (mapBonusItems obstacleMap)
    bonusSpeed
  , obstacleSpeedGoodBird = obstacleSpeedGoodBird obstacleMap + speedIncrease
  , obstacleSpeedBadBird = obstacleSpeedBadBird obstacleMap + speedIncrease
  , obstacleSpeedClover = obstacleSpeedClover obstacleMap + speedIncrease
  , obstacleSpeedBonusItem = obstacleSpeedBonusItem obstacleMap + speedIncrease
  }
  where
    goodBirdSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedGoodBird obstacleMap) * (goodBirdSpeedMultiplier bsc)
      _ -> obstacleSpeedGoodBird obstacleMap
    badBirdSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedBadBird obstacleMap) * (badBirdSpeedMultiplier bsc)
      _ -> obstacleSpeedBadBird obstacleMap
    cloverSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedClover obstacleMap) * (cloverSpeedMultiplier bsc)
      _ -> obstacleSpeedClover obstacleMap
    bonusSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedBonusItem obstacleMap) * (bonusSpeedMultiplier bsc)
      _ -> obstacleSpeedBonusItem obstacleMap


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

-- | Обновить препятствия игровой вселенной
updateObstacles :: Obstacle o => Float -> [o] -> Speed -> [o]
updateObstacles _ [] _ = []
updateObstacles dt obstacles speed =
  dropWhile (\o -> fst (getPosition o) < screenLeft)
    (map (\o -> setPosition o (coordX o - dx, coordY o)) obstacles)
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
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

-- | Обновить скорость движения коровы
updateSpeedCow :: (Cow -> Cow) -> Universe -> Universe
updateSpeedCow f u = u { universeCow = f $ universeCow u }

-- | Препятствия, которые не входят в экран
isOutsideScreen :: Obstacle o => o -> Bool
isOutsideScreen = not . isInsideScreen

-- | Препятствия, которые входят в экран
isInsideScreen :: Obstacle o => o -> Bool
isInsideScreen o = pos o < screenRight && pos o > screenLeft
    where
        pos = fst . getPosition

-- | Столкновения
-- | Сталкивается ли корова с любыми препятствиями
collisionMulti :: (Obstacle o, Obstacle c) => c -> [o] -> Bool
collisionMulti cow os = or (map (collides cow) (cropInsideScreen os))


getCollisionObstacle :: (Obstacle o, Obstacle c) => c -> [o] -> o
getCollisionObstacle cow os = (filter (collides cow) (cropInsideScreen os)) !! 0

-- | Сталкивается ли корова с препятствием?
collides :: (Obstacle o, Obstacle c) => c -> o -> Bool
collides cow o
  | crux >= oldx && cruy >= oldy && crdx >= oldx && crdy <= oldy &&
    clux <= oldx && cluy >= oldy = True
  | crdx >= olux && crdy <= oluy && crux >= olux && cruy >= oluy &&
    cldx <= olux && cldy <= oluy = True
  | crdx >= oldx && crdy >= oldy && crux >= olux && cruy <= oluy &&
    clux <= olux && cluy >= oluy = True
  | otherwise = False
  where
    (x1,y1) = getPosition cow
    (x2,y2) = getPosition o
    s1 = getSize cow
    s2 = getSize o
    (clux, cluy) = (x1 - (getWidth cow) / 2 * s1, y1 +
      (getHeight cow) / 2 * s1)
    (cldx, cldy) = (x1 - (getWidth cow) / 2 * s1, y1 -
      (getHeight cow) / 2 * s1)
    (crux, cruy) = (x1 + (getWidth cow) / 2 * s1, y1 +
      (getHeight cow) / 2 * s1)
    (crdx, crdy) = (x1 + (getWidth cow) / 2 * s1, y1 -
      (getHeight cow) / 2 * s1)
    (olux, oluy) = (x2 - (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    (oldx, oldy) = (x2 - (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)
    -- (orux, oruy) = (x2 + (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    -- (ordx, ordy) = (x2 + (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)

-- |  Удаления обьекта, с которым столкнулись
collisionHandle :: (Obstacle c) => Map -> c -> Bonus -> Map
-- collisionHandle m c (InvincibleBonus i) = m
collisionHandle m c b = m
  { mapClovers = filter (collidesVary b) (cropInsideScreen (mapClovers m)) ++
   dropWhile isInsideScreen (mapClovers m)
  , mapBadBirds = filter (collidesVary b) (cropInsideScreen (mapBadBirds m)) ++
   dropWhile isInsideScreen (mapBadBirds m)
  , mapGoodBirds = filter (collidesVary b) (cropInsideScreen (mapGoodBirds m)) ++
   dropWhile isInsideScreen (mapGoodBirds m)
  , mapBonusItems = filter (not . collides c) (cropInsideScreen (mapBonusItems m)) ++
   dropWhile isInsideScreen (mapBonusItems m)
  }
  where
      collidesVary (InvincibleBonus _) = (\_ -> True)
      collidesVary _ = not . collides c
  
-- |  Удаления обьекта, с которым столкнулись
badCollisionHandle :: (Obstacle c) => Map -> c -> Bonus -> Map
badCollisionHandle m _ (InvincibleBonus _) = m
badCollisionHandle m c _ = m
  { mapBadBirds = filter (not . collides c) (cropInsideScreen (mapBadBirds m)) ++
   dropWhile isInsideScreen (mapBadBirds m)
  , mapGoodBirds = filter (not . collides c) (cropInsideScreen (mapGoodBirds m)) ++
   dropWhile isInsideScreen (mapGoodBirds m)
  }



moveObstacles :: Map -> Float -> Map
moveObstacles m count = m
    { mapClovers = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapClovers m)
    , mapGoodBirds = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapGoodBirds m)
    , mapBadBirds = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapBadBirds m)
    , mapBonusItems = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapBonusItems m)
    }
    where
      newPosition (x, y) cnt = (x + cnt, y)


applyBonus :: Float -> Universe -> Universe
applyBonus dt u = case cowBonus $ universeCow (updateDonuts dt u) of
  InvincibleBonus i -> u { universeLife = invincibleLife i }
  DonutGunBonus dg -> collideDonuts (allDonuts dg) (updateDonuts dt u)
  -- { universeCow = updateDonuts dt (allDonuts $ cowBonus $ universeCow u)})
  _ -> u


removeCollidedDonuts :: Map -> Universe -> Universe
removeCollidedDonuts m u = case cowBonus $ universeCow u of
  DonutGunBonus dg -> u{ universeCow = (universeCow u) 
    { cowBonus = DonutGunBonus (dg
      { allDonuts = filter (\d -> 
        not ((collisionMulti d badBirds) || (collisionMulti d goodBirds))) (allDonuts dg) 
      })  
    }}
    where 
      badBirds = mapBadBirds m
      goodBirds = mapGoodBirds m
  _ -> u
    
collideDonuts :: [Donut] -> Universe -> Universe
collideDonuts [] u = u
collideDonuts (d:ds) u = case cowBonus $ universeCow u of 
  DonutGunBonus _ -> collideDonuts ds (u 
      { universeMap = badCollisionHandle (universeMap u) d (cowBonus $ universeCow u)})
  _ -> u

updateDonuts :: Float -> Universe -> Universe
updateDonuts dt u = u 
  { universeCow = cow 
    { cowBonus = case cowbonus of 
      DonutGunBonus dg -> DonutGunBonus (dg { allDonuts = cropInsideScreen (updateDonutPositions 
        dt (cowPosition cow) (allDonuts dg) (donutSpeed dg) (timeBetweenDonuts dg) (universeScore u)) })
      _ -> NoBonus
    } 
  }
  where 
    cow = universeCow u
    cowbonus = cowBonus cow
