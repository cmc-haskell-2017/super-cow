-- | Препятствие
module Game.Obstacle where

import Game.Bonus
import Type
import Const 
import System.Random

-----------------------------
-- * Модель препятствий 
-----------------------------

-- | Карта препятствий
data Map = Map
  { mapGoodBirds           :: [GoodBird]  -- ^ Все хорошие птички
  , mapBadBirds            :: [BadBird]   -- ^ Все плохие птички
  , mapClovers             :: [Clover]    -- ^ Все клеверы
  , mapBonusItems          :: [BonusItem] -- ^ Все бонусы
  , obstacleSpeedGoodBird  :: Speed       -- ^ Скорость хороших птичек
  , obstacleSpeedBadBird   :: Speed       -- ^ Скорость плохих птичек
  , obstacleSpeedClover    :: Speed       -- ^ Скорость клеверов
  , obstacleSpeedBonusItem :: Speed       -- ^ Скорость бонусов
  }

-- | Клевер - добавляет одну жизнь
data Clover = Clover
  { cloverPosition :: Position -- ^ Положение в пространстве
  , cloverSize     :: Size     -- ^ Размер
  }

-- | Плохая птичка - снимает 2 жизни
data BadBird = BadBird
  { badBirdPosition :: Position -- ^ Положение в пространстве
  , badBirdSize     :: Size     -- ^ Размер
  }

-- | Хорошая птичка - снимает 1 жизни
data GoodBird = GoodBird
  { goodBirdPosition :: Position -- ^ Положение в пространстве
  , goodBirdSize     :: Size     -- ^ Размер
  }


-- | Реализация класса типов - препятствие
class Obstacle o where
  getPosition :: o -> Position       -- ^ Получить позицию
  
  getSize     :: o -> Size           -- ^ Получить размер
  
  setPosition :: o -> Position -> o  -- ^ Изменить позицию
  
  setSize     :: o -> Size -> o      -- ^ Изменить размер
  
  getHeight   :: o -> Size           -- ^ Получить длинну
  
  getWidth    :: o -> Size           -- ^ Поулчить ширину

-- | Препятствие - клевер
instance Obstacle Clover where
  getPosition                 = cloverPosition

  getSize                     = cloverSize

  setPosition clover position = clover { cloverPosition = position }

  setSize clover size         = clover { cloverSize = size }

  getHeight _                 = 50

  getWidth _                  = 50

-- | Препятствие - бонус
instance Obstacle BonusItem where
  getPosition                    = bonusItemPosition

  getSize                        = bonusItemSize

  setPosition bonusItem position = bonusItem { bonusItemPosition = position }

  setSize bonusItem size         = bonusItem { bonusItemSize = size }

  getHeight _                    = 68

  getWidth _                     = 68

-- | Препятствие - плохая птичка
instance Obstacle BadBird where
  getPosition                  = badBirdPosition

  getSize                      = badBirdSize

  setPosition badBird position = badBird { badBirdPosition = position }

  setSize badBird size         = badBird { badBirdSize = size }

  getWidth _                   = 81

  getHeight _                  = 42

-- | Препятствие - хорошая птичка
instance Obstacle GoodBird where
  getPosition                   = goodBirdPosition

  getSize                       = goodBirdSize

  setPosition goodBird position = goodBird { goodBirdPosition = position }

  setSize goodBird size         = goodBird { goodBirdSize = size }

  getWidth _                    = 67

  getHeight _                   = 36

-- | Препятствие - пончик
instance Obstacle Donut where
  getPosition                = donutPosition

  getSize                    = donutSize

  setPosition donut position = donut { donutPosition = position }

  setSize donut size         = donut { donutSize = size }

  getWidth _                 = 67

  getHeight _                = 36

-- | Инициализировать клевер
initClover :: Position -> Clover
initClover position = Clover
  { cloverPosition = position
  , cloverSize     = defaultCloverSize
  }

-- | Инициализировать плохую птичку
initBadBird :: Position -> BadBird
initBadBird position = BadBird
  { badBirdPosition = position
  , badBirdSize     = defaultBadBirdSize
  }

-- | Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird position = GoodBird
  { goodBirdPosition = position
  , goodBirdSize     = defaultGoodBirdSize
  }

-- | Инициализировать пончик
initDonut :: Position -> Donut
initDonut position = Donut
  { donutPosition = position
  , donutSize = defaultDonutSize
  }

-- | Инициализировать карту препятствий
initMap :: StdGen -> Map
initMap g = Map
  { mapGoodBirds           = map initGoodBird goodBirdPositions
  , mapClovers             = map initClover cloverPositions
  , mapBadBirds            = map initBadBird badBirdPositions
  , mapBonusItems          = map initBonusItem bonusItemTypePositions
  , obstacleSpeedGoodBird  = originSpeedGoodBird
  , obstacleSpeedBadBird   = originSpeedBadBird
  , obstacleSpeedClover    = originSpeedClover
  , obstacleSpeedBonusItem = originSpeedBonusItem
  }
  where
    (g1, g2)               = split g
    (g3, g4)               = split g1
    (g5, g6)               = split g2
    (g7, g8)               = split g3
    (g9, g10)              = split g4
    (g11, g12)             = split g5
    (g13, g14)             = split g6
    goodBirdPositions      = zip 
      (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g14)) 
      (randomRs obstacleHeightRange g7)
    cloverPositions        = zip 
      (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g8)) 
      (randomRs obstacleHeightRange g9)
    badBirdPositions       = zip 
      (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g10))
      (randomRs obstacleHeightRange g11)
    bonusItemTypePositions = zipWith (\(x,y) z -> (x,y,z)) (zip
      (zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..]
      (randomRs obstacleOffsetRange g12)) 
      (randomRs obstacleHeightRange g13))
      (map intToBonusType (randomRs typeRange g))) 
      (randomRs (0, 5) g)

-- | Обновить карту игровой вселенной
updateMap :: Float -> Map -> Bonus -> Map
updateMap dt obstacleMap bonus = obstacleMap
  { mapGoodBirds           = updateObstacles dt (mapGoodBirds obstacleMap)
    goodBirdSpeed
  , mapBadBirds            = updateObstacles dt (mapBadBirds obstacleMap)
    badBirdSpeed
  , mapClovers             = updateObstacles dt (mapClovers obstacleMap)
    cloverSpeed
  , mapBonusItems          = updateObstacles dt (mapBonusItems obstacleMap)
    bonusSpeed
  , obstacleSpeedGoodBird  = obstacleSpeedGoodBird obstacleMap  + 
    speedIncrease
  , obstacleSpeedBadBird   = obstacleSpeedBadBird obstacleMap   + 
    speedIncrease
  , obstacleSpeedClover    = obstacleSpeedClover obstacleMap    + 
    speedIncrease
  , obstacleSpeedBonusItem = obstacleSpeedBonusItem obstacleMap + 
    speedIncrease
  }
  where
    goodBirdSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedGoodBird obstacleMap)  * 
        (goodBirdSpeedMultiplier bsc)
      _                        -> obstacleSpeedGoodBird obstacleMap
    badBirdSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedBadBird obstacleMap)   * 
        (badBirdSpeedMultiplier bsc)
      _                        -> obstacleSpeedBadBird obstacleMap
    cloverSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedClover obstacleMap)    * 
        (cloverSpeedMultiplier bsc)
      _                        -> obstacleSpeedClover obstacleMap
    bonusSpeed = case bonus of
      BirdSpeedChangeBonus bsc -> (obstacleSpeedBonusItem obstacleMap) * 
        (bonusSpeedMultiplier bsc)
      _                        -> obstacleSpeedBonusItem obstacleMap

-- | Обновить препятствия игровой вселенной
updateObstacles :: Obstacle o => Float -> [o] -> Speed -> [o]
updateObstacles _ [] _ = []
updateObstacles dt obstacles speed =
  dropWhile (\o -> fst (getPosition o) < screenLeft)
    (map (\o -> setPosition o (coordX o - dx, coordY o)) obstacles)
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
    dx     = dt * speed
    
-- | Подвинуть все препятствия за пределы экрана
moveObstacles :: Map -> Float -> Map
moveObstacles m count = m
  { mapClovers    = map (\o -> setPosition o (newPosition (getPosition o) count))
    (mapClovers m)
  , mapGoodBirds  = map (\o -> setPosition o (newPosition (getPosition o) count)) 
    (mapGoodBirds m)
  , mapBadBirds   = map (\o -> setPosition o (newPosition (getPosition o) count)) 
    (mapBadBirds m)
  , mapBonusItems = map (\o -> setPosition o (newPosition (getPosition o) count)) 
    (mapBonusItems m)
  }
  where
    newPosition (x, y) cnt = (x + cnt, y)      

-- | Обновить пончики
updateDonutPositions :: Float    -- ^ Изменение времени с прошлого кадра
                     -> Position -- ^ Положение коровы
                     -> [Donut]  -- ^ Список всех пончиков
                     -> Speed    -- ^ Скорость пончиков
                     -> Float    -- ^ Время(очки) между пончиками
                     -> Score    -- ^ Очки
                     -> [Donut]
updateDonutPositions dt cowpos ds speed time score
  | mod score (truncate time) == 0 = 
    map (\d -> setPosition d (coordX d + dx, coordY d)) 
    ([initDonut cowpos] ++ ds)
  | otherwise                      = 
    map (\d -> setPosition d (coordX d + dx, coordY d)) 
    ds
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
    dx     = dt * speed
            
-- | Оставить только те препятствия, которые входят в экран
cropInsideScreen :: (Obstacle o) => [o] -> [o]
cropInsideScreen obs = dropWhile (\o -> pos o < screenLeft) $
  takeWhile (\o -> pos o < screenRight) obs
  where
    pos = fst . getPosition

-- | Препятствия, которые не входят в экран
isOutsideScreen :: Obstacle o => o -> Bool
isOutsideScreen = not . isInsideScreen

-- | Препятствия, которые входят в экран
isInsideScreen :: Obstacle o => o -> Bool
isInsideScreen o = pos o < screenRight && pos o > screenLeft
  where
    pos = fst . getPosition
        
-----------------------
-- * Столкновения
-----------------------

-- | Сталкивается ли препятствие с любыми препятствиями
collisionMulti :: (Obstacle o, Obstacle c) => c -> [o] -> Bool
collisionMulti cow os = or (map (collides cow) (cropInsideScreen os))

-- | Обьект, с которым сталкивается препятствие
getCollisionObstacle :: (Obstacle o, Obstacle c) => c -> [o] -> o
getCollisionObstacle cow os = (filter (collides cow) (cropInsideScreen os)) !! 0

-- | Сталкивается ли препяствие с другим препятствием
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
    (x1,y1)      = getPosition cow
    (x2,y2)      = getPosition o
    s1           = getSize cow
    s2           = getSize o
    (clux, cluy) = (x1 - (getWidth cow) / 2 * s1, y1 +
      (getHeight cow) / 2 * s1)
    (cldx, cldy) = (x1 - (getWidth cow) / 2 * s1, y1 -
      (getHeight cow) / 2 * s1)
    (crux, cruy) = (x1 + (getWidth cow) / 2 * s1, y1 +
      (getHeight cow) / 2 * s1)
    (crdx, crdy) = (x1 + (getWidth cow) / 2 * s1, y1 -
      (getHeight cow) / 2 * s1)
    (olux, oluy) = (x2 - (getWidth o) / 2 * s2, y2 + 
      (getHeight o) / 2 * s2)
    (oldx, oldy) = (x2 - (getWidth o) / 2 * s2, y2 - 
      (getHeight o) / 2 * s2)
  -- (orux, oruy) = (x2 + (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
  -- (ordx, ordy) = (x2 + (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)

-- |  Удаления обьекта, с которым столкнулись
collisionHandle :: (Obstacle c) => Map -> c -> Bonus -> Map
collisionHandle m c b = m
  { mapClovers    = filter (collidesVary b) 
    (cropInsideScreen (mapClovers m))    ++
    dropWhile isInsideScreen (mapClovers m)
  , mapBadBirds   = filter (collidesVary b) 
    (cropInsideScreen (mapBadBirds m))   ++
    dropWhile isInsideScreen (mapBadBirds m)
  , mapGoodBirds  = filter (collidesVary b) 
    (cropInsideScreen (mapGoodBirds m))  ++
    dropWhile isInsideScreen (mapGoodBirds m)
  , mapBonusItems = filter (not . collides c) 
    (cropInsideScreen (mapBonusItems m)) ++
    dropWhile isInsideScreen (mapBonusItems m)
  }
  where
    collidesVary (InvincibleBonus _) = (\_ -> True)
    collidesVary _ = not . collides c

-- |  Удаления НЕГАТИВНОГО обьекта, с которым столкнулись
badCollisionHandle :: (Obstacle c) => Map -> c -> Bonus -> Map
badCollisionHandle m _ (InvincibleBonus _) = m
badCollisionHandle m c _                   = m
  { mapBadBirds  = filter (not . collides c) 
    (cropInsideScreen (mapBadBirds m))  ++
    dropWhile isInsideScreen (mapBadBirds m)
  , mapGoodBirds = filter (not . collides c) 
    (cropInsideScreen (mapGoodBirds m)) ++
    dropWhile isInsideScreen (mapGoodBirds m)
  }

-------------------
-- * Константы 
-------------------

-- | Размер клевера
defaultCloverSize :: Size
defaultCloverSize = 1.0

-- | Размер плохой птички
defaultBadBirdSize :: Size
defaultBadBirdSize = 1.0

-- | Размер хорошей птички
defaultGoodBirdSize :: Size
defaultGoodBirdSize = 1.0

-- | Размер пончика
defaultDonutSize :: Size
defaultDonutSize = 1.0

-- | Диапазон высот препятствий.
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (screenBottom, screenTop)

-- | Диапазон типов бонусов
typeRange :: (Int, Int)
typeRange = (1, 5)

-- | Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = screenRight * 1.5

-- | Диапазон, для вариации расстояния между препятсвтиями
obstacleOffsetRange :: (Offset, Offset)
obstacleOffsetRange = (-(defaultOffset / 2), defaultOffset / 2)

-- | Изначальная скорость хорошей птички
originSpeedGoodBird :: Speed
originSpeedGoodBird = 100

-- | Изначальная скорость плохо птички
originSpeedBadBird :: Speed
originSpeedBadBird = 200

-- | Изначальная скорость клевера
originSpeedClover :: Speed
originSpeedClover = 10

-- | Изначальная скорость бонуса
originSpeedBonusItem :: Speed
originSpeedBonusItem = 150

-- | Изначальная скорость пончика
defaultDonutSpeed :: Speed
defaultDonutSpeed = 1000
