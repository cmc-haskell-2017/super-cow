module SuperCow where

import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- | Запустить игру «Super Cow»
runSuperCow :: Images -> IO ()
runSuperCow images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) 
    handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60     -- кол-во кадров в секунду

-- | Загрузка изображений
loadImages :: IO Images
loadImages = do
  Just cowPicture              <- loadJuicyPNG "images/cow.png"
  Just cloverPicture           <- loadJuicyPNG "images/clover.png"
  Just goodBirdUpPicture       <- loadJuicyPNG "images/GrayBirdUp.png"
  Just goodBirdDownPicture     <- loadJuicyPNG "images/GrayBirdDown.png"
  Just badBirdUpPicture        <- loadJuicyPNG "images/BlueBirdUp.png"
  Just badBirdDownPicture      <- loadJuicyPNG "images/BlueBirdDown.png"
  Just skyWithGrassPicture     <- loadJuicyJPG "images/SkyWithGrass.jpg"

  return Images
    { imageCow          = scale 1.0 1.0 cowPicture
    , imageClover       = scale 1.0 1.0 cloverPicture
    , imageGoodBirdUp   = scale 1.0 1.0 goodBirdUpPicture
    , imageGoodBirdDown = scale 1.0 1.0 goodBirdDownPicture
    , imageBadBirdUp    = scale 1.0 1.0 badBirdUpPicture
    , imageBadBirdDown  = scale 1.0 1.0 badBirdDownPicture
    , imageSkyWithGrass = scale 1.0 1.0 skyWithGrassPicture
    }


-- | Структуры данных
-- | Высота и Положение объектов
type Height   = Float            -- ^ Высота обьекта
type Offset   = Float            -- ^ Сдвиг обьекта
type Position = (Offset,Height)  -- ^ Координаты обьекта
type Life     = Int              -- ^ Жизни (изначально 3)
type Score    = Int              -- ^ Счет (изменяется постоянно)
type Size     = Float            -- ^ Размер обьекта
type Speed    = Float            -- ^ Скорость обьекта

-- | Объекты игровой вселенной
-- | Клевер - добавляет одну жизнь
data Clover = Clover
  { cloverPosition :: Position
  , cloverSize     :: Size
  }
  
-- | Плохая птичка - снимает 2 жизни
data BadBird = BadBird
  { badBirdPosition :: Position
  , badBirdSize     :: Size
  }

-- | Хорошая птичка - снимает 1 жизни
data GoodBird = GoodBird 
  { goodBirdPosition :: Position
  , goodBirdSize     :: Size
  }

-- | Карта препятствий
data Map = Map
  { mapGoodBirds  :: [GoodBird]
  , mapBadBirds   :: [BadBird]
  , mapClovers    :: [Clover]
  , obstacleSpeed :: Speed
  }

-- | Корова
data Cow = Cow
  { cowPosition  :: Position
  , cowSize      :: Size
  , cowSpeedUp   :: Speed  -- ^ Cкорость по вертикали 
  , cowSpeedLeft :: Speed  -- ^ Cкорость по горизонтали
  }

-- | Игровая вселенная
data Universe = Universe
  { universeMap       :: Map    -- ^ Препятствия игровой вселенной
  , universeCow       :: Cow    -- ^ Корова
  , universeScore     :: Score  -- ^ Cчет
  , universeLife      :: Life   -- ^ Жизни
  , universeStop      :: Bool   -- ^ Флаг остановки игры
  }
  
-- | Изображения объектов
data Images = Images
  { imageCow             :: Picture   -- ^ Изображение коровы.
  , imageClover          :: Picture   -- ^ Изображение клевера
  , imageGoodBirdUp      :: Picture   -- ^ Изображение GrayBirdUp.
  , imageGoodBirdDown    :: Picture   -- ^ Изображение GrayBirdDown.
  , imageBadBirdUp       :: Picture   -- ^ Изображение BlueBirdUp.
  , imageBadBirdDown     :: Picture   -- ^ Изображение BlueBirdDown.
  , imageSkyWithGrass    :: Picture   -- ^ Изображение Неба.
  }


-- | Реализация класса типов - препятствие
class Obstacle o where
  -- | Получение позиции препятствия
  getPosition :: o -> Position
  -- | Получние размера препятствия
  getSize :: o -> Size
  -- | Установка позиции препятствия
  setPosition :: o -> Position -> o
  -- | Установка размера препятствия
  setSize :: o -> Size -> o
  -- | Высота рисунка препятствия
  getHeight :: o -> Size
  -- | Ширина рисунка препятствия
  getWidth :: o -> Size
  

-- | Препятствие - клевер
instance Obstacle Clover where    
  getPosition = cloverPosition
  
  getSize = cloverSize
  
  setPosition clover position = clover { cloverPosition = position }
  
  setSize clover size = clover { cloverSize = size }
  
  getHeight _ = 50
  
  getWidth _ = 50

-- | Препятствие - плохая птичка
instance Obstacle BadBird where
  getPosition = badBirdPosition 
  
  getSize = badBirdSize 
  
  setPosition badBird position = badBird { badBirdPosition = position }
  
  setSize badBird size = badBird { badBirdSize = size }
  
  getWidth _ = 81
  
  getHeight _ = 42
      
-- | Препятствие - хорошая птичка
instance Obstacle GoodBird where 
  getPosition = goodBirdPosition 
  
  getSize = goodBirdSize 

  setPosition goodBird position = goodBird { goodBirdPosition = position }
  
  setSize goodBird size = goodBird { goodBirdSize = size }
  
  getWidth _ = 67
  
  getHeight _ = 36

-- | Отрисовка игровой вселенной
-- | Отобразить игровую вселенную 
drawUniverse :: Images -> Universe -> Picture
drawUniverse images universe = pictures
  [ drawBackground (imageSkyWithGrass images)
  , drawObstacles images (universeMap universe)
  , drawCow (imageCow images) (universeCow universe)
  , drawScore (universeScore universe)
  , drawLife (universeLife universe)
  ]

-- | Отобразить фон
drawBackground :: Picture -> Picture
drawBackground image = translate (-w) h (scale 1.0 1.0 image)
  where 
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2
    
-- | Отобразить все препятствия игровой вселенной, вмещающихся в экран 
drawObstacles :: Images -> Map -> Picture
drawObstacles images obstacles = pictures
  [ pictures (map (draw (imageGoodBirdUp  images)) 
    (cropInsideScreen (mapGoodBirds obstacles)))
  , pictures (map (draw (imageBadBirdUp images)) 
    (cropInsideScreen (mapBadBirds obstacles)))
  , pictures (map (draw (imageClover  images)) 
    (cropInsideScreen (mapClovers obstacles)))
  ]

-- | Нарисовать корову 
drawCow :: Picture -> Cow -> Picture
drawCow image cow = translate x y (scale r r image)
  where
    (x, y) = cowPosition cow
    r = cowSize cow

-- | Нарисовать счёт в левом верхнем углу экрана 
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show score)))) ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать жизни в правом верхнем углу экрана 
drawLife :: Life -> Picture
drawLife life = translate w h (scale 30 30 (pictures
  [ translate (-2) (-1.5) (scale 0.01 0.01 (color red (text (show life)))) ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать препятствие    
draw :: Obstacle o => Picture -> o -> Picture
draw image o = translate x y (scale r r image)
  where
    (x, y) = getPosition o
    r = getSize o

-- | Инициализация вселенной 
-- | Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeMap = initMap g 
  , universeCow = initCow
  , universeScore = 0
  , universeLife  = 3
  , universeStop = False
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

-- | Инициализировать карту препятствий 
initMap :: StdGen -> Map
initMap g = Map 
  { mapGoodBirds = map initGoodBird goodBirdPositions
  , mapClovers = map initClover cloverPositions
  , mapBadBirds = map initBadBird badBirdPositions
  , obstacleSpeed = gameSpeed
  }
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (g9, g10) = split g4
    goodBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g8)) (randomRs obstacleHeightRange g5)
    cloverPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g9)) (randomRs obstacleHeightRange g6)
    badBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g10)) (randomRs obstacleHeightRange g7)

-- | Инициализировать корову 
initCow :: Cow
initCow = Cow 
  { cowPosition = (cowInitOffset, cowInitHeight)
  , cowSize = defaultCowSize
  , cowSpeedUp = 0
  , cowSpeedLeft = 0
  }

-- | Взаимодействия c игровой вселенной        
-- | Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) = updateSpeedCow 
  (goCowUpDown (subtract cowSpeed))
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) = updateSpeedCow 
  (goCowUpDown (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) = updateSpeedCow 
  (goCowUpDown (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) = updateSpeedCow 
  (goCowUpDown (subtract cowSpeed))
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
goCowUpDown :: (Speed -> Speed) -> Cow -> Cow
goCowUpDown f cow = cow { cowSpeedUp = f $ cowSpeedUp cow }

-- | Передвижение коровы влево и вправо, если можно.
goCowLeftRight :: (Speed -> Speed) -> Cow -> Cow
goCowLeftRight f cow = cow { cowSpeedLeft = f $ cowSpeedLeft cow }

-- | Обновление игровой вселенной
-- | Обновить состояние игровой вселенной 
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | gameStopped == True = u
  | negativeLifeBalance u = toggleGame u
  | otherwise = updateLife dt (u
    { universeMap  = updateMap dt (universeMap u)
    , universeCow = updateCow dt (universeCow u)
    , universeScore  = updateScore dt (universeScore u)
    })
  where
    gameStopped = (universeStop u)


-- | Проверка на конец игры
negativeLifeBalance :: Universe -> Bool
negativeLifeBalance u = life <= 0
  where
    life = (universeLife u)

-- | Изменить активность игры
toggleGame :: Universe -> Universe
toggleGame u 
  | universeLife u > 0 = u { universeStop = not stopFlag }
  | otherwise = u
    { universeStop = not stopFlag
    , universeLife = 3
    , universeScore = 0
    }
  where
    stopFlag = (universeStop u)
  
-- | Обновление коровы
updateCow :: Float -> Cow -> Cow
updateCow dt c = c 
  { cowPosition = ((max screenLeft (min screenRight (coordX - dx))) 
  ,(max screenBottom (min screenTop (coordY - dy))))
  }
  where
    coordX = fst (cowPosition c)
    coordY = snd (cowPosition c)
    dx  = dt * (cowSpeedLeft c)
    dy = dt * (cowSpeedUp c)


-- | Обновить карту игровой вселенной 
updateMap :: Float -> Map -> Map
updateMap dt obstacleMap = obstacleMap
  { mapGoodBirds = updateObstacles dt (mapGoodBirds obstacleMap) 
    (obstacleSpeed obstacleMap)
  , mapBadBirds = updateObstacles dt (mapBadBirds obstacleMap) 
    (obstacleSpeed obstacleMap)
  , mapClovers = updateObstacles dt (mapClovers obstacleMap) 
    (obstacleSpeed obstacleMap)
  , obstacleSpeed = obstacleSpeed obstacleMap + speedIncrease
  }

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
  | collisionMulti cow (mapGoodBirds obstacleMap) = u 
    { universeLife = (life - 1) 
    , universeMap = collisionHandle obstacleMap cow
    }
  | collisionMulti cow (mapBadBirds obstacleMap) = u 
    { universeLife = (life - 2) 
    , universeMap = collisionHandle obstacleMap cow
    }
  | collisionMulti cow (mapClovers obstacleMap) = u 
    { universeLife = (life + 1) 
    , universeMap = collisionHandle obstacleMap cow
    }
  | otherwise = u
  where
    life = (universeLife u)
    cow = (universeCow u)
    obstacleMap = (universeMap u)

-- | Обновить скорость движения коровы
updateSpeedCow :: (Cow -> Cow) -> Universe -> Universe
updateSpeedCow f u = u { universeCow = f $ universeCow u }


-- | Оставить только те препятствия, которые входят в экран 
cropInsideScreen :: Obstacle o => [o] -> [o]
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

-- | Столкновения
-- | Сталкивается ли корова с любыми препятствиями  
collisionMulti :: Obstacle o => Cow -> [o] -> Bool
collisionMulti cow os = or (map (collides cow) (cropInsideScreen os)) 

-- | Сталкивается ли корова с препятствием?
collides :: Obstacle o => Cow -> o -> Bool
collides cow o 
  | crux >= oldx && cruy >= oldy && crdx >= oldx && crdy <= oldy && 
    clux <= oldx && cluy >= oldy = True
  | crdx >= olux && crdy <= oluy && crux >= olux && cruy >= oluy && 
    cldx <= olux && cldy <= oluy = True
  | crdx >= oldx && crdy >= oldy && crux >= olux && cruy <= oluy && 
    clux <= olux && cluy >= oluy = True
  | otherwise = False
  where
    (x1,y1) = cowPosition cow
    (x2,y2) = getPosition o
    s1 = cowSize cow
    s2 = getSize o
    (clux, cluy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 + 
      (cowPictureSizeHeight cow) / 2 * s1)
    (cldx, cldy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 - 
      (cowPictureSizeHeight cow) / 2 * s1)
    (crux, cruy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 + 
      (cowPictureSizeHeight cow) / 2 * s1)
    (crdx, crdy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 - 
      (cowPictureSizeHeight cow) / 2 * s1)
    (olux, oluy) = (x2 - (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    (oldx, oldy) = (x2 - (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)
    -- (orux, oruy) = (x2 + (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    -- (ordx, ordy) = (x2 + (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)

-- |  Удаления обьекта, с которым столкнулись
collisionHandle :: Map -> Cow -> Map 
collisionHandle m c = m 
  { mapClovers = filter (not . collides c) (cropInsideScreen (mapClovers m)) ++ 
   dropWhile isInsideScreen (mapClovers m) 
  , mapBadBirds = filter (not . collides c) (cropInsideScreen (mapBadBirds m)) ++ 
   dropWhile isInsideScreen (mapBadBirds m) 
  , mapGoodBirds = filter (not . collides c) (cropInsideScreen (mapGoodBirds m)) ++ 
   dropWhile isInsideScreen (mapGoodBirds m) 
  }

-- | Константы, параметры игры
-- | Экран
-- | Ширина экрана
screenWidth :: Int
screenWidth = 800

-- | Высота экрана
screenHeight :: Int
screenHeight = 450

-- | Положение правого края экрана
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

-- | Положение верхнего края экрана
screenTop :: Height
screenTop = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана
screenBottom :: Height
screenBottom = - fromIntegral screenHeight / 2

-- | Препятствия
-- | Размер клевера
defaultCloverSize :: Size
defaultCloverSize = 1.1

-- | Размер плохой птички
defaultBadBirdSize :: Size
defaultBadBirdSize = 1.1

-- | Размер хорошей птички
defaultGoodBirdSize :: Size
defaultGoodBirdSize = 1.1

-- | Диапазон высот препятствий.
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (screenBottom, screenTop)

-- | Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = screenRight * 1.5

-- | Диапазон, для вариации расстояния между препятсвтиями
obstacleOffsetRange :: (Offset, Offset)
obstacleOffsetRange = (-(defaultOffset / 2), defaultOffset / 2)

-- | Скорость игры
-- | Изначальная скорость движения игрока по вселенной - абсолютное изменение 
gameSpeed :: Speed
gameSpeed = 100

-- | Величина ускорения игры
speedIncrease :: Speed
speedIncrease = 0.1

-- | Корова
-- | Размер коровы
defaultCowSize :: Size
defaultCowSize = 1.1

-- | Мзменение высоты коровы при нажатии на клавиши (в пикселях)
cowSpeed :: Float
cowSpeed = 200

-- | Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + (fromIntegral screenWidth / 10)

-- | Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = 0

-- | Ширина картинки коровы
cowPictureSizeWidth :: Cow -> Size 
cowPictureSizeWidth _ = 133

-- | Высота картинки коровы
cowPictureSizeHeight :: Cow -> Size 
cowPictureSizeHeight _ = 68
        
