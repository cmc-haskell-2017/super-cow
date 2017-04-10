module SuperCow where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- | Запустить игру «Super Cow»
runSuperCow :: Images -> IO ()
runSuperCow images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60     -- кол-во кадров в секунду

-- | Загрузка изображений
loadImages :: IO Images
loadImages = do
  Just cow                <- loadJuicyPNG "images/cow.png"
  Just clover             <- loadJuicyPNG "images/clover.png"
  Just good_bird_up       <- loadJuicyPNG "images/GrayBirdUp.png"
  Just good_bird_down     <- loadJuicyPNG "images/GrayBirdDown.png"
  Just bad_bird_up        <- loadJuicyPNG "images/BlueBirdUp.png"
  Just bad_bird_down      <- loadJuicyPNG "images/BlueBirdDown.png"
  Just sky_with_grass     <- loadJuicyJPG "images/SkyWithGrass.jpg"

  return Images
    { imageCow          = scale 1.0 1.0 cow
    , imageClover       = scale 1.0 1.0 clover
    , imageGoodBirdUp   = scale 1.0 1.0 good_bird_up
    , imageGoodBirdDown = scale 1.0 1.0 good_bird_down
    , imageBadBirdUp    = scale 1.0 1.0 bad_bird_up
    , imageBadBirdDown  = scale 1.0 1.0 bad_bird_down
    , imageSkyWithGrass = scale 1.0 1.0 sky_with_grass
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

-- |  Карта препятствий
data Map = Map
  { mapGoodBirds :: [GoodBird]
  , mapBadBirds  :: [BadBird]
  , mapClovers   :: [Clover]
  , obstacleSpeed :: Speed
  }

-- | Корова
data Cow = Cow
  { cowPosition :: Position
  , cowSize     :: Size
  , cowSpeedUp :: Speed  -- скорость по вертикали 
  , cowSpeedLeft :: Speed  -- скорость по горизонтали
  }

-- | Игровая вселенная
data Universe = Universe
  { universeMap       :: Map   -- ^ Препятствия игровой вселенной
  , universeCow       :: Cow   -- ^ Корова
  , universeScore     :: Score  -- ^ Cчет
  , universeLife      :: Life  -- ^ Жизни
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


-- | Отрисовка игровой вселенной
-- | Отобразить игровую вселенную 
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageSkyWithGrass images)
  , drawObstacles images (universeMap u)
  , drawCow (imageCow images) (universeCow u)
  , drawScore (universeScore u)
  , drawLife (universeLife u)
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
  [ pictures (map (draw (imageGoodBirdUp  images)) (cropInsideScreen (mapGoodBirds obstacles)))
  , pictures (map (draw (imageBadBirdUp images)) (cropInsideScreen (mapBadBirds obstacles)))
  , pictures (map (draw (imageClover  images)) (cropInsideScreen (mapClovers obstacles)))
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
  [ 
  -- color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- ^ белая рамка
  -- , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])  -- ^ чёрные внутренности
  -- , 
  translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show score))))     -- ^ красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать жизни в правом верхнем углу экрана 
drawLife :: Life -> Picture
drawLife life = translate w h (scale 30 30 (pictures
  [ --color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- ^ белая рамка
  -- , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])   -- ^ чёрные внутренности
  -- , 
  translate (-2) (-1.5) (scale 0.01 0.01 (color red (text (show life))))       -- ^ красная жизнь
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2


-- | Инициализация вселенной 
-- | Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
    { universeMap  = initMap g 
    , universeCow = initCow
    , universeScore  = 0
    , universeLife  = 3
    }

-- | Реализация препятствий
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
    

    
instance Obstacle Clover where    
    getPosition = cloverPosition
    
    getSize = cloverSize
    
    setPosition clover p = clover { cloverPosition = p }
    
    setSize clover s = clover { cloverSize = s }
    
    getHeight clover = 50
    
    getWidth clover = 50
    
instance Obstacle BadBird where
    getPosition = badBirdPosition 
    
    getSize = badBirdSize 
    
    setPosition badBird p = badBird { badBirdPosition = p }
    
    setSize badBird s = badBird { badBirdSize = s }
    
    getWidth badBird = 81
    
    getHeight badBird = 42
        
instance Obstacle GoodBird where 
    getPosition = goodBirdPosition 
    
    getSize = goodBirdSize 

    setPosition goodBird p = goodBird { goodBirdPosition = p }
    
    setSize goodBird s = goodBird { goodBirdSize = s }
    
    getWidth goodBird = 67
    
    getHeight goodBird = 36
            
-- | Нарисовать препятствие    
draw :: Obstacle o => Picture -> o -> Picture
draw image o = translate x y (scale r r image)
    where
        (x, y) = getPosition o
        r = getSize o
    
-- | Сталкивается ли корова с препятствием?
collides :: Obstacle o => Cow -> o -> Bool
collides cow o 
    | crux >= oldx && cruy >= oldy && crdx >= oldx && crdy <= oldy = True
    | crdx >= olux && crdy <= oluy && crux >= olux && cruy >= oluy = True
    | crdx >= oldx && crdy >= oldy && crux >= olux && cruy <= oluy = True
    | otherwise = False
        where
            (x1,y1) = cowPosition cow
            (x2,y2) = getPosition o
            s1 = cowSize cow
            s2 = getSize o
            -- (clux, cluy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 + (cowPictureSizeHeight cow) / 2 * s1)
            -- (cldx, cldy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 - (cowPictureSizeHeight cow) / 2 * s1)
            (crux, cruy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 + (cowPictureSizeHeight cow) / 2 * s1)
            (crdx, crdy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 - (cowPictureSizeHeight cow) / 2 * s1)
            (olux, oluy) = (x2 - (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
            (oldx, oldy) = (x2 - (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)
            -- (orux, oruy) = (x2 + (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
            -- (ordx, ordy) = (x2 + (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)
    
-- | Инициализировать клевер
initClover :: Position -> Clover
initClover p = Clover 
    { cloverPosition = p
    , cloverSize = defaultCloverSize
    }

-- | Инициализировать плохую птичку 
initBadBird :: Position -> BadBird
initBadBird p = BadBird
    { badBirdPosition = p 
    , badBirdSize = defaultBadBirdSize
    }
    
-- | Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird p = GoodBird
    { goodBirdPosition = p 
    , goodBirdSize = defaultGoodBirdSize
    }

-- | Инициализировать карту препятствий 
--TODO: можно изменить defaultOffset для препятствий разного типа
initMap :: StdGen -> Map
initMap g = Map 
  { mapGoodBirds = map initGoodBird positions_1
  , mapClovers = map initClover positions_2
  , mapBadBirds = map initBadBird positions_3
  , obstacleSpeed = gameSpeed
  }
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (g9, g10) = split g4
    -- ВОТ ТУТ НАДО РАЗНИЦУ ПО ГОРИЗОНТАЛИ ТОЖЕ ДЕЛАТЬ РАНДОМНОЙ
    positions_1 = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleOffsetRange g8)) (randomRs obstacleHeightRange g5)
    positions_2 = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleOffsetRange g9)) (randomRs obstacleHeightRange g6)
    positions_3 = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleOffsetRange g10)) (randomRs obstacleHeightRange g7)
    -- positions_1 = zip [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleHeightRange g2)
    -- positions_2 = zip [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleHeightRange g3)
    -- positions_3 = zip [screenLeft, screenLeft + defaultOffset..] (randomRs obstacleHeightRange g4)

-- | Инициализировать корову 
initCow :: Cow
initCow = Cow 
    { cowPosition = (cowInitOffset, cowInitHeight)
    , cowSize = defaultCowSize
    , cowSpeedUp = 0
    , cowSpeedLeft = 0
    }

-- | Оставить только те препятствия, которые входят в экран 
cropInsideScreen :: Obstacle o => [o] -> [o]
cropInsideScreen obs = dropWhile (\o -> pos o < screenLeft) $ takeWhile (\o -> pos o < screenRight) obs
    where 
        pos = fst . getPosition

-- | Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) = updateSpeedCow goUp
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) = updateSpeedCow goDown
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) = updateSpeedCow stopCow
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) = updateSpeedCow stopCow
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = updateSpeedCow goLeft
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = updateSpeedCow goRight
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) = updateSpeedCow stopCow
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) = updateSpeedCow stopCow
-- handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = goLeft
-- handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = goRight
handleUniverse _ = id


updateSpeedCow :: (Cow -> Cow) -> Universe -> Universe
updateSpeedCow f u = u { universeCow = f $ universeCow u }

-- | Передвижение коровы вверх, если можно.
goUp :: Cow -> Cow
goUp c = c { cowSpeedUp = -cowSpeed }

-- | Передвижение коровы вниз, если можно.
goDown :: Cow -> Cow
goDown c = c { cowSpeedUp = cowSpeed }

goLeft :: Cow -> Cow
goLeft c = c { cowSpeedLeft = cowSpeed }

goRight :: Cow -> Cow
goRight c = c { cowSpeedLeft = -cowSpeed }

stopCow :: Cow -> Cow
stopCow c = c { cowSpeedUp = 0, cowSpeedLeft = 0}


-- | Сталкивается ли корова с любыми препятствиями  
collisionMulti :: Obstacle o => Cow -> [o] -> Bool
collisionMulti cow os = or (map (collides cow) (cropInsideScreen os)) 
    
-- | Обновление игровой вселенной
-- | Обновить состояние игровой вселенной 
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = u
  { universeMap  = updateMap  dt (universeMap  u)
  , universeCow = updateCow dt (universeCow u)
  , universeScore  = updateScore dt (universeScore u)
  , universeLife = updateLife dt u
  }

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
updateMap dt map = map
  { mapGoodBirds = updateObstacles dt (mapGoodBirds map) (obstacleSpeed map)
  , mapBadBirds = updateObstacles dt (mapBadBirds map) (obstacleSpeed map)
  , mapClovers = updateObstacles dt (mapClovers map) (obstacleSpeed map)
  , obstacleSpeed = obstacleSpeed map + speedIncrease
  }

-- | Обновить препятствия игровой вселенной 
updateObstacles :: Obstacle o => Float -> [o] -> Speed -> [o]
updateObstacles _ [] _ = []
updateObstacles dt obstacles speed = 
    dropWhile (\o -> fst (getPosition o) < screenLeft) (map (\o -> setPosition o (coordX o - dx, coordY o)) obstacles)
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
    -- pos = coordX - screenLeft + fromIntegral size
    dx  = dt * speed
    -- dt' = dt - coordX / gameSpeed

-- | Обновить счет 
updateScore :: Float -> Score -> Score
updateScore _ score = score + 1


-- | Обновить жизни 
updateLife :: Float -> Universe -> Life
updateLife _ u
  | collisionMulti cow (mapGoodBirds map) = life - 1
  | collisionMulti cow (mapBadBirds map) = life - 2
  | collisionMulti cow (mapClovers map) = life + 1
  | otherwise = life
  where
    life = (universeLife u)
    cow = (universeCow u)
    map = (universeMap u)

-- | Константы, параметры игры
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

defaultCloverSize :: Size
defaultCloverSize = 1.1

defaultBadBirdSize :: Size
defaultBadBirdSize = 1.1

defaultGoodBirdSize :: Size
defaultGoodBirdSize = 1.1

defaultCowSize :: Size
defaultCowSize = 1.1

-- | Диапазон высот препятствий.
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (screenBottom, screenTop)

-- | Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = screenRight * 1.5

-- | Расстояние между клеверами
defaultCloverOffset :: Offset
defaultCloverOffset = 2000

obstacleOffsetRange :: (Offset, Offset)
obstacleOffsetRange = (-(defaultOffset / 2), defaultOffset / 2)

-- | Изначальная скорость движения игрока по вселенной - абсолютное изменение 
gameSpeed :: Speed
gameSpeed = 100

speedIncrease :: Speed
speedIncrease = 0.1

-- | изменение высоты коровы при нажатии на клавиши (в пикселях)
cowSpeed :: Float
cowSpeed = 200

-- | Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + (fromIntegral screenWidth / 10)

-- | Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = 0

cowPictureSizeWidth :: Cow -> Size 
cowPictureSizeWidth _ = 133

cowPictureSizeHeight :: Cow -> Size 
cowPictureSizeHeight _ = 68
        
