module SuperCow where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру «Super Cow»
runSuperCow :: Images -> IO ()
runSuperCow images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- Загрузка изображений
loadImages :: IO Images
loadImages = do
  Just cow   <- loadJuicyPNG "images/cow.png"
  Just clover  <- loadJuicyPNG "images/clover.png"
  Just good_bird_up  <- loadJuicyPNG "images/GrayBirdUp.png"
  Just good_bird_down  <- loadJuicyPNG "images/GrayBirdDown.png"
  Just bad_bird_up  <- loadJuicyPNG "images/BlueBirdUp.png"
  Just bad_bird_down  <- loadJuicyPNG "images/BlueBirdDown.png"
  Just sky_with_grass  <- loadJuicyPNG "images/SkyWithGrass.png"

  return Images
    { imageCow   = scale 0.1 0.1 cow
    , imageClover  = scale 0.1 0.1 clover
    , imageGoodBirdUp = scale 0.1 0.1 good_bird_up
    , imageGoodBirdDown = scale 0.1 0.1 good_bird_down
    , imageBadBirdUp = scale 0.1 0.1 bad_bird_up
    , imageBadBirdDown = scale 0.1 0.1 bad_bird_down
    , imageSkyWithGrass = scale 1.0 1.0 sky_with_grass
    }


-- Структуры данных
-- Высота и Положение объектов
type Height = Float -- Высота обьекта
type Offset = Float -- Сдвиг обьекта
type Position = (Offset,Height)  -- Координаты обьекта
type Life = Int -- Жизни (изначально 3)
type Score = Int -- Счет (изменяется постоянно)

-- Объекты игровой вселенной
data Clover = Clover  -- Клевер - добавляет одну жизнь
  { cloverPosition :: Position
  , cloverSize :: Float
  }
     
data BadBird = BadBird -- Плохая птичка - снимает 2 жизни 
  { badBirdPosition :: Position
  , badBirdSize     :: Float
  }
  
data GoodBird = GoodBird -- Хорошая птичка - снимает 1 жизни
  { goodBirdPosition :: Position
  , goodBirdSize     :: Float
  }
  
data Map = Map 
  { mapGoodBirds :: [GoodBird]
  , mapBadBirds  :: [BadBird]
  , mapClovers   :: [Clover]
  }

-- Корова
data Cow = Cow
  { cowPosition :: Position
  , cowSize     :: Float
  }

-- Игровая вселенная
data Universe = Universe
  { universeMap       :: Map   -- Препятствия игровой вселенной
  , universeCow       :: Cow   -- Корова
  , universeScore     :: Score    --  Cчет
  , universeLife      :: Life    --  Жизни
  }
-- Изображения объектов
data Images = Images
  { imageCow  :: Picture   -- ^ Изображение коровы.
  , imageClover :: Picture   -- ^ Изображение клевера
  , imageGoodBirdUp       :: Picture   -- ^ Изображение GrayBirdUp.
  , imageGoodBirdDown       :: Picture   -- ^ Изображение GrayBirdDown.
  , imageBadBirdUp       :: Picture   -- ^ Изображение BlueBirdUp.
  , imageBadBirdDown       :: Picture   -- ^ Изображение BlueBirdDown.
  , imageSkyWithGrass       :: Picture   -- ^ Изображение Неба.
  }


-- Отрисовка игровой вселенной
-- Отобразить игровую вселенную (Ралина)
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawObstacles images (universeMap u)
  , drawCow (imageCow images) (universeCow u)
  , drawScore (universeScore u)
  , drawLife (universeLife u)
  ]

-- Отобразить все препятствия игровой вселенной, вмещающихся в экран (Ралина)
drawObstacles :: Images -> Map -> Picture
drawObstacles images obstacles = pictures
  [ pictures (map (draw  (imageGoodBirdUp  images)) (mapGoodBirds  obstacles))
  , pictures (map (draw (imageBadBirdUp images)) (mapBadBirds obstacles))
  , pictures (map (draw (imageClover  images)) (mapClovers obstacles))
  ]

-- Оставить только те препятствия, которые входят в экран (-)
cropObstaclesInsideScreen :: [Obstacle] -> [Obstacle]


-- Нарисовать корову (Ралина)
drawCow :: Picture -> Cow -> Picture
drawCow image cow = translate x y (scale r r image)
  where
    (x, y) = position cow
    r = size cow

-- Нарисовать счёт в левом верхнем углу экрана (Ралина)
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ 
  -- color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  -- , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  -- , 
  translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show score))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- Нарисовать жизни в правом верхнем углу экрана (Ралина)
drawLife :: Life -> Picture
drawLife life = translate w h (scale 30 30 (pictures
  [ --color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  -- , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  -- , 
  translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show life))))  -- красная жизнь
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2



-- Инициализация вселенной (Дана)
-- Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeMap  = initMap g 
  , universeCow = initCow
  , universeScore  = 0
  , universeLife  = 3
  }

-- Реализация препятствий
class Obstacle o where 
    -- Нарисовать одно препятствие (Ралина)
    draw :: Picture -> o -> Picture
    -- Сталкивается ли корова с препятствием? (Денис)
    collides :: Cow -> o -> Bool
    -- Получение позиции
    getPosition :: o -> Position
    -- Получние размера
    getSize :: o -> Float
    
    
instance Obstacle Clover where    
    draw image clover = translate x y (scale r r image)
        where
            (x, y) = cloverPosition clover
            r = cloverSize clover
            
    collides (Cow (x1,y1)) clover 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = cloverPosition clover
    
    getPosition clover = cloverPosition clover
    
    getSize clover = cloverSize clover
    
instance Obstacle BadBird where
    draw image badbird = translate x y (scale r r image)
        where
            (x, y) = badBirdPosition badbird
            r = badBirdSize badbird
    
    collides (Cow (x1,y1)) badbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = badBirdPosition badbird
        
    getPosition badbird = badBirdPosition badbird
    
    getSize badbird = badBirdSize badbird
        
instance Obstacle GoodBird where 
    draw image goodbird = translate x y (scale r r image)
        where
            (x, y) = goodBirdPosition goodbird
            r = goodBirdSize goodbird
    
    collides (Cow (x1,y1)) goodbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = goodBirdPosition goodbird
            
    getPosition goodbird = goodBirdPosition goodbird
    
    getSize goodbird = goodBirdSize goodbird

-- Инициализировать клевер
initClover :: Position -> Clover
initClover p = Clover 
    { cloverPosition = p
    , cloverSize = defaultCloverSize
    }

-- Инициализировать плохую птичку 
initBadBird :: Position -> BadBird
initBadBird p = BadBird
    { badBirdPosition = p 
    , badBirdSize = defaultBadBirdSize
    }
    
-- Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird p = GoodBird
    { goodBirdPosition = p 
    , goodBirdSize = defaultGoodBirdSize
    }

-- Инициализировать карту препятствий (Дана)
initMap :: StdGen -> Map

-- Инициализировать случайный бесконечный список препятствий (Дана)
initObstacles :: Obstacle o => StdGen -> [o]

-- Инициализировать корову (Дана)
initCow :: Cow
initCow = Cow 
    { cowPosition = (cowInitHeight, cowInitOffset)
    , cowSize = defaultCowSize
    }

-- Оставить только те препятствия, которые входят в экран 
cropInsideScreen :: Obstacle o => [o] -> [o]

-- Обработка событий (-)
-- Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe

-- Сталкивается ли корова с любыми препятствиями (Денис 
collisionMulti :: Obstacle o => Cow -> [o] -> Bool
collisionMulti cow os = foldr1 (&&) (map (collides cow) (cropInsideScreen os)) 
    
-- Обновление игровой вселенной
-- Обновить состояние игровой вселенной (Валера)
updateUniverse :: Float -> Universe -> Universe

-- Обновить состояние коровы (Валера)
updateCow :: Float -> Cow -> Cow

-- Изменить положение коровы, если можно (Дана)
moveCow :: Universe -> Universe

-- Обновить карту игровой вселенной (Валера)
updateMap :: Float -> Map -> Map

-- Обновить препятствия игровой вселенной (Валера)
updateObstacles :: Obstacle o => Float -> [o] -> [o]

-- Обновить счет (Валера)
updateScore :: Float -> Score -> Score

-- Обновить жизни (Валера)
updateLife :: Float -> Life -> Life

-- Текущая скорость движения игрока по вселенной (троится по времени и изначальной скорости)
сurrentSpeed :: Float -> Float -> Float


-- Константы, параметры игры
-- Ширина экрана
screenWidth :: Int
screenWidth = 800

-- Высота экрана
screenHeight :: Int
screenHeight = 450

-- Положение правого края экрана
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- Положение левого края экрана
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

-- Положение верхнего края экрана
screenTop :: Height
screenTop = fromIntegral screenHeight / 2

-- Положение нижнего края экрана
screenBottom :: Height
screenBottom = - fromIntegral screenHeight / 2

-- Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = 300

defaultCloverSize :: Float
defaultCloverSize = 1

defaultBadBirdSize :: Float
defaultBadBirdSize = 1

defaultGoodBirdSize :: Float
defaultGoodBirdSize = 1

defaultCowSize :: Float
defaultCowSize = 1

-- Диапазон высот препятствий
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (0,0)
-- Изначальная скорость движения игрока по вселенной (в пикселях в секунду)
gameSpeed :: Float
gameSpeed = 100

-- Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + 100

-- Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = screenBottom + 200
