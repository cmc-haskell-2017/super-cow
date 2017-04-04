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
data Obstacle = Clover Position | -- Клевер - добавляет одну жизнь
    BadBird Position | -- Плохая птичка - снимает 2 жизни 
    GoodBird Position -- Хорошая птичка - снимает 1 жизни

-- Корова
data Cow = Cow Position

-- Игровая вселенная
data Universe = Universe
  { universeObstacles     :: [Obstacle]   -- Препятствия игровой вселенной
  , universeCow       :: Cow   -- Корова
  , universeScore     :: Score    --  Cчет
  , universeLife     :: Life    --  Жизни
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
  { universeObstacles  = initObstacles g 
  , universeCow = initCow
  , universeScore  = 0
  , universeLife  = 3
  }

-- Инициализировать корову (Дана)
initCow :: Cow
initCow = Cow cowInitHeight cowInitOffset

-- Инициализировать одно препятствие (Дана)
initGoodBird :: Position -> Obstacle -- хорошая птичка
initBadBird :: Position -> Obstacle -- птичка птичка
initClover :: Position -> Obstacle -- клевер

-- Инициализировать случайный бесконечный список препятствий (Дана)
initObstacles :: StdGen -> [Obstacle]




-- Обработка событий (-)
-- Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe


-- Обновление игровой вселенной
-- Обновить состояние игровой вселенной (Валера)
updateUniverse :: Float -> Universe -> Universe

-- Обновить состояние коровы (Валера)
updateCow :: Float -> Cow -> Cow

-- Изменить положение коровы, если можно (Дана)
moveCow :: Universe -> Universe

-- Обновить препятствия игровой вселенной (Валера)
updateObstacles :: Float -> [Obstacle] -> [Obstacle]

-- Обновить счет (Валера)
updateScore :: Float -> Score -> Score

-- Обновить жизни (Валера)
updateLife :: Float -> Life -> Life

-- Сталкивается ли корова с любыми препятствиями (Денис)
collisionObstacle :: Cow -> [Obstacle] -> Bool

-- Сталкивается ли корова с препятствием? (Денис)
collides :: Cow -> Obstacle -> Bool

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

-- Диапазон высот препятствий
ObstacleHeightRange :: (Height, Height)

-- Изначальная скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 100

-- Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + 100

-- Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = screenBottom + 200
