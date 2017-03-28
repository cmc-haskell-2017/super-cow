module SuperCow where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру «Flappy Lambda»
runSuperCow :: IO ()
runSuperCow = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду


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


-- Инициализация вселенной
-- Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeObstacles  = initObstacles g 
  , universeCow = initCow
  , universeScore  = 0
  , universeLife  = 3
  }

-- Инициализировать корову
initCow :: Cow
initCow = Cow cowInitHeight cowInitOffset

-- Инициализировать одно препятствие
initGoodBird :: Position -> Obstacle -- хорошая птичка
initBadBird :: Position -> Obstacle -- птичка птичка
initClover :: Position -> Obstacle -- клевер

-- Инициализировать случайный бесконечный список препятствий
initObstacles :: StdGen -> [Obstacle]


-- Отрисовка игровой вселенной
-- Отобразить игровую вселенную
drawUniverse :: Universe -> Picture

-- Отобразить все препятствия игровой вселенной, вмещающихся в экран
drawObstacles :: [Obstacle] -> Picture

-- Оставить только те препятствия, которые входят в экран
cropObstaclesInsideScreen :: [Obstacle] -> [Obstacle]

-- Нарисовать одно препятствие
drawObstacle :: Obstacle -> Picture

-- Нарисовать корову
drawCow :: Cow -> Picture

-- Нарисовать счёт в левом верхнем углу экрана
drawScore :: Score -> Picture

-- Нарисовать счёт в левом верхнем углу экрана
drawLife :: Life -> Picture


-- Обработка событий
-- Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe


-- Обновление игровой вселенной
-- Обновить состояние игровой вселенной
updateUniverse :: Float -> Universe -> Universe

-- Обновить состояние коровы
updateCow :: Float -> Cow -> Cow

-- Изменить положение коровы, если можно
moveCow :: Universe -> Universe

-- Обновить препятствия игровой вселенной
updateObstacles :: Float -> [Bird] -> [Bird]

-- Обновить счет 
updateScore :: Float -> Score -> Score

-- Обновить жизни
updateLife :: Float -> Life -> Life

-- Сталкивается ли корова с любыми препятствиями
collisionObstacle :: Cow -> [Obstacle] -> Bool

-- Сталкивается ли корова с препятствием?
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
