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


-- Модель игровой вселенной
-- Высота и Положение объектов
type Height = Float
type Offset = Float

-- Объекты
-- Птичка
data Bird = Grey Offset Height | Blue Offset Height
-- Клевер
data Clover = Clover Offset Height
-- Корова
data Cow = Cow
  { cowHeight :: Height  -- Положение коровы по вертикали
  , cowOffset :: Offset   -- Положение коровы по горизонтали
  }

-- Счёт
type Score = Int

-- Модель игровой вселенной
data Universe = Universe
  { universeBirds     :: [Bird]   -- Птички игровой вселенной
  , universeСlovers   :: [Clover]   -- Клеверы игровой вселенной
  , universeCow       :: Cow   -- Корова
  , universeScore     :: Score    --  Cчет
  }

-- Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeBirds  = initBirds g
  , universeClovers  = initClovers g
  , universeCow = initCow
  , universeScore  = 0
  }

-- | Начальное состояние коровы
initCow :: Cow
initCow = Cow
  { cowHeight = 0
  , cowOffset  = cowInitOffset
  }

-- Инициализировать одну птичку
initBird :: Height -> Bird

-- Инициализировать случайный бесконечный
-- список птичек для игровой вселенной
initBirds :: StdGen -> [Bird]

-- Рассчитать абсолютное положение клевера
absoluteBirds :: [Bird] -> [Bird]

-- Инициализировать одну птичку
initClover :: Height -> Clover

-- Инициализировать случайный бесконечный
-- список клеверов для игровой вселенной
initClovers :: StdGen -> [Clover]

-- Рассчитать абсолютное положение клевера
absoluteClovers :: [Clover] -> [Clover]


-- Отрисовка игровой вселенной
-- Отобразить игровую вселенную
drawUniverse :: Universe -> Picture

-- Отобразить всех птиц игровой вселенной, вмещающихся в экран
drawBirds :: [Bird] -> Picture

-- Нарисовать одну птичку
drawBird :: Bird -> Picture

-- Отобразить всех клеверов игровой вселенной, вмещающихся в экран
drawClovers :: [Clover] -> Picture

-- Нарисовать одну птичку
drawClover :: Clover -> Picture

-- Нарисовать корову
drawCow :: Cow -> Picture

-- Нарисовать счёт в левом верхнем углу экрана
drawScore :: Score -> Picture

-- не нужно наерное, если картинку грузить
-- Многоугольники, определяющие корову
cowPolygons :: Cow -> [Path]

-- Многоугольники птичек
birdBoxes :: Bird -> [(Point, Point)]


-- Обработка событий
-- Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe

-- Изменить положение коровы, если можно
moveCow :: Universe -> Universe


-- Обновление игровой вселенной
-- Обновить состояние игровой вселенной
updateUniverse :: Float -> Universe -> Universe

-- Обновить состояние коровы
-- Корова не может двигаться дальше, чем края вселенной
updateCow :: Float -> Cow -> Cow

-- Обновить птичек игровой вселенной
updateBirds :: Float -> [Bird] -> [Bird]

-- Обновить клеверы игровой вселенной
updateClovers :: Float -> [Bird] -> [Bird]


-- Сталкивается ли корова с любыми птичками
collisionBird :: Cow -> [Bird] -> Bool
-- Сталкивается ли корова с любыми птичками
collisionClover :: Cow -> [Clover] -> Bool

-- Сталкивается ли корова с птичками?
collides :: Cow -> Bird -> Bool

-- Сталкивается ли корова с клеверами?
collides :: Cow -> Bird -> Bool

-- Упрощённая проверка на пересечение многоугольников
polygonBoxCollides :: Path -> (Point, Point) -> Bool

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

-- Расстояние между птичками и клеверами
defaultOffset :: Offset
defaultOffset = 300

-- Диапазон высот птиц
birdHeightRange :: (Height, Height)

-- Скорость движения игрока по вселенной (в пикселях в секунду).
offset :: Float
offset = 100

-- Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + 100

-- Положение коровы по вертикали
cowInitTop :: Height
cowInitTop = screenBottom + 200
