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

-- =========================================
-- Модель игровой вселенной
-- =========================================

-- | Высота объектов
type Height = Float

-- | Положение объектов по горизонтали
type Offset = Float

-- | Вид птицы
type Race = Int

-- | Параматры птицы
type Bird = (Offset, Height, Race)

-- | Счёт
type Score = Int

-- | Корова
data Cow = Cow
  { cowHeight :: Height  -- ^ Положение коровы по вертикали
  , cowOffset  :: Float   -- ^ Положение коровы по горизонтали
  }

-- | Модель игровой вселенной
data Universe = Universe
  { universeBirds   :: [Bird]   -- ^ Птички игровой вселенной
  , universeCow  :: Cow   -- ^ Корова
  , universeScore   :: Score    -- ^ Счёт (кол-во успешно пройденных птичек)
  }

-- | Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeBirds  = initBirds g
  , universeCow = initCow
  , universeScore  = 0
  }

-- | Начальное состояние коровы
initCow :: Cow
initCow = Cow
  { cowHeight = 0
  , cowOffset  = 0
  }

-- | Инициализировать одну птичку
initBird :: Height -> Bird

-- | Инициализировать случайный бесконечный
-- список птичек для игровой вселенной
initBirds :: StdGen -> [Bird]

-- | Рассчитать абсолютное положение птичек
absoluteBirds :: [Bird] -> [Bird]

-- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную
drawUniverse :: Universe -> Picture

-- | Отобразить всех птиц игровой вселенной, вмещающихся в экран
drawBirds :: [Birds] -> Picture

-- | Нарисовать одну птичку
drawBird :: Bird -> Picture

-- | Нарисовать корову
drawCow :: Cow -> Picture

-- | Нарисовать счёт в левом верхнем углу экрана
drawScore :: Score -> Picture

-- | Многоугольники, определяющие корову
cowPolygons :: Cow -> [Path]

-- | Многоугольники птичек
birdBoxes :: Bird -> [(Point, Point)]

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры
handleUniverse :: Event -> Universe -> 

-- | Подпрыгнуть, если можно
bumpCow :: Universe -> Universe

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной
updateUniverse :: Float -> Universe -> Universe

-- | Обновить состояние коровы
-- Корова не может прыгнуть выше потолка
updateCow :: Float -> Cow -> Cow
updateCow dt cow = cow
  { cowHeight = min h (cowHeight cow + dt * cowOffset cow)
  , cowOffset  = cowOffset cow + dt * gravity
  }
  where
    h = fromIntegral screenHeight / 2

-- | Обновить птичек игровой вселенной
updateBirds :: Float -> [Bird] -> [Bird]

-- | Сбросить игру (начать с начала со случайными птичками)
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { universeBirds  = tail (universeBirds u)
  , universeCow = initCow
  , universeScore  = 0
  }

-- | Конец игры?
isGameOver :: Universe -> Bool

-- | Сталкивается ли корова с любыми из
-- бесконечного списка птичек?
collision :: Cow -> [Bird] -> Bool

-- | Сталкивается ли корова с птичками?
collides :: Cow -> Bird -> Bool
collides cow bird = or
  [ polygonBoxCollides polygon box
  | polygon <- cowPolygons cow
  , box     <- birdBoxes bird ]

-- | Упрощённая проверка на пересечение многоугольников
polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs)) ]

-- =========================================
-- Константы, параметры игры
-- =========================================

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

-- | Ширина стенок ворот
birdWidth :: Float
birdWidth = 40

-- | Размер проёма ворот
birdSize :: Float
birdSize = 150

-- | Расстояние между птичками
defaultOffset :: Offset
defaultOffset = 300

-- | Диапазон высот птиц
birdHeightRange :: (Height, Height)

-- | Скорость движения игрока по вселенной (в пикселях в секунду).
offset :: Float
offset = 100

-- | Положение коровы по горизонтали
cowOffset :: Offset
cowOffset = screenLeft + 200