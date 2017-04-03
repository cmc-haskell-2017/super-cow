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
data Clover = Clover  -- Клевер - добавляет одну жизнь
  { position :: Position
  , size :: Float
  }
     
data BadBird = BadBird -- Плохая птичка - снимает 2 жизни 
  { position :: Position
  , size     :: Float
  }
  
data GoodBird = GoodBird -- Хорошая птичка - снимает 1 жизни
  { position :: Position
  , size     :: Float
  }
  
data Map = Map 
  { mapGoodBirds :: [GoodBird]
  , mapBadBirds  :: [BadBird]
  , mapClovers   :: [Clover]
  }

-- Корова
data Cow = Cow Position

-- Игровая вселенная
data Universe = Universe
  { universeMap       :: Map   -- Препятствия игровой вселенной
  , universeCow       :: Cow   -- Корова
  , universeScore     :: Score    --  Cчет
  , universeLife      :: Life    --  Жизни
  }


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

    
instance Obstacle Clover where    
    draw image clover = translate x y (scale r r image)
        where
            (x, y) = position clover
            r = size clover
            
    collides (Cow (x1,y1)) clover 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = position clover
    
instance Obstacle BadBird where
    draw image badbird = translate x y (scale r r image)
        where
            (x, y) = position badbird
            r = size badbird
    
    collides (Cow (x1,y1)) badbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = position badbird
        
instance Obstacle GoodBird where 
    draw image goodbird = translate x y (scale r r image)
        where
            (x, y) = position goodbird
            r = size goodbird
    
    collides (Cow (x1,y1)) goodbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = position goodbird

-- Инициализировать клевер
initClover p = Clover 
    { position = p
    , size = defaultCloverSize
    }

-- Инициализировать плохую птичку 
initBadBird p = BadBird
    { position = p 
    , size = defaultBadBirdSize
    }
    
-- Инициализировать хорошую птичку
initGoodBird p = GoodBird
    { position = p 
    , size = defaultGoodBirdSize
    }

-- Инициализировать карту препятствий (Дана)
initMap :: StdGen -> Map

-- Инициализировать случайный бесконечный список препятствий (Дана)
initObstacles :: Obstacle o => StdGen -> [o]

-- Инициализировать корову (Дана)
initCow :: Cow
initCow = Cow cowInitHeight cowInitOffset

-- Отрисовка игровой вселенной
-- Отобразить игровую вселенную (Ралина)
drawUniverse :: Universe -> Picture

-- Нарисовать корову (Ралина)
drawCow :: Cow -> Picture

-- Нарисовать счёт в левом верхнем углу экрана (Ралина)
drawScore :: Score -> Picture

-- Нарисовать счёт в левом верхнем углу экрана (Ралина)
drawLife :: Life -> Picture

-- Отобразить все препятствия игровой вселенной, вмещающихся в экран (Ралина)
drawObstacles :: Obstacle o => [o] -> Picture

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

-- Диапазон высот препятствий
obstacleHeightRange :: (Height, Height)

-- Изначальная скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 100

-- Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + 100

-- Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = screenBottom + 200
