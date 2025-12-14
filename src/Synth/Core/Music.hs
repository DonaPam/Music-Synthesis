module Synth.Core.Music where

import Synth.Core.Signal
import Synth.Core.Synthesizer

-- | Музыкальная структура
data Music =
    Note Int Duration Envelope (Frequency -> Signal)  -- Полутон, длительность, огибающая, форма волны
  | Rest Duration                                     -- Пауза
  | Sequential [Music]                                -- Последовательность (мелодия)
  | Parallel [Music]                                  -- Параллельное исполнение (аккорд)
  | Transpose Int Music                               -- Транспозиция
  | Tempo Double Music                                -- Изменение темпа
  | Volume Double Music                               -- Изменение громкости
  deriving (Show)

-- | "Умные" конструкторы для удобства

-- | Нота по имени
namedNote :: NoteName -> Octave -> Duration -> Music
namedNote name oct dur = 
  Note (pitch name oct) dur defaultEnvelope sineWave

-- | Синусоидальная нота
sinNote :: Int -> Duration -> Music
sinNote p dur = Note p dur defaultEnvelope sineWave

-- | Квадратная волна
sqrNote :: Int -> Duration -> Music
sqrNote p dur = Note p dur defaultEnvelope squareWave

-- | Пауза
pause :: Duration -> Music
pause = Rest

-- | Операторы для DSL
infixr 5 |:
infixr 5 =:

-- | Последовательность
(|:) :: Music -> Music -> Music
a |: b = Sequential [a, b]

-- | Параллельное исполнение (аккорд)
(=:) :: Music -> Music -> Music
a =: b = Parallel [a, b]

-- | Транспозиция
trans :: Int -> Music -> Music
trans = Transpose

-- | Изменение темпа
tempo :: Double -> Music -> Music
tempo = Tempo

-- | Изменение громкости
volume :: Double -> Music -> Music
volume = Volume

-- | Интерпретация музыки в сигнал
interpret :: Music -> Signal
interpret (Note p dur env wave) = 
  note p dur env wave
interpret (Rest dur) = 
  \t -> if t < dur then 0 else 0
interpret (Sequential ms) = 
  let signals = map interpret ms
      durations = map musicDuration ms
      offsets = scanl (+) 0 durations
  in \t -> case findIndex (\off -> t >= off && t < off + durations !! offIndex) offsets of
             Just idx -> signals !! idx (t - offsets !! idx)
             Nothing -> 0
  where
    findIndex :: (Double -> Bool) -> [Double] -> Maybe Int
    findIndex p xs = go 0 xs
      where
        go _ [] = Nothing
        go n (x:xs) | p x = Just n
                    | otherwise = go (n+1) xs

interpret (Parallel ms) = 
  mix (map interpret ms)
interpret (Transpose n m) = 
  case m of
    Note p dur env wave -> Note (p + n) dur env wave
    _ -> Transpose n m  -- Для композитных структур нужна рекурсия
interpret (Tempo factor m) = 
  case m of
    Note p dur env wave -> Note p (dur / factor) env wave
    Rest dur -> Rest (dur / factor)
    _ -> Tempo factor m
interpret (Volume factor m) = 
  scale factor (interpret m)

-- | Вычисление длительности музыкальной структуры
musicDuration :: Music -> Duration
musicDuration (Note _ dur _ _) = dur
musicDuration (Rest dur) = dur
musicDuration (Sequential ms) = sum (map musicDuration ms)
musicDuration (Parallel ms) = maximum (map musicDuration ms)
musicDuration (Transpose _ m) = musicDuration m
musicDuration (Tempo factor m) = musicDuration m / factor
musicDuration (Volume _ m) = musicDuration m

-- | Примеры композиций

-- | Простая гамма C мажор
cMajorScale :: Music
cMajorScale =
  namedNote C 4 0.5 |:
  namedNote D 4 0.5 |:
  namedNote E 4 0.5 |:
  namedNote F 4 0.5 |:
  namedNote G 4 0.5 |:
  namedNote A 4 0.5 |:
  namedNote B 4 0.5 |:
  namedNote C 5 1.0

-- | Аккорд C мажор
cMajorChord :: Music
cMajorChord =
  namedNote C 4 1.0 =:
  namedNote E 4 1.0 =:
  namedNote G 4 1.0

-- | Простая мелодия (Twinkle Twinkle Little Star)
twinkle :: Music
twinkle =
  namedNote C 4 0.5 |:
  namedNote C 4 0.5 |:
  namedNote G 4 0.5 |:
  namedNote G 4 0.5 |:
  namedNote A 4 0.5 |:
  namedNote A 4 0.5 |:
  namedNote G 4 1.0 |:
  pause 0.5 |:
  namedNote F 4 0.5 |:
  namedNote F 4 0.5 |:
  namedNote E 4 0.5 |:
  namedNote E 4 0.5 |:
  namedNote D 4 0.5 |:
  namedNote D 4 0.5 |:
  namedNote C 4 1.0