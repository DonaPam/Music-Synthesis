module Synth.Core.Signal where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Время в секундах
type Time = Double

-- | Амплитуда сигнала (нормализованная от -1.0 до 1.0)
type Amplitude = Double

-- | Сигнал - функция от времени
type Signal = Time -> Amplitude

-- | Частота в Герцах
type Frequency = Double

-- | Длительность в секундах
type Duration = Double

-- | Дискретизированный сигнал (семплы)
type Samples = Vector Amplitude

-- | Частота дискретизации
sampleRate :: Double
sampleRate = 44100.0

-- | Генерирует дискретные семплы из сигнала
renderSamples :: Duration -> Signal -> Samples
renderSamples dur signal = V.generate nSamples sampleAt
  where
    nSamples = floor (dur * sampleRate)
    sampleAt i = signal (fromIntegral i / sampleRate)

-- | Базовые волновые формы
sineWave :: Frequency -> Signal
sineWave freq t = sin (2 * pi * freq * t)

sawtoothWave :: Frequency -> Signal
sawtoothWave freq t = 2 * (t * freq - fromIntegral (floor (t * freq + 0.5))) - 1

squareWave :: Frequency -> Signal
squareWave freq t = if sin (2 * pi * freq * t) >= 0 then 1 else -1

triangleWave :: Frequency -> Signal
triangleWave freq t = 2 * abs (2 * (t * freq - fromIntegral (floor (t * freq + 0.5))) - 1) - 1

-- | Постоянный сигнал
constant :: Amplitude -> Signal
constant amp _ = amp

-- | Сложение сигналов
mix :: [Signal] -> Signal
mix signals t = sum [s t | s <- signals]

-- | Умножение сигналов (амплитудная модуляция)
multiply :: Signal -> Signal -> Signal
multiply s1 s2 t = s1 t * s2 t

-- | Масштабирование сигнала
scale :: Amplitude -> Signal -> Signal
scale factor signal t = factor * signal t