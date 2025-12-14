module Synth.Core.Synthesizer where

import Synth.Core.Signal

-- | Огибающая ADSR
data Envelope = Envelope
  { attackTime :: Duration    -- Время атаки
  , decayTime :: Duration     -- Время спада
  , sustainLevel :: Amplitude -- Уровень поддержки
  , releaseTime :: Duration   -- Время затухания
  }

-- | Огибающая по умолчанию (фортепиано)
defaultEnvelope :: Envelope
defaultEnvelope = Envelope
  { attackTime = 0.01
  , decayTime = 0.05
  , sustainLevel = 0.7
  , releaseTime = 0.1
  }

-- | Применяет ADSR-огибающую к сигналу
applyEnvelope :: Envelope -> Duration -> Signal -> Signal
applyEnvelope env dur signal t
  | t < 0 = 0
  | t >= dur = 0  -- После окончания ноты
  | t <= at = t / at  -- Атака
  | t <= at + dt = 1 - (1 - sl) * ((t - at) / dt)  -- Спад
  | t <= dur - rt = sl  -- Поддержка
  | otherwise = sl * (1 - (t - (dur - rt)) / rt)  -- Затухание
  where
    at = attackTime env
    dt = decayTime env
    sl = sustainLevel env
    rt = releaseTime env

-- | Частота ноты по номеру полутона
-- A4 = 440 Гц, номер полутона 69
noteFrequency :: Int -> Frequency
noteFrequency n = 440 * (2 ** ((fromIntegral n - 69) / 12))

-- | Номера полутонов для основных нот
data NoteName = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Show, Eq, Enum)

-- | Октава (0-8)
type Octave = Int

-- | Полутон в октаве
pitch :: NoteName -> Octave -> Int
pitch name octave = fromEnum name + 12 * octave

-- | Примеры нот
c, d, e, f, g, a, b :: Octave -> Duration -> Signal
c oct dur = note (pitch C oct) dur defaultEnvelope sineWave
d oct dur = note (pitch D oct) dur defaultEnvelope sineWave
e oct dur = note (pitch E oct) dur defaultEnvelope sineWave
f oct dur = note (pitch F oct) dur defaultEnvelope sineWave
g oct dur = note (pitch G oct) dur defaultEnvelope sineWave
a oct dur = note (pitch A oct) dur defaultEnvelope sineWave
b oct dur = note (pitch B oct) dur defaultEnvelope sineWave

-- | Создание ноты с заданными параметрами
note :: Int -> Duration -> Envelope -> (Frequency -> Signal) -> Signal
note pitchNum dur env waveForm =
  let freq = noteFrequency pitchNum
      rawSignal = waveForm freq
      envSignal = applyEnvelope env dur rawSignal
  in \t -> if t < dur then envSignal t else 0