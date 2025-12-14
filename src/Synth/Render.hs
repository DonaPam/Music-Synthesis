module Synth.Render where

import Synth.Core.Signal
import Synth.Core.Music
import qualified Data.Vector as V
import Data.Word (Word16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Binary.Put (runPut, putWord16le)

-- | Рендеринг музыки в семплы
renderMusic :: Music -> Samples
renderMusic music = 
  let dur = musicDuration music
      sig = interpret music
  in renderSamples dur sig

-- | Конвертация амплитуды в 16-битное целое
amplitudeToWord16 :: Amplitude -> Word16
amplitudeToWord16 amp = 
  let clamped = max (-1.0) (min 1.0 amp)
      scaled = clamped * fromIntegral (maxBound :: Word16) / 2
  in fromIntegral (floor (scaled + fromIntegral (maxBound :: Word16) / 2))

-- | Экспорт в RAW 16-битный файл
exportRaw :: FilePath -> Samples -> IO ()
exportRaw filePath samples = do
  let bytes = BS.concat . V.toList $ V.map (\amp -> 
        runPut (putWord16le (amplitudeToWord16 amp))) samples
  BS.writeFile filePath bytes

-- | Экспорт музыки в RAW файл
exportMusicRaw :: FilePath -> Music -> IO ()
exportMusicRaw filePath music = do
  let samples = renderMusic music
  exportRaw filePath samples

-- | Простой WAV заголовок (моно, 44100 Гц, 16 бит)
wavHeader :: Int -> BS.ByteString
wavHeader numSamples = 
  let byteRate = sampleRate * 2 * 1  -- sampleRate * bytesPerSample * numChannels
      dataSize = numSamples * 2  -- 2 bytes per sample
      fileSize = 36 + dataSize
  in BS.pack $ concat
    [ "RIFF"
    , toBytes 4 fileSize        -- ChunkSize
    , "WAVE"
    , "fmt "
    , toBytes 4 16              -- Subchunk1Size (16 for PCM)
    , toBytes 2 1               -- AudioFormat (PCM = 1)
    , toBytes 2 1               -- NumChannels (mono = 1)
    , toBytes 4 (floor sampleRate)  -- SampleRate
    , toBytes 4 (floor byteRate)    -- ByteRate
    , toBytes 2 2               -- BlockAlign
    , toBytes 2 16              -- BitsPerSample
    , "data"
    , toBytes 4 dataSize        -- Subchunk2Size
    ]
  where
    toBytes :: Int -> Int -> [Word8]
    toBytes n x = map (fromIntegral . ((x `shiftR`) . (*8))) [0..n-1]

-- | Экспорт в WAV файл
exportWav :: FilePath -> Music -> IO ()
exportWav filePath music = do
  let samples = renderMusic music
      numSamples = V.length samples
      header = wavHeader numSamples
      
  -- Конвертируем семплы в байты
  let audioData = BS.concat . V.toList $ V.map (\amp -> 
        runPut (putWord16le (amplitudeToWord16 amp))) samples
  
  BS.writeFile filePath (BS.append header audioData)