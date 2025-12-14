module Main where

import Synth.Core.Music
import Synth.Render
import Options.Applicative
import Data.Semigroup ((<>))

-- | Команды CLI
data Command = 
    Play Music
  | ExportRaw FilePath Music
  | ExportWav FilePath Music
  | Test

-- | Парсер нотной строки (упрощенный)
parseSimpleMusic :: String -> Music
parseSimpleMusic = cMajorScale  -- Заглушка, будет расширено позже

-- | CLI парсер
commandParser :: Parser Command
commandParser = subparser $
  command "play" (info playCmd (progDesc "Воспроизвести музыку (в разработке)")) 
  <> command "export-raw" (info exportRawCmd (progDesc "Экспорт в RAW файл"))
  <> command "export-wav" (info exportWavCmd (progDesc "Экспорт в WAV файл"))
  <> command "test" (info testCmd (progDesc "Тестовые примеры"))

playCmd :: Parser Command
playCmd = Play <$> (parseSimpleMusic <$> argument str (metavar "MUSIC"))

exportRawCmd :: Parser Command
exportRawCmd = ExportRaw 
  <$> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Выходной файл")
  <*> (parseSimpleMusic <$> argument str (metavar "MUSIC"))

exportWavCmd :: Parser Command
exportWavCmd = ExportWav
  <$> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Выходной файл")
  <*> (parseSimpleMusic <$> argument str (metavar "MUSIC"))

testCmd :: Parser Command
testCmd = pure Test

-- | Обработка команд
handleCommand :: Command -> IO ()
handleCommand (Play music) = do
  putStrLn "Воспроизведение музыки..."
  -- Здесь будет код воспроизведения
  putStrLn $ "Длительность: " ++ show (musicDuration music) ++ " сек"
  
handleCommand (ExportRaw file music) = do
  putStrLn $ "Экспорт в RAW файл: " ++ file
  exportMusicRaw file music
  putStrLn "Готово!"

handleCommand (ExportWav file music) = do
  putStrLn $ "Экспорт в WAV файл: " ++ file
  exportWav file music
  putStrLn "Готово!"

handleCommand Test = do
  putStrLn "Запуск тестовых примеров..."
  
  -- Тест 1: Экспорт гаммы
  putStrLn "1. Экспорт гаммы C мажор в test_scale.wav"
  exportWav "test_scale.wav" cMajorScale
  
  -- Тест 2: Экспорт аккорда
  putStrLn "2. Экспорт аккорда C мажор в test_chord.wav"
  exportWav "test_chord.wav" cMajorChord
  
  -- Тест 3: Экспорт мелодии
  putStrLn "3. Экспорт 'Twinkle Twinkle' в test_twinkle.wav"
  exportWav "test_twinkle.wav" twinkle
  
  putStrLn "Все тесты завершены!"

main :: IO ()
main = do
  command <- execParser $ info (commandParser <**> helper) $
    fullDesc 
    <> progDesc "Музыкальный синтезатор и процессор"
    <> header "music-synth - создание и обработка музыки"
  
  handleCommand command