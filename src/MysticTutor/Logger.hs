module MysticTutor.Logger (logMessage, setupLogger) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified System.Log.FastLogger as FL

-- Functiojn to log messages
setupLogger :: IO FL.LoggerSet
setupLogger = FL.newStdoutLoggerSet defaultBufSize

-- Function to log messages
logMessage :: FL.LoggerSet -> String -> IO ()
logMessage logger msg = do
  time <- getCurrentTime
  let logStr = show time ++ " [Mystic Tutor] " ++ msg
  FL.pushLogStrLn logger (FL.toLogStr logStr)

-- Default buffer size for fast logger
defaultBufSize :: Int
defaultBufSize = 4096
