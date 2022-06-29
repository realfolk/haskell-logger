module Logger.Lib.Logger
    ( Logger
    , module Logger.Lib.Record
    , loggerWithLevelAndTimestamp
    ) where

import qualified Data.Text         as T
import qualified Lib.Time          as Time
import           Logger.Lib.Record

type Logger = Level -> Record -> IO ()

loggerWithLevelAndTimestamp :: Logger -> Logger
loggerWithLevelAndTimestamp baseLogger = logger
  where
    logger level message = Time.now >>= baseLogger level . logLine level message

-- HELPERS

logLine :: Level -> Record -> Time.Time -> Record
logLine level record time = delimitSpace [ header level, timestamp time, record ]

header :: Level -> Record
header level = surroundBrackets $ regular (toColor level) $ levelToRecord level

levelToRecord :: Level -> Record
levelToRecord = toRecord . T.toUpper . T.pack . show

timestamp :: Time.Time -> Record
timestamp = surroundBrackets . regular Muted . toRecord

toColor :: Level -> Color
toColor level =
  case level of
    Debug -> Muted
    Info  -> Level Info
    Warn  -> Level Warn
    Error -> Level Error
