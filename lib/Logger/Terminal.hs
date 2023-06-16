{-|
Module: Logger.Terminal
Description: A logger for the terminal.
Copyright: (c) Real Folk Inc. 2021
Maintainer: admin@realfolk.com
Stability: experimental
Portability: POSIX

A logger for the terminal. It allows for specifying a theme for displaying log messages to a terminal.
-}
module Logger.Terminal
    ( Theme
    , defaultLogger
    , defaultTheme
    , logger
    , render
    ) where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import qualified Logger.Lib.Logger     as L
import qualified Logger.Terminal.Style as S
import           System.IO             (stderr, stdout)

-- TYPES

type Theme = L.Color -> S.Color

-- LOGGER

logger :: Maybe Theme -> L.Logger
logger = L.loggerWithLevelAndTimestamp . baseLogger

baseLogger :: Maybe Theme -> L.Logger
baseLogger maybeTheme level record = TIO.hPutStrLn handle message
  where
    handle = if level == L.Error then stderr else stdout
    message = render maybeTheme record

-- DEFAULTS

defaultLogger :: L.Logger
defaultLogger = logger $ Just defaultTheme

defaultTheme :: Theme
defaultTheme color =
  case color of
    L.Level L.Debug -> S.DarkGrey
    L.Level L.Info  -> S.Cyan
    L.Level L.Warn  -> S.Yellow
    L.Level L.Error -> S.Red
    L.Muted         -> S.DarkGrey
    L.Success       -> S.Green

-- RENDERER

render :: Maybe Theme -> L.Record -> T.Text
render maybeTheme record =
  case record of
    L.Plain text -> text
    L.Formatted color bold record ->
      case maybeTheme of
        Just theme -> format theme color bold $ render maybeTheme record
        Nothing    -> render maybeTheme record
    L.List records -> foldr (\record text -> render maybeTheme record <> text) T.empty records

format :: Theme -> L.Color -> Bool -> T.Text -> T.Text
format theme color = S.format $ theme color
