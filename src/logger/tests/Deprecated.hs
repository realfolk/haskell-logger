{-# LANGUAGE OverloadedStrings #-}

module Main
    where

import qualified Logger                as L
import           Logger.Terminal       as LT
import qualified Logger.Terminal.Style as S
import           MDRN.Data

lightTheme :: Theme
lightTheme color =
  case color of
    L.Level L.Debug -> S.LightGrey
    L.Level L.Info  -> S.LightCyan
    L.Level L.Warn  -> S.LightYellow
    L.Level L.Error -> S.LightRed
    L.Muted         -> S.LightGrey
    L.Success       -> S.LightGreen

loggerWithDefaultTheme :: L.Logger
loggerWithDefaultTheme = LT.defaultLogger

logWithDefaultTheme :: L.ToRecord r => r -> IO ()
logWithDefaultTheme = loggerWithDefaultTheme L.Debug . L.toRecord

loggerWithCustomTheme :: L.Logger
loggerWithCustomTheme = LT.logger $ Just lightTheme

logWithCustomTheme :: L.ToRecord r => r -> IO ()
logWithCustomTheme = loggerWithCustomTheme L.Info . L.toRecord

loggerWithNoTheme :: L.Logger
loggerWithNoTheme = LT.logger Nothing

logWithNoTheme :: L.ToRecord r => r -> IO ()
logWithNoTheme = loggerWithNoTheme L.Warn . L.toRecord

main :: IO ()
main = do
  logWithDefaultTheme $ text "--- Default Theme ---"
  logWithDefaultTheme $
    list
      [ text "hello world!"
      , integer 123
      , bool True
      , list [rational 1.2, rational 1.2983982]
      ]
  logWithDefaultTheme $ text "this is the second test"
  logWithDefaultTheme $ text "it's like magic!"
  logWithDefaultTheme $ list [symbol "rgb", integer 255, integer 255, integer 255]
  logWithDefaultTheme L.leftArrow
  logWithDefaultTheme $ L.boldText L.Muted "bold text"
  logWithDefaultTheme $ L.regularText (L.Level L.Warn) "coloured text"

  logWithCustomTheme $ L.regularText (L.Level L.Info) "--- Custom Theme ---"
  logWithCustomTheme $ L.boldText (L.Level L.Warn) "a bold warning"

  logWithNoTheme $ text "--- No Theme ---"
  logWithNoTheme $ L.boldText (L.Level L.Error) "should be no formatting on this one"

  -- Try various levels

  loggerWithDefaultTheme L.Debug $ L.toRecord $ text "---Value---"
  loggerWithDefaultTheme L.Info $ L.toRecord $
    list
      [ text "hello world!"
      , integer 123
      , bool True
      , list [rational 1.2, rational 1.2983982]
      ]
  loggerWithDefaultTheme L.Error $ L.toRecord $ text "this is the second test"
  loggerWithDefaultTheme L.Warn $ L.toRecord $ text "it's like magic!"
  loggerWithDefaultTheme L.Info $ L.toRecord $ text "---ADT---"
  loggerWithDefaultTheme L.Info $ L.toRecord $
    list [symbol "rgb", integer 255, integer 255, integer 255]
  loggerWithDefaultTheme L.Debug $ L.toRecord $ text "---Record---"
  loggerWithDefaultTheme L.Error L.leftArrow
  loggerWithDefaultTheme L.Error $ L.boldText L.Muted "bold text"
  loggerWithDefaultTheme L.Info $ L.regularText (L.Level L.Warn) "coloured text"

  loggerWithCustomTheme L.Info $ L.regularText (L.Level L.Info) "--- Custom Theme ---"
  loggerWithCustomTheme L.Warn $ L.boldText (L.Level L.Warn) "a bold warning"

  loggerWithNoTheme L.Debug $ L.toRecord $ text "--- No Formatting ---"
  loggerWithNoTheme L.Error $ L.boldText (L.Level L.Error) "should be no formatting on this one"
