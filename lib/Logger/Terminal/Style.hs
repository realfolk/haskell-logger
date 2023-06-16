{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Logger.Terminal.Style
Description: Style text for logging on a terminal.
Copyright: (c) Real Folk Inc. 2021
Maintainer: admin@realfolk.com
Stability: experimental
Portability: POSIX

Add escape codes for formatting text, adding color or making it bold, for output to a terminal.
-}
module Logger.Terminal.Style
    ( Color (..)
    , format
    ) where

import qualified Data.Text as T

data Color = Foreground | Black | Red | Green | Yellow | Blue | Magenta | Cyan | LightGrey | DarkGrey | LightRed | LightGreen | LightYellow | LightBlue | LightMagenta | LightCyan | White

format :: Color -> Bool -> T.Text -> T.Text
format color bold message = T.concat [ colorToSequence color, if bold then boldSequence else "", message, resetSequence ]

-- HELPERS

makeSequence :: T.Text -> T.Text
makeSequence code = T.concat ["\ESC[", code, "m"]

boldSequence :: T.Text
boldSequence = makeSequence "1"

resetSequence :: T.Text
resetSequence = makeSequence "0"

colorToSequence :: Color -> T.Text
colorToSequence tColor =
  case tColor of
    Foreground   -> makeSequence "39"
    Black        -> makeSequence "30"
    Red          -> makeSequence "31"
    Green        -> makeSequence "32"
    Yellow       -> makeSequence "33"
    Blue         -> makeSequence "34"
    Magenta      -> makeSequence "35"
    Cyan         -> makeSequence "36"
    LightGrey    -> makeSequence "37"
    DarkGrey     -> makeSequence "90"
    LightRed     -> makeSequence "91"
    LightGreen   -> makeSequence "92"
    LightYellow  -> makeSequence "93"
    LightBlue    -> makeSequence "94"
    LightMagenta -> makeSequence "95"
    LightCyan    -> makeSequence "96"
    White        -> makeSequence "97"
