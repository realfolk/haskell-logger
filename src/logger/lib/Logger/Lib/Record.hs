{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.Lib.Record
    ( Color (..)
    , Level (..)
    , Record (..)
    , ToRecord (..)
    , bold
    , boldText
    , delimitComma
    , delimitSemicolon
    , delimitSpace
    , leftArrow
    , list
    , mutedRegularText
    , regular
    , regularText
    , rightArrow
    , surroundBraces
    , surroundBrackets
    , text
    ) where

import           Data.List               (intersperse)
import qualified Data.Map                as M
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Lib.Time                as Time
import qualified Lib.Time.Date           as Date
import qualified Lib.Time.Date.Formatter as DateFmt
import qualified Lib.Time.Formatter      as TimeFmt
import qualified Lib.Time.Month          as Month
import qualified Lib.Time.Weekday        as Weekday
import           Numeric.Natural         (Natural)

-- TYPES

data Record
  = Plain !T.Text
  | Formatted !Color !Bool !Record
  | List ![Record]

data Color
  = Level Level
  | Muted
  | Success
  deriving (Show)

data Level = Debug | Info | Warn | Error deriving (Eq, Show)

instance Semigroup Record where
  (<>) (List as) (List bs) = List $ as <> bs
  (<>) (List as) b         = List $ as <> [b]
  (<>) a (List bs)         = List $ a : bs
  (<>) a b                 = List [a, b]

-- CONVERT

class ToRecord a where
  toRecord :: a -> Record

instance ToRecord Record where
  toRecord = id

instance ToRecord T.Text where
  toRecord = Plain

instance ToRecord LT.Text where
  toRecord = Plain . LT.toStrict

showToRecord :: (Show a) => a -> Record
showToRecord = Plain . T.pack . show

instance ToRecord Natural where
  toRecord = showToRecord

instance ToRecord Integer where
  toRecord = showToRecord

instance ToRecord Rational where
  toRecord = showToRecord

instance ToRecord Word where
  toRecord = showToRecord

instance ToRecord Int where
  toRecord = showToRecord

instance ToRecord Float where
  toRecord = showToRecord

instance ToRecord Double where
  toRecord = showToRecord

instance ToRecord Bool where
  toRecord = showToRecord

instance (ToRecord a) => ToRecord [a] where
  toRecord list = surroundBrackets (delimitComma True (map toRecord list))

instance (Ord k, ToRecord k, ToRecord v) => ToRecord [(k, v)] where
  toRecord = toRecord . M.fromList

instance (ToRecord k, ToRecord v) => ToRecord (M.Map k v) where
  toRecord m =
    surroundBraces (delimitSemicolon True (map kvToRecord (M.toList m)))
    where
      kvToRecord (k, v) = delimitSpace [toRecord k, Plain "=", toRecord v]

instance ToRecord Date.Date where
  toRecord = Plain . DateFmt.format (DateFmt.iso8601Date DateFmt.Day)

timeToRecord :: Time.Zone -> Time.Time -> Record
timeToRecord zone = Plain . TimeFmt.format (TimeFmt.iso8601DateAndTime TimeFmt.Day TimeFmt.Millisecond) zone

instance ToRecord Time.Time where
  toRecord = timeToRecord Time.utc

timeUnitToRecord :: Integral a => T.Text -> a -> Record
timeUnitToRecord suffix a = toRecord (toInteger a) <> toRecord suffix

instance ToRecord Time.Years where
  toRecord = timeUnitToRecord "y"

instance ToRecord Time.Month where
  toRecord a =
    Plain $ Month.toText a

instance ToRecord Time.Weeks where
  toRecord = timeUnitToRecord "w"

instance ToRecord Time.Weekday where
  toRecord a =
    Plain $ Weekday.toText a

instance ToRecord Time.Days where
  toRecord = timeUnitToRecord "d"

instance ToRecord Time.Hours where
  toRecord = timeUnitToRecord "h"

instance ToRecord Time.Minutes where
  toRecord = timeUnitToRecord "m"

instance ToRecord Time.Seconds where
  toRecord = timeUnitToRecord "s"

instance ToRecord Time.Milliseconds where
  toRecord = timeUnitToRecord "ms"

instance ToRecord Time.Microseconds where
  toRecord = timeUnitToRecord "µs"

instance ToRecord Time.Nanoseconds where
  toRecord = timeUnitToRecord "ns"

instance ToRecord Time.Picoseconds where
  toRecord = timeUnitToRecord "ps"

-- BUILD

text :: T.Text -> Record
text = Plain

bold :: Color -> Record -> Record
bold color = Formatted color True

boldText :: Color -> T.Text -> Record
boldText color = bold color . text

regular :: Color -> Record -> Record
regular color = Formatted color False

regularText :: Color -> T.Text -> Record
regularText color = regular color . text

mutedRegularText :: T.Text -> Record
mutedRegularText = regular Muted . text

leftArrow :: Record
leftArrow = mutedRegularText "<-"

rightArrow :: Record
rightArrow = mutedRegularText "->"

list :: [Record] -> Record
list = List

-- SURROUND

surroundBraces :: Record -> Record
surroundBraces = surround "{" "}"

surroundBrackets :: Record -> Record
surroundBrackets = surround "[" "]"

surround :: T.Text -> T.Text -> Record -> Record
surround left right record = list [ mutedRegularText left, record, mutedRegularText right ]

-- DELIMIT

delimitSpace :: [Record] -> Record
delimitSpace = delimit " "

delimitComma :: Bool -> [Record] -> Record
delimitComma True  = delimit ", "
delimitComma False = delimit ","

delimitSemicolon :: Bool -> [Record] -> Record
delimitSemicolon True  = delimit "; "
delimitSemicolon False = delimit ";"

delimit :: T.Text -> [Record] -> Record
delimit t = list . intersperse (text t)
