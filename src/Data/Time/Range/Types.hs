{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Time.Range.Types
  ( StartDate
  , EndDate
  , DayInRange
  , StartHour
  , EndHour
  , Hour
  , LowerTimeBound
  , UpperTimeBound
  , Ranges (..)
  , HasRanges (..)
  ) where

import Control.Lens (makeWrapped, makeClassy)
import Data.Time (Day, UTCTime)

import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

newtype StartDate = StartDate Day
  deriving (Eq,Show,Generic)
makeWrapped ''StartDate

newtype EndDate = EndDate Day
  deriving (Eq,Show,Generic)
makeWrapped ''EndDate

newtype DayInRange = DayInRange Day
  deriving (Eq,Show,Generic)
makeWrapped ''DayInRange

newtype StartHour = StartHour Natural
  deriving (Eq,Show,Generic)
makeWrapped ''StartHour

newtype EndHour = EndHour Natural
  deriving (Eq,Show,Generic)
makeWrapped ''EndHour

newtype Hour = Hour Text
  deriving (Eq,Show,Generic)
makeWrapped ''Hour

newtype LowerTimeBound = LowerBound UTCTime
  deriving (Show, Eq, Generic)
makeWrapped ''LowerTimeBound

newtype UpperTimeBound = UpperBound UTCTime
  deriving (Show, Eq, Generic)
makeWrapped ''UpperTimeBound

data Ranges = Ranges
  { _rangesLowerBound  :: LowerTimeBound
  , _rangesUpperBound  :: UpperTimeBound
  , _rangesDaysInRange :: NonEmpty DayInRange
  , _rangesUtcTimes    :: NonEmpty UTCTime
  }
  deriving (Show, Eq, Generic)
makeClassy ''Ranges
