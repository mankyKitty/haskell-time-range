{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Time.Range where

import           Prelude               (Eq, Int, Integer, Ord, Show,
                                        fromInteger, toInteger, (*), (+), (<),
                                        (<=), (==), (>=))

import           Control.Applicative   (Applicative, (<$>), (<|>))
import           Control.Category      ((.))
import           Control.Lens          (Choice, Optic', Prism', Rewrapped,
                                        Unwrapped, from, makeClassy, makeLenses,
                                        makeWrapped, prism', to, (^.), _1, _2,
                                        _3, _Wrapped, (#))

import           GHC.Generics          (Generic)
import           GHC.Natural           (Natural)

import           Data.Text             (Text)

import           Data.Bool             (Bool (..), (&&), (||))
import           Data.Either           (Either (..))
import           Data.Function         (const, ($))
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Data.List             (dropWhile, reverse)
import           Data.List.NonEmpty    (NonEmpty (..), unfold)
import           Data.Maybe            (Maybe (..), fromMaybe, maybe)
import           Data.Monoid           ((<>))
import           Data.String           (String)
import           Data.Time             (Day, UTCTime (..), addDays, addUTCTime,
                                        defaultTimeLocale, fromGregorianValid,
                                        parseTimeM, secondsToDiffTime,
                                        showGregorian, toGregorian)
import           Text.Read             (readMaybe)

import           Data.Time.Range.Types

class AsDay p f s where
  _Day :: Optic' p f s Day

-- | @Prism@ for going between the Gregorian version of the @Day@ type
-- $setup
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> :set -XFlexibleContexts
-- >>> let d = fromGregorian (2017 :: Integer) (2 :: Int) (25 :: Int)

-- |
-- >>> d ^. re _Day :: (Integer, Int, Int)
-- (2017,2,25)
-- >>> (2017 :: Integer, 2 :: Int, 25 :: Int) ^? _Day
-- Just 2017-02-25
-- >>> (2017 :: Integer, 2 :: Int, 35 :: Int) ^? _Day
-- Nothing
instance (Choice p, Applicative f) => AsDay p f (Integer, Int, Int) where
  _Day = prism' toGregorian (\(y,m,d) -> fromGregorianValid y m d)

-- |
-- >>> d ^. re _Day :: String
-- "2017-02-25"
-- >>> "2017-02-25" ^? _Day
-- Just 2017-02-25
instance (Choice p, Applicative f) => AsDay p f String where
  _Day = prism' showGregorian (parseTimeM False defaultTimeLocale "%Y-%m-%d")

-- | Parse some common date formats to a @Day@
--
-- Examples:
--
-- >>> dayParse "2017-01-09"
-- Right 2017-01-09
-- >>> dayParse "09/01/2017"
-- Right 2017-01-09
-- >>> dayParse "30/02/2017"
-- Left "Unable to parse Date Accepts [yyyy-mm-dd, yyyymmdd, mm/dd/yy, dd/mm/yyyy]: 30/02/2017"
-- >>> dayParse "20170109"
-- Right 2017-01-09
-- >>> dayParse "010917"
-- Left "Unable to parse Date Accepts [yyyy-mm-dd, yyyymmdd, mm/dd/yy, dd/mm/yyyy]: 010917"
-- >>> dayParse "02/13/17"
-- Right 2017-02-13
-- >>> dayParse "2017-01-09"
-- Right 2017-01-09
dayParse :: String -> Either String Day
dayParse s = maybe err Right $
  nom "%F" <|>
  nom "%0Y%m%d" <|>
  nom "%d/%m/%Y" <|>
  nom "%D"
  where
    nom f = parseTimeM True defaultTimeLocale f s
    err = Left $ "Unable to parse Date Accepts [yyyy-mm-dd, yyyymmdd, mm/dd/yy, dd/mm/yyyy]: " <> s

-- | Given a wrapped start and end @Day@, as well as a chosen @Hour@, create a pair of wrapped @UTCTime@
-- values that represent their upper and lower values. Used internally.
-- $setup
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> :set -XFlexibleContexts
-- >>> let startD = _Wrapped # (fromGregorian (2017 :: Integer) (2 :: Int) (25 :: Int)) :: StartDate
-- >>> let endD = _Wrapped # (fromGregorian (2017 :: Integer) (2 :: Int) (27 :: Int)) :: EndDate
-- >>> let startH = (_Wrapped # 3) :: StartHour
-- >>> let endH = (_Wrapped # 4) :: EndHour

-- |
-- >>> bounds startD endD startH endH
-- LowerBound 2017-02-25 03:00:00 UTC,UpperBound 2017-02-27 04:00:00 UTC)
boundFromWrapped
  :: ( Rewrapped s s
     , Rewrapped e e
     , Unwrapped s ~ Day
     , Unwrapped e ~ Day
     , Rewrapped hs hs
     , Rewrapped he he
     , Unwrapped hs ~ Natural
     , Unwrapped he ~ Natural
     )
  => s
  -> e
  -> hs
  -> he
  -> (LowerTimeBound, UpperTimeBound)
boundFromWrapped sd ed sh eh =
  ( (toUtc sd sh) ^. from _Wrapped
  , (toUtc ed eh) ^. from _Wrapped
  )
  where
    handle00 00 = h + m + s
      where
        h = 23 * 60 * 60
        m = 59 * 60
        s = 59
    handle00 ss = ss * 60 * 60

    toUtc d h = UTCTime
      (d ^. _Wrapped)
      (secondsToDiffTime
      . handle00
      . toInteger
      $ h ^. _Wrapped
      )

-- | Provide a range builder for any wrapped types that have
-- an @Ord@ instance.
-- $setup
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> import Data.List.NonEmpty (NonEmpty)
-- >>> let startH = (_Wrapped # 3) :: StartHour
-- >>> let endH = (_Wrapped # 5) :: EndHour

-- |
-- >>> wrappedRange (_Wrapped #) succ startH endH :: NonEmpty StartHour
-- StartHour 3 :| [StartHour 4, StartHour 5]
-- >>> wrappedRange (_Wrapped #) succ endH startH :: NonEmpty StartHour
-- StartHour 5 :| []
wrappedRange
  :: ( Rewrapped a a
     , Rewrapped b b
     , Rewrapped c c
     , Unwrapped a ~ t
     , Unwrapped b ~ t
     , Unwrapped c ~ t
     , Ord t
     )
  => (t -> c)
  -> (t -> t)
  -> a
  -> b
  -> NonEmpty c
wrappedRange con inc s e =
  unfold next (s ^. _Wrapped . to con)
  where
    haveNext cc =
      if (e ^. _Wrapped) < (cc ^. _Wrapped)
      then Nothing
      else Just cc

    next c =
      (c,  c ^. _Wrapped . to (haveNext . con . inc))

-- | Try to read a @String@ value into a @Natural@ representing a whole hour,
-- in 24 hour time. "1"  .. "23" or "00"
--
-- >>> strToHourNatural "1"
-- Right 1
-- >>> strToHourNatural "10"
-- Right 10
-- >>> strToHourNatural "0"
-- Left "Unable to parse 0 into acceptable hour value. Expected '1'-'23' or '00'"
-- >>> strToHourNatural  "00"
-- Right 0
strToHourNatural :: String -> Either String Natural
strToHourNatural s = maybe err toNat $ readMaybe (trim s)
  where
    trim = let f = reverse . dropWhile (== ' ') in f . f

    err = Left ( "Unable to parse " <> s <>
                 " into acceptable hour value. Expected '1'-'23' or '00'"
               )
    toNat n = if isHour n then Right (fromInteger n) else err

    isHour n = (n >= 1 && n <= 23) || (s == "00" && n == 0)

-- | Convenience function for @wrappedRange@ that provides a list of @UTCTime@
-- at hourly intervals between the given start and end times.
utcRangeHours
  :: ( Rewrapped start start
     , Rewrapped end end
     , Unwrapped start ~ UTCTime
     , Unwrapped end ~ UTCTime
     )
  => start
  -> end
  -> NonEmpty UTCTime
utcRangeHours s e = runIdentity <$>
  wrappedRange Identity (addUTCTime oneHour) s e
  where
    oneHour = 3600

-- | Given two wrapped @Day@ values, provide a @NonEmpty@ list of all the days
-- in between.
-- $setup
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> :set -XFlexibleContexts
-- >>> let startD = _Wrapped # (fromGregorian (2017 :: Integer) (2 :: Int) (20 :: Int)) :: StartDate
-- >>> let endD = _Wrapped # (fromGregorian (2017 :: Integer) (2 :: Int) (27 :: Int)) :: EndDate

-- |
-- >>> datesInRange startD endD
-- DayInRange 2017-02-20 :| [DayInRange 2017-02-21,DayInRange 2017-02-22,DayInRange 2017-02-23,DayInRange 2017-02-24,DayInRange 2017-02-25,DayInRange 2017-02-26,DayInRange 2017-02-27]
-- >>> datesInRange endD startD
-- DayInRange 2017-02-27 :| []
datesInRange
  :: ( Rewrapped a a
     , Rewrapped b b
     , Unwrapped a ~ Day
     , Unwrapped b ~ Day
     )
    => a -> b -> NonEmpty DayInRange
datesInRange =
  wrappedRange (_Wrapped #) (addDays 1)

-- | From the given starting points, construct a @Ranges@ record with ranges
-- and bounds constructed. Not providing an @EndDate@ will use the @StartDate@.
buildRanges
  :: StartDate
  -> Maybe EndDate
  -> StartHour
  -> EndHour
  -> Ranges
buildRanges startD mayEndD startH endH =
  Ranges lower upper neDays utcz
  where
    endD' = fromMaybe (startD ^. _Wrapped . to (_Wrapped #)) mayEndD
    (lower, upper) = boundFromWrapped startD endD' startH endH
    neDays = datesInRange startD endD'
    utcz = utcRangeHours lower upper
