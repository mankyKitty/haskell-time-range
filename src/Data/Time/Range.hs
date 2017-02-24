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
                                        _3, _Wrapped)

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

instance (Choice p, Applicative f) => AsDay p f (Integer, Int, Int) where
  _Day = prism' toGregorian (\(y,m,d) -> fromGregorianValid y m d)

instance (Choice p, Applicative f) => AsDay p f String where
  _Day = prism' showGregorian (parseTimeM False defaultTimeLocale "%Y-%m-%d")

dayParse :: String -> Either String Day
dayParse s = maybe err Right $
  nom "%F" <|>
  nom "%Y%m%d" <|>
  nom "%d/%m/%Y" <|>
  nom "%D"
  where
    nom f = parseTimeM True defaultTimeLocale f s
    err = Left $ "Unable to parse Date Accepts [yyyy-mm-dd, yyyymmdd, mm/dd/yy, dd/mm/yyyy]: " <> s

bounds
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
bounds sd ed sh eh =
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

strToHourNatural :: String -> Either String Natural
strToHourNatural s = maybe err toNat $ readMaybe (trim s)
  where
    trim = let f = reverse . dropWhile (== ' ') in f . f

    err = Left ( "Unable to parse " <> s <>
                 " into acceptable hour value. Between '1'-'23' or '00'"
               )
    toNat n = if isHour n then Right (fromInteger n) else err

    isHour n = (n >= 1 && n <= 23) || (s == "00" && n == 0)

utcRangeHours
  :: ( Rewrapped start start
     , Rewrapped end end
     , Unwrapped start ~ UTCTime
     , Unwrapped end ~ UTCTime
     )
  => start
  -> end
  -> NonEmpty UTCTime
utcRangeHours s e =
  runIdentity <$> wrappedRange Identity (addUTCTime oneHour) s e
  where
    oneHour = 3600

datesInRange
  :: ( Rewrapped a a
     , Rewrapped b b
     , Unwrapped a ~ Day
     , Unwrapped b ~ Day
     )
    => a -> b -> NonEmpty DayInRange
datesInRange =
  wrappedRange (^. from _Wrapped) (addDays 1)

buildRanges
  :: StartDate
  -> Maybe EndDate
  -> StartHour
  -> EndHour
  -> Ranges
buildRanges startD mayEndD startH endH =
  Ranges lower upper neDays utcz
  where
    endD' = fromMaybe (startD ^. _Wrapped . to (^. from _Wrapped)) mayEndD
    (lower, upper) = bounds startD endD' startH endH
    neDays = datesInRange startD endD'
    utcz = utcRangeHours lower upper
