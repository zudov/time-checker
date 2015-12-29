{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Time.ValidGregorian
 ( Gregorian(..)
 ) where

import Data.Proxy         (Proxy (..))
import Data.Time.Calendar (Day, fromGregorian)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

data Gregorian (year :: Nat) (month :: Nat) (day :: Nat) = Gregorian

type family ValidGregorian (year :: Nat) (month :: Nat) (day :: Nat) where
  ValidGregorian year 01 day = 1 <=? day && day <=? 31
  ValidGregorian year 02 29  = IsLeapYear year
  ValidGregorian year 02 day = 1 <=? day && day <=? 28
  ValidGregorian year 03 day = 1 <=? day && day <=? 31
  ValidGregorian year 04 day = 1 <=? day && day <=? 30
  ValidGregorian year 05 day = 1 <=? day && day <=? 31
  ValidGregorian year 06 day = 1 <=? day && day <=? 30
  ValidGregorian year 07 day = 1 <=? day && day <=? 31
  ValidGregorian year 08 day = 1 <=? day && day <=? 31
  ValidGregorian year 09 day = 1 <=? day && day <=? 30
  ValidGregorian year 10 day = 1 <=? day && day <=? 31
  ValidGregorian year 11 day = 1 <=? day && day <=? 30
  ValidGregorian year 12 day = 1 <=? day && day <=? 31

gregorianFamily :: forall year month day.
                ( KnownNat year
                , KnownNat month
                , KnownNat day
                , ValidGregorian year month day ~ True
                )
                => Gregorian year month day -> Day
gregorianFamily _ = fromGregorian year (fromInteger month) (fromInteger day)
  where
    year  = natVal (Proxy :: Proxy year)
    month = natVal (Proxy :: Proxy month)
    day   = natVal (Proxy :: Proxy day)

type family IsLeapYear (n :: Nat) where
  IsLeapYear 0 = True
  IsLeapYear n = (DivisibleBy 4 n) && (Not (DivisibleBy 100 n))
              || (DivisibleBy 400 n)

-- | Checks whether `n` divides by `a`
type family DivisibleBy (a :: Nat) (n :: Nat) where
  DivisibleBy 0 n = False -- Nothing divides by 0
  DivisibleBy a 0 = True  -- 0 divides by anything
  DivisibleBy a n = If (CmpNat a n == GT) False (DivisibleBy a (n - a))
