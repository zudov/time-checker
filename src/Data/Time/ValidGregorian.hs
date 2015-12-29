{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Time.ValidGregorian
 ( gregorian
 , Gregorian(..)
 , ValidGregorian
 ) where

import Data.Proxy         (Proxy (..))
import Data.Time.Calendar (Day, fromGregorian)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

gregorian :: forall year month day.
          ( KnownNat year
          , KnownNat month
          , KnownNat day
          , ValidGregorian year month day
          )
          => Gregorian year month day -> Day
gregorian _ = fromGregorian year (fromInteger month) (fromInteger day)
  where
    year  = natVal (Proxy :: Proxy year)
    month = natVal (Proxy :: Proxy month)
    day   = natVal (Proxy :: Proxy day)

data Gregorian (year :: Nat) (month :: Nat) (day :: Nat) = Gregorian

type ValidGregorian year month day = IsValidGregorian year month day ~ 'True

type family IsValidGregorian (year :: Nat) (month :: Nat) (day :: Nat) where
  IsValidGregorian year 01 day = 1 <=? day && day <=? 31
  IsValidGregorian year 02 29  = IsLeapYear year
  IsValidGregorian year 02 day = 1 <=? day && day <=? 28
  IsValidGregorian year 03 day = 1 <=? day && day <=? 31
  IsValidGregorian year 04 day = 1 <=? day && day <=? 30
  IsValidGregorian year 05 day = 1 <=? day && day <=? 31
  IsValidGregorian year 06 day = 1 <=? day && day <=? 30
  IsValidGregorian year 07 day = 1 <=? day && day <=? 31
  IsValidGregorian year 08 day = 1 <=? day && day <=? 31
  IsValidGregorian year 09 day = 1 <=? day && day <=? 30
  IsValidGregorian year 10 day = 1 <=? day && day <=? 31
  IsValidGregorian year 11 day = 1 <=? day && day <=? 30
  IsValidGregorian year 12 day = 1 <=? day && day <=? 31

type family IsLeapYear (n :: Nat) where
  IsLeapYear 0 = 'True
  IsLeapYear n = (DivisibleBy 4 n) && (Not (DivisibleBy 100 n))
              || (DivisibleBy 400 n)

-- | Checks whether `n` divides by `a`
type family DivisibleBy (a :: Nat) (n :: Nat) where
  DivisibleBy 0 n = 'False -- Nothing divides by 0
  DivisibleBy a 0 = 'True  -- 0 divides by anything
  DivisibleBy a n = If (CmpNat a n == 'GT) 'False (DivisibleBy a (n - a))
