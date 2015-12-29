{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Data.Time.ValidGregorian
 ( validGregorian
 , Gregorian(..)
 , ValidGregorian
 ) where

import Data.Proxy (Proxy(..))
import Data.Time.Calendar (Day, fromGregorian)
import GHC.TypeLits


class ValidGregorian (year :: Nat) (month :: Nat) (day :: Nat)

data Gregorian (year :: Nat) (month :: Nat) (day :: Nat) = Gregorian

validGregorian :: forall year month day.
               ( ValidGregorian year month day
               , KnownNat year
               , KnownNat month
               , KnownNat day
               )
               => Gregorian year month day -> Day
validGregorian _ = fromGregorian year (fromInteger month) (fromInteger day)
  where
    year  = natVal (Proxy :: Proxy year)
    month = natVal (Proxy :: Proxy month)
    day   = natVal (Proxy :: Proxy day)

instance (day <= 31, 1 <= day) => ValidGregorian year 01 day
instance (day <= 28, 1 <= day) => ValidGregorian year 02 day
instance (day <= 31, 1 <= day) => ValidGregorian year 03 day
instance (day <= 30, 1 <= day) => ValidGregorian year 04 day
instance (day <= 31, 1 <= day) => ValidGregorian year 05 day
instance (day <= 30, 1 <= day) => ValidGregorian year 06 day
instance (day <= 31, 1 <= day) => ValidGregorian year 07 day
instance (day <= 31, 1 <= day) => ValidGregorian year 08 day
instance (day <= 30, 1 <= day) => ValidGregorian year 09 day
instance (day <= 31, 1 <= day) => ValidGregorian year 10 day
instance (day <= 30, 1 <= day) => ValidGregorian year 11 day
instance (day <= 31, 1 <= day) => ValidGregorian year 12 day

-- TODO: Handle leap years
