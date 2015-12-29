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

type IsLeapYear year
  = (year == 1804) || (year == 1808) || (year == 1812) || (year == 1816)
 || (year == 1820) || (year == 1824) || (year == 1828) || (year == 1832)
 || (year == 1836) || (year == 1840) || (year == 1844) || (year == 1848)
 || (year == 1852) || (year == 1856) || (year == 1860) || (year == 1864)
 || (year == 1868) || (year == 1872) || (year == 1876) || (year == 1880)
 || (year == 1884) || (year == 1888) || (year == 1892) || (year == 1896)
 || (year == 1904) || (year == 1908) || (year == 1912) || (year == 1916)
 || (year == 1920) || (year == 1924) || (year == 1928) || (year == 1932)
 || (year == 1936) || (year == 1940) || (year == 1944) || (year == 1948)
 || (year == 1952) || (year == 1956) || (year == 1960) || (year == 1964)
 || (year == 1968) || (year == 1972) || (year == 1976) || (year == 1980)
 || (year == 1984) || (year == 1988) || (year == 1992) || (year == 1996)
 || (year == 2000) || (year == 2004) || (year == 2008) || (year == 2012)
 || (year == 2016) || (year == 2020) || (year == 2024) || (year == 2028)
 || (year == 2032) || (year == 2036) || (year == 2040) || (year == 2044)
 || (year == 2048) || (year == 2052) || (year == 2056) || (year == 2060)
 || (year == 2064) || (year == 2068) || (year == 2072) || (year == 2076)
 || (year == 2080) || (year == 2084) || (year == 2088) || (year == 2092)
 || (year == 2096) || (year == 2104) || (year == 2108) || (year == 2112)
 || (year == 2116) || (year == 2120) || (year == 2124) || (year == 2128)
 || (year == 2132) || (year == 2136) || (year == 2140) || (year == 2144)
 || (year == 2148) || (year == 2152) || (year == 2156) || (year == 2160)
 || (year == 2164) || (year == 2168) || (year == 2172) || (year == 2176)
 || (year == 2180) || (year == 2184) || (year == 2188) || (year == 2192)
 || (year == 2196) || (year == 2204) || (year == 2208) || (year == 2212)
 || (year == 2216) || (year == 2220) || (year == 2224) || (year == 2228)
 || (year == 2232) || (year == 2236) || (year == 2240) || (year == 2244)
 || (year == 2248) || (year == 2252) || (year == 2256) || (year == 2260)
 || (year == 2264) || (year == 2268) || (year == 2272) || (year == 2276)
 || (year == 2280) || (year == 2284) || (year == 2288) || (year == 2292)
 || (year == 2296) || (year == 2304) || (year == 2308) || (year == 2312)
 || (year == 2316) || (year == 2320) || (year == 2324) || (year == 2328)
 || (year == 2332) || (year == 2336) || (year == 2340) || (year == 2344)
 || (year == 2348) || (year == 2352) || (year == 2356) || (year == 2360)
 || (year == 2364) || (year == 2368) || (year == 2372) || (year == 2376)
 || (year == 2380) || (year == 2384) || (year == 2388) || (year == 2392)
 || (year == 2396) || (year == 2400)

