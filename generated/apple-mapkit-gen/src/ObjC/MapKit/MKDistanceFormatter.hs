{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKDistanceFormatter@.
module ObjC.MapKit.MKDistanceFormatter
  ( MKDistanceFormatter
  , IsMKDistanceFormatter(..)
  , stringFromDistance
  , distanceFromString
  , locale
  , setLocale
  , units
  , setUnits
  , unitStyle
  , setUnitStyle
  , distanceFromStringSelector
  , localeSelector
  , setLocaleSelector
  , setUnitStyleSelector
  , setUnitsSelector
  , stringFromDistanceSelector
  , unitStyleSelector
  , unitsSelector

  -- * Enum types
  , MKDistanceFormatterUnitStyle(MKDistanceFormatterUnitStyle)
  , pattern MKDistanceFormatterUnitStyleDefault
  , pattern MKDistanceFormatterUnitStyleAbbreviated
  , pattern MKDistanceFormatterUnitStyleFull
  , MKDistanceFormatterUnits(MKDistanceFormatterUnits)
  , pattern MKDistanceFormatterUnitsDefault
  , pattern MKDistanceFormatterUnitsMetric
  , pattern MKDistanceFormatterUnitsImperial
  , pattern MKDistanceFormatterUnitsImperialWithYards

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- stringFromDistance:@
stringFromDistance :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> CDouble -> IO (Id NSString)
stringFromDistance mkDistanceFormatter distance =
  sendMessage mkDistanceFormatter stringFromDistanceSelector distance

-- | @- distanceFromString:@
distanceFromString :: (IsMKDistanceFormatter mkDistanceFormatter, IsNSString distance) => mkDistanceFormatter -> distance -> IO CDouble
distanceFromString mkDistanceFormatter distance =
  sendMessage mkDistanceFormatter distanceFromStringSelector (toNSString distance)

-- | @- locale@
locale :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO (Id NSLocale)
locale mkDistanceFormatter =
  sendMessage mkDistanceFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsMKDistanceFormatter mkDistanceFormatter, IsNSLocale value) => mkDistanceFormatter -> value -> IO ()
setLocale mkDistanceFormatter value =
  sendMessage mkDistanceFormatter setLocaleSelector (toNSLocale value)

-- | @- units@
units :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO MKDistanceFormatterUnits
units mkDistanceFormatter =
  sendMessage mkDistanceFormatter unitsSelector

-- | @- setUnits:@
setUnits :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> MKDistanceFormatterUnits -> IO ()
setUnits mkDistanceFormatter value =
  sendMessage mkDistanceFormatter setUnitsSelector value

-- | @- unitStyle@
unitStyle :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO MKDistanceFormatterUnitStyle
unitStyle mkDistanceFormatter =
  sendMessage mkDistanceFormatter unitStyleSelector

-- | @- setUnitStyle:@
setUnitStyle :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> MKDistanceFormatterUnitStyle -> IO ()
setUnitStyle mkDistanceFormatter value =
  sendMessage mkDistanceFormatter setUnitStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromDistance:@
stringFromDistanceSelector :: Selector '[CDouble] (Id NSString)
stringFromDistanceSelector = mkSelector "stringFromDistance:"

-- | @Selector@ for @distanceFromString:@
distanceFromStringSelector :: Selector '[Id NSString] CDouble
distanceFromStringSelector = mkSelector "distanceFromString:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @units@
unitsSelector :: Selector '[] MKDistanceFormatterUnits
unitsSelector = mkSelector "units"

-- | @Selector@ for @setUnits:@
setUnitsSelector :: Selector '[MKDistanceFormatterUnits] ()
setUnitsSelector = mkSelector "setUnits:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector '[] MKDistanceFormatterUnitStyle
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector '[MKDistanceFormatterUnitStyle] ()
setUnitStyleSelector = mkSelector "setUnitStyle:"

