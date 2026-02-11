{-# LANGUAGE PatternSynonyms #-}
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
  , stringFromDistanceSelector
  , distanceFromStringSelector
  , localeSelector
  , setLocaleSelector
  , unitsSelector
  , setUnitsSelector
  , unitStyleSelector
  , setUnitStyleSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- stringFromDistance:@
stringFromDistance :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> CDouble -> IO (Id NSString)
stringFromDistance mkDistanceFormatter  distance =
  sendMsg mkDistanceFormatter (mkSelector "stringFromDistance:") (retPtr retVoid) [argCDouble (fromIntegral distance)] >>= retainedObject . castPtr

-- | @- distanceFromString:@
distanceFromString :: (IsMKDistanceFormatter mkDistanceFormatter, IsNSString distance) => mkDistanceFormatter -> distance -> IO CDouble
distanceFromString mkDistanceFormatter  distance =
withObjCPtr distance $ \raw_distance ->
    sendMsg mkDistanceFormatter (mkSelector "distanceFromString:") retCDouble [argPtr (castPtr raw_distance :: Ptr ())]

-- | @- locale@
locale :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO (Id NSLocale)
locale mkDistanceFormatter  =
  sendMsg mkDistanceFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsMKDistanceFormatter mkDistanceFormatter, IsNSLocale value) => mkDistanceFormatter -> value -> IO ()
setLocale mkDistanceFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkDistanceFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- units@
units :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO MKDistanceFormatterUnits
units mkDistanceFormatter  =
  fmap (coerce :: CULong -> MKDistanceFormatterUnits) $ sendMsg mkDistanceFormatter (mkSelector "units") retCULong []

-- | @- setUnits:@
setUnits :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> MKDistanceFormatterUnits -> IO ()
setUnits mkDistanceFormatter  value =
  sendMsg mkDistanceFormatter (mkSelector "setUnits:") retVoid [argCULong (coerce value)]

-- | @- unitStyle@
unitStyle :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> IO MKDistanceFormatterUnitStyle
unitStyle mkDistanceFormatter  =
  fmap (coerce :: CULong -> MKDistanceFormatterUnitStyle) $ sendMsg mkDistanceFormatter (mkSelector "unitStyle") retCULong []

-- | @- setUnitStyle:@
setUnitStyle :: IsMKDistanceFormatter mkDistanceFormatter => mkDistanceFormatter -> MKDistanceFormatterUnitStyle -> IO ()
setUnitStyle mkDistanceFormatter  value =
  sendMsg mkDistanceFormatter (mkSelector "setUnitStyle:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromDistance:@
stringFromDistanceSelector :: Selector
stringFromDistanceSelector = mkSelector "stringFromDistance:"

-- | @Selector@ for @distanceFromString:@
distanceFromStringSelector :: Selector
distanceFromStringSelector = mkSelector "distanceFromString:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @units@
unitsSelector :: Selector
unitsSelector = mkSelector "units"

-- | @Selector@ for @setUnits:@
setUnitsSelector :: Selector
setUnitsSelector = mkSelector "setUnits:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector
setUnitStyleSelector = mkSelector "setUnitStyle:"

