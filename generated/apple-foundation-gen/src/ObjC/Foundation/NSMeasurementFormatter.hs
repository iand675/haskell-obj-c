{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMeasurementFormatter@.
module ObjC.Foundation.NSMeasurementFormatter
  ( NSMeasurementFormatter
  , IsNSMeasurementFormatter(..)
  , stringFromMeasurement
  , stringFromUnit
  , unitOptions
  , setUnitOptions
  , unitStyle
  , setUnitStyle
  , locale
  , setLocale
  , numberFormatter
  , setNumberFormatter
  , localeSelector
  , numberFormatterSelector
  , setLocaleSelector
  , setNumberFormatterSelector
  , setUnitOptionsSelector
  , setUnitStyleSelector
  , stringFromMeasurementSelector
  , stringFromUnitSelector
  , unitOptionsSelector
  , unitStyleSelector

  -- * Enum types
  , NSFormattingUnitStyle(NSFormattingUnitStyle)
  , pattern NSFormattingUnitStyleShort
  , pattern NSFormattingUnitStyleMedium
  , pattern NSFormattingUnitStyleLong
  , NSMeasurementFormatterUnitOptions(NSMeasurementFormatterUnitOptions)
  , pattern NSMeasurementFormatterUnitOptionsProvidedUnit
  , pattern NSMeasurementFormatterUnitOptionsNaturalScale
  , pattern NSMeasurementFormatterUnitOptionsTemperatureWithoutUnit

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringFromMeasurement:@
stringFromMeasurement :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSMeasurement measurement) => nsMeasurementFormatter -> measurement -> IO (Id NSString)
stringFromMeasurement nsMeasurementFormatter measurement =
  sendMessage nsMeasurementFormatter stringFromMeasurementSelector (toNSMeasurement measurement)

-- | @- stringFromUnit:@
stringFromUnit :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSUnit unit) => nsMeasurementFormatter -> unit -> IO (Id NSString)
stringFromUnit nsMeasurementFormatter unit =
  sendMessage nsMeasurementFormatter stringFromUnitSelector (toNSUnit unit)

-- | @- unitOptions@
unitOptions :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO NSMeasurementFormatterUnitOptions
unitOptions nsMeasurementFormatter =
  sendMessage nsMeasurementFormatter unitOptionsSelector

-- | @- setUnitOptions:@
setUnitOptions :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> NSMeasurementFormatterUnitOptions -> IO ()
setUnitOptions nsMeasurementFormatter value =
  sendMessage nsMeasurementFormatter setUnitOptionsSelector value

-- | @- unitStyle@
unitStyle :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO NSFormattingUnitStyle
unitStyle nsMeasurementFormatter =
  sendMessage nsMeasurementFormatter unitStyleSelector

-- | @- setUnitStyle:@
setUnitStyle :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsMeasurementFormatter value =
  sendMessage nsMeasurementFormatter setUnitStyleSelector value

-- | @- locale@
locale :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO (Id NSLocale)
locale nsMeasurementFormatter =
  sendMessage nsMeasurementFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSLocale value) => nsMeasurementFormatter -> value -> IO ()
setLocale nsMeasurementFormatter value =
  sendMessage nsMeasurementFormatter setLocaleSelector (toNSLocale value)

-- | @- numberFormatter@
numberFormatter :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsMeasurementFormatter =
  sendMessage nsMeasurementFormatter numberFormatterSelector

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSNumberFormatter value) => nsMeasurementFormatter -> value -> IO ()
setNumberFormatter nsMeasurementFormatter value =
  sendMessage nsMeasurementFormatter setNumberFormatterSelector (toNSNumberFormatter value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromMeasurement:@
stringFromMeasurementSelector :: Selector '[Id NSMeasurement] (Id NSString)
stringFromMeasurementSelector = mkSelector "stringFromMeasurement:"

-- | @Selector@ for @stringFromUnit:@
stringFromUnitSelector :: Selector '[Id NSUnit] (Id NSString)
stringFromUnitSelector = mkSelector "stringFromUnit:"

-- | @Selector@ for @unitOptions@
unitOptionsSelector :: Selector '[] NSMeasurementFormatterUnitOptions
unitOptionsSelector = mkSelector "unitOptions"

-- | @Selector@ for @setUnitOptions:@
setUnitOptionsSelector :: Selector '[NSMeasurementFormatterUnitOptions] ()
setUnitOptionsSelector = mkSelector "setUnitOptions:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector '[] NSFormattingUnitStyle
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector '[NSFormattingUnitStyle] ()
setUnitStyleSelector = mkSelector "setUnitStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @numberFormatter@
numberFormatterSelector :: Selector '[] (Id NSNumberFormatter)
numberFormatterSelector = mkSelector "numberFormatter"

-- | @Selector@ for @setNumberFormatter:@
setNumberFormatterSelector :: Selector '[Id NSNumberFormatter] ()
setNumberFormatterSelector = mkSelector "setNumberFormatter:"

