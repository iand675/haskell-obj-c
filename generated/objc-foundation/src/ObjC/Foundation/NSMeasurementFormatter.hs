{-# LANGUAGE PatternSynonyms #-}
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
  , stringFromMeasurementSelector
  , stringFromUnitSelector
  , unitOptionsSelector
  , setUnitOptionsSelector
  , unitStyleSelector
  , setUnitStyleSelector
  , localeSelector
  , setLocaleSelector
  , numberFormatterSelector
  , setNumberFormatterSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringFromMeasurement:@
stringFromMeasurement :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSMeasurement measurement) => nsMeasurementFormatter -> measurement -> IO (Id NSString)
stringFromMeasurement nsMeasurementFormatter  measurement =
withObjCPtr measurement $ \raw_measurement ->
    sendMsg nsMeasurementFormatter (mkSelector "stringFromMeasurement:") (retPtr retVoid) [argPtr (castPtr raw_measurement :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromUnit:@
stringFromUnit :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSUnit unit) => nsMeasurementFormatter -> unit -> IO (Id NSString)
stringFromUnit nsMeasurementFormatter  unit =
withObjCPtr unit $ \raw_unit ->
    sendMsg nsMeasurementFormatter (mkSelector "stringFromUnit:") (retPtr retVoid) [argPtr (castPtr raw_unit :: Ptr ())] >>= retainedObject . castPtr

-- | @- unitOptions@
unitOptions :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO NSMeasurementFormatterUnitOptions
unitOptions nsMeasurementFormatter  =
  fmap (coerce :: CULong -> NSMeasurementFormatterUnitOptions) $ sendMsg nsMeasurementFormatter (mkSelector "unitOptions") retCULong []

-- | @- setUnitOptions:@
setUnitOptions :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> NSMeasurementFormatterUnitOptions -> IO ()
setUnitOptions nsMeasurementFormatter  value =
  sendMsg nsMeasurementFormatter (mkSelector "setUnitOptions:") retVoid [argCULong (coerce value)]

-- | @- unitStyle@
unitStyle :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO NSFormattingUnitStyle
unitStyle nsMeasurementFormatter  =
  fmap (coerce :: CLong -> NSFormattingUnitStyle) $ sendMsg nsMeasurementFormatter (mkSelector "unitStyle") retCLong []

-- | @- setUnitStyle:@
setUnitStyle :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsMeasurementFormatter  value =
  sendMsg nsMeasurementFormatter (mkSelector "setUnitStyle:") retVoid [argCLong (coerce value)]

-- | @- locale@
locale :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO (Id NSLocale)
locale nsMeasurementFormatter  =
  sendMsg nsMeasurementFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSLocale value) => nsMeasurementFormatter -> value -> IO ()
setLocale nsMeasurementFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMeasurementFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberFormatter@
numberFormatter :: IsNSMeasurementFormatter nsMeasurementFormatter => nsMeasurementFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsMeasurementFormatter  =
  sendMsg nsMeasurementFormatter (mkSelector "numberFormatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSMeasurementFormatter nsMeasurementFormatter, IsNSNumberFormatter value) => nsMeasurementFormatter -> value -> IO ()
setNumberFormatter nsMeasurementFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMeasurementFormatter (mkSelector "setNumberFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromMeasurement:@
stringFromMeasurementSelector :: Selector
stringFromMeasurementSelector = mkSelector "stringFromMeasurement:"

-- | @Selector@ for @stringFromUnit:@
stringFromUnitSelector :: Selector
stringFromUnitSelector = mkSelector "stringFromUnit:"

-- | @Selector@ for @unitOptions@
unitOptionsSelector :: Selector
unitOptionsSelector = mkSelector "unitOptions"

-- | @Selector@ for @setUnitOptions:@
setUnitOptionsSelector :: Selector
setUnitOptionsSelector = mkSelector "setUnitOptions:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector
setUnitStyleSelector = mkSelector "setUnitStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @numberFormatter@
numberFormatterSelector :: Selector
numberFormatterSelector = mkSelector "numberFormatter"

-- | @Selector@ for @setNumberFormatter:@
setNumberFormatterSelector :: Selector
setNumberFormatterSelector = mkSelector "setNumberFormatter:"

