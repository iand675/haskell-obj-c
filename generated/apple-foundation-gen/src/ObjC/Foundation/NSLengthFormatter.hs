{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLengthFormatter@.
module ObjC.Foundation.NSLengthFormatter
  ( NSLengthFormatter
  , IsNSLengthFormatter(..)
  , stringFromValue_unit
  , stringFromMeters
  , unitStringFromValue_unit
  , unitStringFromMeters_usedUnit
  , getObjectValue_forString_errorDescription
  , numberFormatter
  , setNumberFormatter
  , unitStyle
  , setUnitStyle
  , forPersonHeightUse
  , setForPersonHeightUse
  , forPersonHeightUseSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setForPersonHeightUseSelector
  , setNumberFormatterSelector
  , setUnitStyleSelector
  , stringFromMetersSelector
  , stringFromValue_unitSelector
  , unitStringFromMeters_usedUnitSelector
  , unitStringFromValue_unitSelector
  , unitStyleSelector

  -- * Enum types
  , NSFormattingUnitStyle(NSFormattingUnitStyle)
  , pattern NSFormattingUnitStyleShort
  , pattern NSFormattingUnitStyleMedium
  , pattern NSFormattingUnitStyleLong
  , NSLengthFormatterUnit(NSLengthFormatterUnit)
  , pattern NSLengthFormatterUnitMillimeter
  , pattern NSLengthFormatterUnitCentimeter
  , pattern NSLengthFormatterUnitMeter
  , pattern NSLengthFormatterUnitKilometer
  , pattern NSLengthFormatterUnitInch
  , pattern NSLengthFormatterUnitFoot
  , pattern NSLengthFormatterUnitYard
  , pattern NSLengthFormatterUnitMile

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringFromValue:unit:@
stringFromValue_unit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> NSLengthFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsLengthFormatter value unit =
  sendMessage nsLengthFormatter stringFromValue_unitSelector value unit

-- | @- stringFromMeters:@
stringFromMeters :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> IO (Id NSString)
stringFromMeters nsLengthFormatter numberInMeters =
  sendMessage nsLengthFormatter stringFromMetersSelector numberInMeters

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> NSLengthFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsLengthFormatter value unit =
  sendMessage nsLengthFormatter unitStringFromValue_unitSelector value unit

-- | @- unitStringFromMeters:usedUnit:@
unitStringFromMeters_usedUnit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> Ptr NSLengthFormatterUnit -> IO (Id NSString)
unitStringFromMeters_usedUnit nsLengthFormatter numberInMeters unitp =
  sendMessage nsLengthFormatter unitStringFromMeters_usedUnitSelector numberInMeters unitp

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSLengthFormatter nsLengthFormatter, IsNSString string, IsNSString error_) => nsLengthFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsLengthFormatter obj_ string error_ =
  sendMessage nsLengthFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- numberFormatter@
numberFormatter :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsLengthFormatter =
  sendMessage nsLengthFormatter numberFormatterSelector

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSLengthFormatter nsLengthFormatter, IsNSNumberFormatter value) => nsLengthFormatter -> value -> IO ()
setNumberFormatter nsLengthFormatter value =
  sendMessage nsLengthFormatter setNumberFormatterSelector (toNSNumberFormatter value)

-- | @- unitStyle@
unitStyle :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO NSFormattingUnitStyle
unitStyle nsLengthFormatter =
  sendMessage nsLengthFormatter unitStyleSelector

-- | @- setUnitStyle:@
setUnitStyle :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsLengthFormatter value =
  sendMessage nsLengthFormatter setUnitStyleSelector value

-- | @- forPersonHeightUse@
forPersonHeightUse :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO Bool
forPersonHeightUse nsLengthFormatter =
  sendMessage nsLengthFormatter forPersonHeightUseSelector

-- | @- setForPersonHeightUse:@
setForPersonHeightUse :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> Bool -> IO ()
setForPersonHeightUse nsLengthFormatter value =
  sendMessage nsLengthFormatter setForPersonHeightUseSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector '[CDouble, NSLengthFormatterUnit] (Id NSString)
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromMeters:@
stringFromMetersSelector :: Selector '[CDouble] (Id NSString)
stringFromMetersSelector = mkSelector "stringFromMeters:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector '[CDouble, NSLengthFormatterUnit] (Id NSString)
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromMeters:usedUnit:@
unitStringFromMeters_usedUnitSelector :: Selector '[CDouble, Ptr NSLengthFormatterUnit] (Id NSString)
unitStringFromMeters_usedUnitSelector = mkSelector "unitStringFromMeters:usedUnit:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector '[Ptr RawId, Id NSString, Id NSString] Bool
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @numberFormatter@
numberFormatterSelector :: Selector '[] (Id NSNumberFormatter)
numberFormatterSelector = mkSelector "numberFormatter"

-- | @Selector@ for @setNumberFormatter:@
setNumberFormatterSelector :: Selector '[Id NSNumberFormatter] ()
setNumberFormatterSelector = mkSelector "setNumberFormatter:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector '[] NSFormattingUnitStyle
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector '[NSFormattingUnitStyle] ()
setUnitStyleSelector = mkSelector "setUnitStyle:"

-- | @Selector@ for @forPersonHeightUse@
forPersonHeightUseSelector :: Selector '[] Bool
forPersonHeightUseSelector = mkSelector "forPersonHeightUse"

-- | @Selector@ for @setForPersonHeightUse:@
setForPersonHeightUseSelector :: Selector '[Bool] ()
setForPersonHeightUseSelector = mkSelector "setForPersonHeightUse:"

