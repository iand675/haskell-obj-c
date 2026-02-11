{-# LANGUAGE PatternSynonyms #-}
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
  , stringFromValue_unitSelector
  , stringFromMetersSelector
  , unitStringFromValue_unitSelector
  , unitStringFromMeters_usedUnitSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setNumberFormatterSelector
  , unitStyleSelector
  , setUnitStyleSelector
  , forPersonHeightUseSelector
  , setForPersonHeightUseSelector

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

-- | @- stringFromValue:unit:@
stringFromValue_unit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> NSLengthFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsLengthFormatter  value unit =
  sendMsg nsLengthFormatter (mkSelector "stringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- stringFromMeters:@
stringFromMeters :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> IO (Id NSString)
stringFromMeters nsLengthFormatter  numberInMeters =
  sendMsg nsLengthFormatter (mkSelector "stringFromMeters:") (retPtr retVoid) [argCDouble (fromIntegral numberInMeters)] >>= retainedObject . castPtr

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> NSLengthFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsLengthFormatter  value unit =
  sendMsg nsLengthFormatter (mkSelector "unitStringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- unitStringFromMeters:usedUnit:@
unitStringFromMeters_usedUnit :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> CDouble -> Ptr NSLengthFormatterUnit -> IO (Id NSString)
unitStringFromMeters_usedUnit nsLengthFormatter  numberInMeters unitp =
  sendMsg nsLengthFormatter (mkSelector "unitStringFromMeters:usedUnit:") (retPtr retVoid) [argCDouble (fromIntegral numberInMeters), argPtr unitp] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSLengthFormatter nsLengthFormatter, IsNSString string, IsNSString error_) => nsLengthFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsLengthFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLengthFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- numberFormatter@
numberFormatter :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsLengthFormatter  =
  sendMsg nsLengthFormatter (mkSelector "numberFormatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSLengthFormatter nsLengthFormatter, IsNSNumberFormatter value) => nsLengthFormatter -> value -> IO ()
setNumberFormatter nsLengthFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLengthFormatter (mkSelector "setNumberFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- unitStyle@
unitStyle :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO NSFormattingUnitStyle
unitStyle nsLengthFormatter  =
  fmap (coerce :: CLong -> NSFormattingUnitStyle) $ sendMsg nsLengthFormatter (mkSelector "unitStyle") retCLong []

-- | @- setUnitStyle:@
setUnitStyle :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsLengthFormatter  value =
  sendMsg nsLengthFormatter (mkSelector "setUnitStyle:") retVoid [argCLong (coerce value)]

-- | @- forPersonHeightUse@
forPersonHeightUse :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> IO Bool
forPersonHeightUse nsLengthFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLengthFormatter (mkSelector "forPersonHeightUse") retCULong []

-- | @- setForPersonHeightUse:@
setForPersonHeightUse :: IsNSLengthFormatter nsLengthFormatter => nsLengthFormatter -> Bool -> IO ()
setForPersonHeightUse nsLengthFormatter  value =
  sendMsg nsLengthFormatter (mkSelector "setForPersonHeightUse:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromMeters:@
stringFromMetersSelector :: Selector
stringFromMetersSelector = mkSelector "stringFromMeters:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromMeters:usedUnit:@
unitStringFromMeters_usedUnitSelector :: Selector
unitStringFromMeters_usedUnitSelector = mkSelector "unitStringFromMeters:usedUnit:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @numberFormatter@
numberFormatterSelector :: Selector
numberFormatterSelector = mkSelector "numberFormatter"

-- | @Selector@ for @setNumberFormatter:@
setNumberFormatterSelector :: Selector
setNumberFormatterSelector = mkSelector "setNumberFormatter:"

-- | @Selector@ for @unitStyle@
unitStyleSelector :: Selector
unitStyleSelector = mkSelector "unitStyle"

-- | @Selector@ for @setUnitStyle:@
setUnitStyleSelector :: Selector
setUnitStyleSelector = mkSelector "setUnitStyle:"

-- | @Selector@ for @forPersonHeightUse@
forPersonHeightUseSelector :: Selector
forPersonHeightUseSelector = mkSelector "forPersonHeightUse"

-- | @Selector@ for @setForPersonHeightUse:@
setForPersonHeightUseSelector :: Selector
setForPersonHeightUseSelector = mkSelector "setForPersonHeightUse:"

