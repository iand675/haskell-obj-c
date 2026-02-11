{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMassFormatter@.
module ObjC.Foundation.NSMassFormatter
  ( NSMassFormatter
  , IsNSMassFormatter(..)
  , stringFromValue_unit
  , stringFromKilograms
  , unitStringFromValue_unit
  , unitStringFromKilograms_usedUnit
  , getObjectValue_forString_errorDescription
  , numberFormatter
  , setNumberFormatter
  , unitStyle
  , setUnitStyle
  , forPersonMassUse
  , setForPersonMassUse
  , stringFromValue_unitSelector
  , stringFromKilogramsSelector
  , unitStringFromValue_unitSelector
  , unitStringFromKilograms_usedUnitSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setNumberFormatterSelector
  , unitStyleSelector
  , setUnitStyleSelector
  , forPersonMassUseSelector
  , setForPersonMassUseSelector

  -- * Enum types
  , NSFormattingUnitStyle(NSFormattingUnitStyle)
  , pattern NSFormattingUnitStyleShort
  , pattern NSFormattingUnitStyleMedium
  , pattern NSFormattingUnitStyleLong
  , NSMassFormatterUnit(NSMassFormatterUnit)
  , pattern NSMassFormatterUnitGram
  , pattern NSMassFormatterUnitKilogram
  , pattern NSMassFormatterUnitOunce
  , pattern NSMassFormatterUnitPound
  , pattern NSMassFormatterUnitStone

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
stringFromValue_unit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> NSMassFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsMassFormatter  value unit =
  sendMsg nsMassFormatter (mkSelector "stringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- stringFromKilograms:@
stringFromKilograms :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> IO (Id NSString)
stringFromKilograms nsMassFormatter  numberInKilograms =
  sendMsg nsMassFormatter (mkSelector "stringFromKilograms:") (retPtr retVoid) [argCDouble (fromIntegral numberInKilograms)] >>= retainedObject . castPtr

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> NSMassFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsMassFormatter  value unit =
  sendMsg nsMassFormatter (mkSelector "unitStringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- unitStringFromKilograms:usedUnit:@
unitStringFromKilograms_usedUnit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> Ptr NSMassFormatterUnit -> IO (Id NSString)
unitStringFromKilograms_usedUnit nsMassFormatter  numberInKilograms unitp =
  sendMsg nsMassFormatter (mkSelector "unitStringFromKilograms:usedUnit:") (retPtr retVoid) [argCDouble (fromIntegral numberInKilograms), argPtr unitp] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSMassFormatter nsMassFormatter, IsNSString string, IsNSString error_) => nsMassFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsMassFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMassFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- numberFormatter@
numberFormatter :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsMassFormatter  =
  sendMsg nsMassFormatter (mkSelector "numberFormatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSMassFormatter nsMassFormatter, IsNSNumberFormatter value) => nsMassFormatter -> value -> IO ()
setNumberFormatter nsMassFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMassFormatter (mkSelector "setNumberFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- unitStyle@
unitStyle :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO NSFormattingUnitStyle
unitStyle nsMassFormatter  =
  fmap (coerce :: CLong -> NSFormattingUnitStyle) $ sendMsg nsMassFormatter (mkSelector "unitStyle") retCLong []

-- | @- setUnitStyle:@
setUnitStyle :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsMassFormatter  value =
  sendMsg nsMassFormatter (mkSelector "setUnitStyle:") retVoid [argCLong (coerce value)]

-- | @- forPersonMassUse@
forPersonMassUse :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO Bool
forPersonMassUse nsMassFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMassFormatter (mkSelector "forPersonMassUse") retCULong []

-- | @- setForPersonMassUse:@
setForPersonMassUse :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> Bool -> IO ()
setForPersonMassUse nsMassFormatter  value =
  sendMsg nsMassFormatter (mkSelector "setForPersonMassUse:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromKilograms:@
stringFromKilogramsSelector :: Selector
stringFromKilogramsSelector = mkSelector "stringFromKilograms:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromKilograms:usedUnit:@
unitStringFromKilograms_usedUnitSelector :: Selector
unitStringFromKilograms_usedUnitSelector = mkSelector "unitStringFromKilograms:usedUnit:"

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

-- | @Selector@ for @forPersonMassUse@
forPersonMassUseSelector :: Selector
forPersonMassUseSelector = mkSelector "forPersonMassUse"

-- | @Selector@ for @setForPersonMassUse:@
setForPersonMassUseSelector :: Selector
setForPersonMassUseSelector = mkSelector "setForPersonMassUse:"

