{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , forPersonMassUseSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setForPersonMassUseSelector
  , setNumberFormatterSelector
  , setUnitStyleSelector
  , stringFromKilogramsSelector
  , stringFromValue_unitSelector
  , unitStringFromKilograms_usedUnitSelector
  , unitStringFromValue_unitSelector
  , unitStyleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringFromValue:unit:@
stringFromValue_unit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> NSMassFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsMassFormatter value unit =
  sendMessage nsMassFormatter stringFromValue_unitSelector value unit

-- | @- stringFromKilograms:@
stringFromKilograms :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> IO (Id NSString)
stringFromKilograms nsMassFormatter numberInKilograms =
  sendMessage nsMassFormatter stringFromKilogramsSelector numberInKilograms

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> NSMassFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsMassFormatter value unit =
  sendMessage nsMassFormatter unitStringFromValue_unitSelector value unit

-- | @- unitStringFromKilograms:usedUnit:@
unitStringFromKilograms_usedUnit :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> CDouble -> Ptr NSMassFormatterUnit -> IO (Id NSString)
unitStringFromKilograms_usedUnit nsMassFormatter numberInKilograms unitp =
  sendMessage nsMassFormatter unitStringFromKilograms_usedUnitSelector numberInKilograms unitp

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSMassFormatter nsMassFormatter, IsNSString string, IsNSString error_) => nsMassFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsMassFormatter obj_ string error_ =
  sendMessage nsMassFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- numberFormatter@
numberFormatter :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsMassFormatter =
  sendMessage nsMassFormatter numberFormatterSelector

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSMassFormatter nsMassFormatter, IsNSNumberFormatter value) => nsMassFormatter -> value -> IO ()
setNumberFormatter nsMassFormatter value =
  sendMessage nsMassFormatter setNumberFormatterSelector (toNSNumberFormatter value)

-- | @- unitStyle@
unitStyle :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO NSFormattingUnitStyle
unitStyle nsMassFormatter =
  sendMessage nsMassFormatter unitStyleSelector

-- | @- setUnitStyle:@
setUnitStyle :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsMassFormatter value =
  sendMessage nsMassFormatter setUnitStyleSelector value

-- | @- forPersonMassUse@
forPersonMassUse :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> IO Bool
forPersonMassUse nsMassFormatter =
  sendMessage nsMassFormatter forPersonMassUseSelector

-- | @- setForPersonMassUse:@
setForPersonMassUse :: IsNSMassFormatter nsMassFormatter => nsMassFormatter -> Bool -> IO ()
setForPersonMassUse nsMassFormatter value =
  sendMessage nsMassFormatter setForPersonMassUseSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector '[CDouble, NSMassFormatterUnit] (Id NSString)
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromKilograms:@
stringFromKilogramsSelector :: Selector '[CDouble] (Id NSString)
stringFromKilogramsSelector = mkSelector "stringFromKilograms:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector '[CDouble, NSMassFormatterUnit] (Id NSString)
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromKilograms:usedUnit:@
unitStringFromKilograms_usedUnitSelector :: Selector '[CDouble, Ptr NSMassFormatterUnit] (Id NSString)
unitStringFromKilograms_usedUnitSelector = mkSelector "unitStringFromKilograms:usedUnit:"

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

-- | @Selector@ for @forPersonMassUse@
forPersonMassUseSelector :: Selector '[] Bool
forPersonMassUseSelector = mkSelector "forPersonMassUse"

-- | @Selector@ for @setForPersonMassUse:@
setForPersonMassUseSelector :: Selector '[Bool] ()
setForPersonMassUseSelector = mkSelector "setForPersonMassUse:"

