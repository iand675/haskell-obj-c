{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEnergyFormatter@.
module ObjC.Foundation.NSEnergyFormatter
  ( NSEnergyFormatter
  , IsNSEnergyFormatter(..)
  , stringFromValue_unit
  , stringFromJoules
  , unitStringFromValue_unit
  , unitStringFromJoules_usedUnit
  , getObjectValue_forString_errorDescription
  , numberFormatter
  , setNumberFormatter
  , unitStyle
  , setUnitStyle
  , forFoodEnergyUse
  , setForFoodEnergyUse
  , forFoodEnergyUseSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setForFoodEnergyUseSelector
  , setNumberFormatterSelector
  , setUnitStyleSelector
  , stringFromJoulesSelector
  , stringFromValue_unitSelector
  , unitStringFromJoules_usedUnitSelector
  , unitStringFromValue_unitSelector
  , unitStyleSelector

  -- * Enum types
  , NSEnergyFormatterUnit(NSEnergyFormatterUnit)
  , pattern NSEnergyFormatterUnitJoule
  , pattern NSEnergyFormatterUnitKilojoule
  , pattern NSEnergyFormatterUnitCalorie
  , pattern NSEnergyFormatterUnitKilocalorie
  , NSFormattingUnitStyle(NSFormattingUnitStyle)
  , pattern NSFormattingUnitStyleShort
  , pattern NSFormattingUnitStyleMedium
  , pattern NSFormattingUnitStyleLong

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
stringFromValue_unit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> NSEnergyFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsEnergyFormatter value unit =
  sendMessage nsEnergyFormatter stringFromValue_unitSelector value unit

-- | @- stringFromJoules:@
stringFromJoules :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> IO (Id NSString)
stringFromJoules nsEnergyFormatter numberInJoules =
  sendMessage nsEnergyFormatter stringFromJoulesSelector numberInJoules

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> NSEnergyFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsEnergyFormatter value unit =
  sendMessage nsEnergyFormatter unitStringFromValue_unitSelector value unit

-- | @- unitStringFromJoules:usedUnit:@
unitStringFromJoules_usedUnit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> Ptr NSEnergyFormatterUnit -> IO (Id NSString)
unitStringFromJoules_usedUnit nsEnergyFormatter numberInJoules unitp =
  sendMessage nsEnergyFormatter unitStringFromJoules_usedUnitSelector numberInJoules unitp

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSEnergyFormatter nsEnergyFormatter, IsNSString string, IsNSString error_) => nsEnergyFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsEnergyFormatter obj_ string error_ =
  sendMessage nsEnergyFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- numberFormatter@
numberFormatter :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsEnergyFormatter =
  sendMessage nsEnergyFormatter numberFormatterSelector

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSEnergyFormatter nsEnergyFormatter, IsNSNumberFormatter value) => nsEnergyFormatter -> value -> IO ()
setNumberFormatter nsEnergyFormatter value =
  sendMessage nsEnergyFormatter setNumberFormatterSelector (toNSNumberFormatter value)

-- | @- unitStyle@
unitStyle :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO NSFormattingUnitStyle
unitStyle nsEnergyFormatter =
  sendMessage nsEnergyFormatter unitStyleSelector

-- | @- setUnitStyle:@
setUnitStyle :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsEnergyFormatter value =
  sendMessage nsEnergyFormatter setUnitStyleSelector value

-- | @- forFoodEnergyUse@
forFoodEnergyUse :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO Bool
forFoodEnergyUse nsEnergyFormatter =
  sendMessage nsEnergyFormatter forFoodEnergyUseSelector

-- | @- setForFoodEnergyUse:@
setForFoodEnergyUse :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> Bool -> IO ()
setForFoodEnergyUse nsEnergyFormatter value =
  sendMessage nsEnergyFormatter setForFoodEnergyUseSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector '[CDouble, NSEnergyFormatterUnit] (Id NSString)
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromJoules:@
stringFromJoulesSelector :: Selector '[CDouble] (Id NSString)
stringFromJoulesSelector = mkSelector "stringFromJoules:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector '[CDouble, NSEnergyFormatterUnit] (Id NSString)
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromJoules:usedUnit:@
unitStringFromJoules_usedUnitSelector :: Selector '[CDouble, Ptr NSEnergyFormatterUnit] (Id NSString)
unitStringFromJoules_usedUnitSelector = mkSelector "unitStringFromJoules:usedUnit:"

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

-- | @Selector@ for @forFoodEnergyUse@
forFoodEnergyUseSelector :: Selector '[] Bool
forFoodEnergyUseSelector = mkSelector "forFoodEnergyUse"

-- | @Selector@ for @setForFoodEnergyUse:@
setForFoodEnergyUseSelector :: Selector '[Bool] ()
setForFoodEnergyUseSelector = mkSelector "setForFoodEnergyUse:"

