{-# LANGUAGE PatternSynonyms #-}
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
  , stringFromValue_unitSelector
  , stringFromJoulesSelector
  , unitStringFromValue_unitSelector
  , unitStringFromJoules_usedUnitSelector
  , getObjectValue_forString_errorDescriptionSelector
  , numberFormatterSelector
  , setNumberFormatterSelector
  , unitStyleSelector
  , setUnitStyleSelector
  , forFoodEnergyUseSelector
  , setForFoodEnergyUseSelector

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
stringFromValue_unit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> NSEnergyFormatterUnit -> IO (Id NSString)
stringFromValue_unit nsEnergyFormatter  value unit =
  sendMsg nsEnergyFormatter (mkSelector "stringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- stringFromJoules:@
stringFromJoules :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> IO (Id NSString)
stringFromJoules nsEnergyFormatter  numberInJoules =
  sendMsg nsEnergyFormatter (mkSelector "stringFromJoules:") (retPtr retVoid) [argCDouble (fromIntegral numberInJoules)] >>= retainedObject . castPtr

-- | @- unitStringFromValue:unit:@
unitStringFromValue_unit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> NSEnergyFormatterUnit -> IO (Id NSString)
unitStringFromValue_unit nsEnergyFormatter  value unit =
  sendMsg nsEnergyFormatter (mkSelector "unitStringFromValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral value), argCLong (coerce unit)] >>= retainedObject . castPtr

-- | @- unitStringFromJoules:usedUnit:@
unitStringFromJoules_usedUnit :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> CDouble -> Ptr NSEnergyFormatterUnit -> IO (Id NSString)
unitStringFromJoules_usedUnit nsEnergyFormatter  numberInJoules unitp =
  sendMsg nsEnergyFormatter (mkSelector "unitStringFromJoules:usedUnit:") (retPtr retVoid) [argCDouble (fromIntegral numberInJoules), argPtr unitp] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSEnergyFormatter nsEnergyFormatter, IsNSString string, IsNSString error_) => nsEnergyFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsEnergyFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEnergyFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- numberFormatter@
numberFormatter :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO (Id NSNumberFormatter)
numberFormatter nsEnergyFormatter  =
  sendMsg nsEnergyFormatter (mkSelector "numberFormatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberFormatter:@
setNumberFormatter :: (IsNSEnergyFormatter nsEnergyFormatter, IsNSNumberFormatter value) => nsEnergyFormatter -> value -> IO ()
setNumberFormatter nsEnergyFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEnergyFormatter (mkSelector "setNumberFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- unitStyle@
unitStyle :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO NSFormattingUnitStyle
unitStyle nsEnergyFormatter  =
  fmap (coerce :: CLong -> NSFormattingUnitStyle) $ sendMsg nsEnergyFormatter (mkSelector "unitStyle") retCLong []

-- | @- setUnitStyle:@
setUnitStyle :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> NSFormattingUnitStyle -> IO ()
setUnitStyle nsEnergyFormatter  value =
  sendMsg nsEnergyFormatter (mkSelector "setUnitStyle:") retVoid [argCLong (coerce value)]

-- | @- forFoodEnergyUse@
forFoodEnergyUse :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> IO Bool
forFoodEnergyUse nsEnergyFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEnergyFormatter (mkSelector "forFoodEnergyUse") retCULong []

-- | @- setForFoodEnergyUse:@
setForFoodEnergyUse :: IsNSEnergyFormatter nsEnergyFormatter => nsEnergyFormatter -> Bool -> IO ()
setForFoodEnergyUse nsEnergyFormatter  value =
  sendMsg nsEnergyFormatter (mkSelector "setForFoodEnergyUse:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromValue:unit:@
stringFromValue_unitSelector :: Selector
stringFromValue_unitSelector = mkSelector "stringFromValue:unit:"

-- | @Selector@ for @stringFromJoules:@
stringFromJoulesSelector :: Selector
stringFromJoulesSelector = mkSelector "stringFromJoules:"

-- | @Selector@ for @unitStringFromValue:unit:@
unitStringFromValue_unitSelector :: Selector
unitStringFromValue_unitSelector = mkSelector "unitStringFromValue:unit:"

-- | @Selector@ for @unitStringFromJoules:usedUnit:@
unitStringFromJoules_usedUnitSelector :: Selector
unitStringFromJoules_usedUnitSelector = mkSelector "unitStringFromJoules:usedUnit:"

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

-- | @Selector@ for @forFoodEnergyUse@
forFoodEnergyUseSelector :: Selector
forFoodEnergyUseSelector = mkSelector "forFoodEnergyUse"

-- | @Selector@ for @setForFoodEnergyUse:@
setForFoodEnergyUseSelector :: Selector
setForFoodEnergyUseSelector = mkSelector "setForFoodEnergyUse:"

