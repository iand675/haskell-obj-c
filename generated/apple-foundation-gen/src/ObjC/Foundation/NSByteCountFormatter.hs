{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSByteCountFormatter@.
module ObjC.Foundation.NSByteCountFormatter
  ( NSByteCountFormatter
  , IsNSByteCountFormatter(..)
  , stringFromByteCount_countStyle
  , stringFromByteCount
  , stringFromMeasurement_countStyle
  , stringFromMeasurement
  , stringForObjectValue
  , allowedUnits
  , setAllowedUnits
  , countStyle
  , setCountStyle
  , allowsNonnumericFormatting
  , setAllowsNonnumericFormatting
  , includesUnit
  , setIncludesUnit
  , includesCount
  , setIncludesCount
  , includesActualByteCount
  , setIncludesActualByteCount
  , adaptive
  , setAdaptive
  , zeroPadsFractionDigits
  , setZeroPadsFractionDigits
  , formattingContext
  , setFormattingContext
  , adaptiveSelector
  , allowedUnitsSelector
  , allowsNonnumericFormattingSelector
  , countStyleSelector
  , formattingContextSelector
  , includesActualByteCountSelector
  , includesCountSelector
  , includesUnitSelector
  , setAdaptiveSelector
  , setAllowedUnitsSelector
  , setAllowsNonnumericFormattingSelector
  , setCountStyleSelector
  , setFormattingContextSelector
  , setIncludesActualByteCountSelector
  , setIncludesCountSelector
  , setIncludesUnitSelector
  , setZeroPadsFractionDigitsSelector
  , stringForObjectValueSelector
  , stringFromByteCountSelector
  , stringFromByteCount_countStyleSelector
  , stringFromMeasurementSelector
  , stringFromMeasurement_countStyleSelector
  , zeroPadsFractionDigitsSelector

  -- * Enum types
  , NSByteCountFormatterCountStyle(NSByteCountFormatterCountStyle)
  , pattern NSByteCountFormatterCountStyleFile
  , pattern NSByteCountFormatterCountStyleMemory
  , pattern NSByteCountFormatterCountStyleDecimal
  , pattern NSByteCountFormatterCountStyleBinary
  , NSByteCountFormatterUnits(NSByteCountFormatterUnits)
  , pattern NSByteCountFormatterUseDefault
  , pattern NSByteCountFormatterUseBytes
  , pattern NSByteCountFormatterUseKB
  , pattern NSByteCountFormatterUseMB
  , pattern NSByteCountFormatterUseGB
  , pattern NSByteCountFormatterUseTB
  , pattern NSByteCountFormatterUsePB
  , pattern NSByteCountFormatterUseEB
  , pattern NSByteCountFormatterUseZB
  , pattern NSByteCountFormatterUseYBOrHigher
  , pattern NSByteCountFormatterUseAll
  , NSFormattingContext(NSFormattingContext)
  , pattern NSFormattingContextUnknown
  , pattern NSFormattingContextDynamic
  , pattern NSFormattingContextStandalone
  , pattern NSFormattingContextListItem
  , pattern NSFormattingContextBeginningOfSentence
  , pattern NSFormattingContextMiddleOfSentence

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ stringFromByteCount:countStyle:@
stringFromByteCount_countStyle :: CLong -> NSByteCountFormatterCountStyle -> IO (Id NSString)
stringFromByteCount_countStyle byteCount countStyle =
  do
    cls' <- getRequiredClass "NSByteCountFormatter"
    sendClassMessage cls' stringFromByteCount_countStyleSelector byteCount countStyle

-- | @- stringFromByteCount:@
stringFromByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> CLong -> IO (Id NSString)
stringFromByteCount nsByteCountFormatter byteCount =
  sendMessage nsByteCountFormatter stringFromByteCountSelector byteCount

-- | @+ stringFromMeasurement:countStyle:@
stringFromMeasurement_countStyle :: IsNSMeasurement measurement => measurement -> NSByteCountFormatterCountStyle -> IO (Id NSString)
stringFromMeasurement_countStyle measurement countStyle =
  do
    cls' <- getRequiredClass "NSByteCountFormatter"
    sendClassMessage cls' stringFromMeasurement_countStyleSelector (toNSMeasurement measurement) countStyle

-- | @- stringFromMeasurement:@
stringFromMeasurement :: (IsNSByteCountFormatter nsByteCountFormatter, IsNSMeasurement measurement) => nsByteCountFormatter -> measurement -> IO (Id NSString)
stringFromMeasurement nsByteCountFormatter measurement =
  sendMessage nsByteCountFormatter stringFromMeasurementSelector (toNSMeasurement measurement)

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsByteCountFormatter obj_ =
  sendMessage nsByteCountFormatter stringForObjectValueSelector obj_

-- | @- allowedUnits@
allowedUnits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSByteCountFormatterUnits
allowedUnits nsByteCountFormatter =
  sendMessage nsByteCountFormatter allowedUnitsSelector

-- | @- setAllowedUnits:@
setAllowedUnits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSByteCountFormatterUnits -> IO ()
setAllowedUnits nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setAllowedUnitsSelector value

-- | @- countStyle@
countStyle :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSByteCountFormatterCountStyle
countStyle nsByteCountFormatter =
  sendMessage nsByteCountFormatter countStyleSelector

-- | @- setCountStyle:@
setCountStyle :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSByteCountFormatterCountStyle -> IO ()
setCountStyle nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setCountStyleSelector value

-- | @- allowsNonnumericFormatting@
allowsNonnumericFormatting :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
allowsNonnumericFormatting nsByteCountFormatter =
  sendMessage nsByteCountFormatter allowsNonnumericFormattingSelector

-- | @- setAllowsNonnumericFormatting:@
setAllowsNonnumericFormatting :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setAllowsNonnumericFormatting nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setAllowsNonnumericFormattingSelector value

-- | @- includesUnit@
includesUnit :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesUnit nsByteCountFormatter =
  sendMessage nsByteCountFormatter includesUnitSelector

-- | @- setIncludesUnit:@
setIncludesUnit :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesUnit nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setIncludesUnitSelector value

-- | @- includesCount@
includesCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesCount nsByteCountFormatter =
  sendMessage nsByteCountFormatter includesCountSelector

-- | @- setIncludesCount:@
setIncludesCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesCount nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setIncludesCountSelector value

-- | @- includesActualByteCount@
includesActualByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesActualByteCount nsByteCountFormatter =
  sendMessage nsByteCountFormatter includesActualByteCountSelector

-- | @- setIncludesActualByteCount:@
setIncludesActualByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesActualByteCount nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setIncludesActualByteCountSelector value

-- | @- adaptive@
adaptive :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
adaptive nsByteCountFormatter =
  sendMessage nsByteCountFormatter adaptiveSelector

-- | @- setAdaptive:@
setAdaptive :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setAdaptive nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setAdaptiveSelector value

-- | @- zeroPadsFractionDigits@
zeroPadsFractionDigits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
zeroPadsFractionDigits nsByteCountFormatter =
  sendMessage nsByteCountFormatter zeroPadsFractionDigitsSelector

-- | @- setZeroPadsFractionDigits:@
setZeroPadsFractionDigits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setZeroPadsFractionDigits nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setZeroPadsFractionDigitsSelector value

-- | @- formattingContext@
formattingContext :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSFormattingContext
formattingContext nsByteCountFormatter =
  sendMessage nsByteCountFormatter formattingContextSelector

-- | @- setFormattingContext:@
setFormattingContext :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsByteCountFormatter value =
  sendMessage nsByteCountFormatter setFormattingContextSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromByteCount:countStyle:@
stringFromByteCount_countStyleSelector :: Selector '[CLong, NSByteCountFormatterCountStyle] (Id NSString)
stringFromByteCount_countStyleSelector = mkSelector "stringFromByteCount:countStyle:"

-- | @Selector@ for @stringFromByteCount:@
stringFromByteCountSelector :: Selector '[CLong] (Id NSString)
stringFromByteCountSelector = mkSelector "stringFromByteCount:"

-- | @Selector@ for @stringFromMeasurement:countStyle:@
stringFromMeasurement_countStyleSelector :: Selector '[Id NSMeasurement, NSByteCountFormatterCountStyle] (Id NSString)
stringFromMeasurement_countStyleSelector = mkSelector "stringFromMeasurement:countStyle:"

-- | @Selector@ for @stringFromMeasurement:@
stringFromMeasurementSelector :: Selector '[Id NSMeasurement] (Id NSString)
stringFromMeasurementSelector = mkSelector "stringFromMeasurement:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector '[RawId] (Id NSString)
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @allowedUnits@
allowedUnitsSelector :: Selector '[] NSByteCountFormatterUnits
allowedUnitsSelector = mkSelector "allowedUnits"

-- | @Selector@ for @setAllowedUnits:@
setAllowedUnitsSelector :: Selector '[NSByteCountFormatterUnits] ()
setAllowedUnitsSelector = mkSelector "setAllowedUnits:"

-- | @Selector@ for @countStyle@
countStyleSelector :: Selector '[] NSByteCountFormatterCountStyle
countStyleSelector = mkSelector "countStyle"

-- | @Selector@ for @setCountStyle:@
setCountStyleSelector :: Selector '[NSByteCountFormatterCountStyle] ()
setCountStyleSelector = mkSelector "setCountStyle:"

-- | @Selector@ for @allowsNonnumericFormatting@
allowsNonnumericFormattingSelector :: Selector '[] Bool
allowsNonnumericFormattingSelector = mkSelector "allowsNonnumericFormatting"

-- | @Selector@ for @setAllowsNonnumericFormatting:@
setAllowsNonnumericFormattingSelector :: Selector '[Bool] ()
setAllowsNonnumericFormattingSelector = mkSelector "setAllowsNonnumericFormatting:"

-- | @Selector@ for @includesUnit@
includesUnitSelector :: Selector '[] Bool
includesUnitSelector = mkSelector "includesUnit"

-- | @Selector@ for @setIncludesUnit:@
setIncludesUnitSelector :: Selector '[Bool] ()
setIncludesUnitSelector = mkSelector "setIncludesUnit:"

-- | @Selector@ for @includesCount@
includesCountSelector :: Selector '[] Bool
includesCountSelector = mkSelector "includesCount"

-- | @Selector@ for @setIncludesCount:@
setIncludesCountSelector :: Selector '[Bool] ()
setIncludesCountSelector = mkSelector "setIncludesCount:"

-- | @Selector@ for @includesActualByteCount@
includesActualByteCountSelector :: Selector '[] Bool
includesActualByteCountSelector = mkSelector "includesActualByteCount"

-- | @Selector@ for @setIncludesActualByteCount:@
setIncludesActualByteCountSelector :: Selector '[Bool] ()
setIncludesActualByteCountSelector = mkSelector "setIncludesActualByteCount:"

-- | @Selector@ for @adaptive@
adaptiveSelector :: Selector '[] Bool
adaptiveSelector = mkSelector "adaptive"

-- | @Selector@ for @setAdaptive:@
setAdaptiveSelector :: Selector '[Bool] ()
setAdaptiveSelector = mkSelector "setAdaptive:"

-- | @Selector@ for @zeroPadsFractionDigits@
zeroPadsFractionDigitsSelector :: Selector '[] Bool
zeroPadsFractionDigitsSelector = mkSelector "zeroPadsFractionDigits"

-- | @Selector@ for @setZeroPadsFractionDigits:@
setZeroPadsFractionDigitsSelector :: Selector '[Bool] ()
setZeroPadsFractionDigitsSelector = mkSelector "setZeroPadsFractionDigits:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector '[] NSFormattingContext
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector '[NSFormattingContext] ()
setFormattingContextSelector = mkSelector "setFormattingContext:"

