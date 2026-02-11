{-# LANGUAGE PatternSynonyms #-}
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
  , stringFromByteCount_countStyleSelector
  , stringFromByteCountSelector
  , stringFromMeasurement_countStyleSelector
  , stringFromMeasurementSelector
  , stringForObjectValueSelector
  , allowedUnitsSelector
  , setAllowedUnitsSelector
  , countStyleSelector
  , setCountStyleSelector
  , allowsNonnumericFormattingSelector
  , setAllowsNonnumericFormattingSelector
  , includesUnitSelector
  , setIncludesUnitSelector
  , includesCountSelector
  , setIncludesCountSelector
  , includesActualByteCountSelector
  , setIncludesActualByteCountSelector
  , adaptiveSelector
  , setAdaptiveSelector
  , zeroPadsFractionDigitsSelector
  , setZeroPadsFractionDigitsSelector
  , formattingContextSelector
  , setFormattingContextSelector

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

-- | @+ stringFromByteCount:countStyle:@
stringFromByteCount_countStyle :: CLong -> NSByteCountFormatterCountStyle -> IO (Id NSString)
stringFromByteCount_countStyle byteCount countStyle =
  do
    cls' <- getRequiredClass "NSByteCountFormatter"
    sendClassMsg cls' (mkSelector "stringFromByteCount:countStyle:") (retPtr retVoid) [argCLong (fromIntegral byteCount), argCLong (coerce countStyle)] >>= retainedObject . castPtr

-- | @- stringFromByteCount:@
stringFromByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> CLong -> IO (Id NSString)
stringFromByteCount nsByteCountFormatter  byteCount =
  sendMsg nsByteCountFormatter (mkSelector "stringFromByteCount:") (retPtr retVoid) [argCLong (fromIntegral byteCount)] >>= retainedObject . castPtr

-- | @+ stringFromMeasurement:countStyle:@
stringFromMeasurement_countStyle :: IsNSMeasurement measurement => measurement -> NSByteCountFormatterCountStyle -> IO (Id NSString)
stringFromMeasurement_countStyle measurement countStyle =
  do
    cls' <- getRequiredClass "NSByteCountFormatter"
    withObjCPtr measurement $ \raw_measurement ->
      sendClassMsg cls' (mkSelector "stringFromMeasurement:countStyle:") (retPtr retVoid) [argPtr (castPtr raw_measurement :: Ptr ()), argCLong (coerce countStyle)] >>= retainedObject . castPtr

-- | @- stringFromMeasurement:@
stringFromMeasurement :: (IsNSByteCountFormatter nsByteCountFormatter, IsNSMeasurement measurement) => nsByteCountFormatter -> measurement -> IO (Id NSString)
stringFromMeasurement nsByteCountFormatter  measurement =
withObjCPtr measurement $ \raw_measurement ->
    sendMsg nsByteCountFormatter (mkSelector "stringFromMeasurement:") (retPtr retVoid) [argPtr (castPtr raw_measurement :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsByteCountFormatter  obj_ =
  sendMsg nsByteCountFormatter (mkSelector "stringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- allowedUnits@
allowedUnits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSByteCountFormatterUnits
allowedUnits nsByteCountFormatter  =
  fmap (coerce :: CULong -> NSByteCountFormatterUnits) $ sendMsg nsByteCountFormatter (mkSelector "allowedUnits") retCULong []

-- | @- setAllowedUnits:@
setAllowedUnits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSByteCountFormatterUnits -> IO ()
setAllowedUnits nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setAllowedUnits:") retVoid [argCULong (coerce value)]

-- | @- countStyle@
countStyle :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSByteCountFormatterCountStyle
countStyle nsByteCountFormatter  =
  fmap (coerce :: CLong -> NSByteCountFormatterCountStyle) $ sendMsg nsByteCountFormatter (mkSelector "countStyle") retCLong []

-- | @- setCountStyle:@
setCountStyle :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSByteCountFormatterCountStyle -> IO ()
setCountStyle nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setCountStyle:") retVoid [argCLong (coerce value)]

-- | @- allowsNonnumericFormatting@
allowsNonnumericFormatting :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
allowsNonnumericFormatting nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "allowsNonnumericFormatting") retCULong []

-- | @- setAllowsNonnumericFormatting:@
setAllowsNonnumericFormatting :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setAllowsNonnumericFormatting nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setAllowsNonnumericFormatting:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesUnit@
includesUnit :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesUnit nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "includesUnit") retCULong []

-- | @- setIncludesUnit:@
setIncludesUnit :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesUnit nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setIncludesUnit:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesCount@
includesCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesCount nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "includesCount") retCULong []

-- | @- setIncludesCount:@
setIncludesCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesCount nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setIncludesCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesActualByteCount@
includesActualByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
includesActualByteCount nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "includesActualByteCount") retCULong []

-- | @- setIncludesActualByteCount:@
setIncludesActualByteCount :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setIncludesActualByteCount nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setIncludesActualByteCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- adaptive@
adaptive :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
adaptive nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "adaptive") retCULong []

-- | @- setAdaptive:@
setAdaptive :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setAdaptive nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setAdaptive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- zeroPadsFractionDigits@
zeroPadsFractionDigits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO Bool
zeroPadsFractionDigits nsByteCountFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsByteCountFormatter (mkSelector "zeroPadsFractionDigits") retCULong []

-- | @- setZeroPadsFractionDigits:@
setZeroPadsFractionDigits :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> Bool -> IO ()
setZeroPadsFractionDigits nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setZeroPadsFractionDigits:") retVoid [argCULong (if value then 1 else 0)]

-- | @- formattingContext@
formattingContext :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> IO NSFormattingContext
formattingContext nsByteCountFormatter  =
  fmap (coerce :: CLong -> NSFormattingContext) $ sendMsg nsByteCountFormatter (mkSelector "formattingContext") retCLong []

-- | @- setFormattingContext:@
setFormattingContext :: IsNSByteCountFormatter nsByteCountFormatter => nsByteCountFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsByteCountFormatter  value =
  sendMsg nsByteCountFormatter (mkSelector "setFormattingContext:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromByteCount:countStyle:@
stringFromByteCount_countStyleSelector :: Selector
stringFromByteCount_countStyleSelector = mkSelector "stringFromByteCount:countStyle:"

-- | @Selector@ for @stringFromByteCount:@
stringFromByteCountSelector :: Selector
stringFromByteCountSelector = mkSelector "stringFromByteCount:"

-- | @Selector@ for @stringFromMeasurement:countStyle:@
stringFromMeasurement_countStyleSelector :: Selector
stringFromMeasurement_countStyleSelector = mkSelector "stringFromMeasurement:countStyle:"

-- | @Selector@ for @stringFromMeasurement:@
stringFromMeasurementSelector :: Selector
stringFromMeasurementSelector = mkSelector "stringFromMeasurement:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @allowedUnits@
allowedUnitsSelector :: Selector
allowedUnitsSelector = mkSelector "allowedUnits"

-- | @Selector@ for @setAllowedUnits:@
setAllowedUnitsSelector :: Selector
setAllowedUnitsSelector = mkSelector "setAllowedUnits:"

-- | @Selector@ for @countStyle@
countStyleSelector :: Selector
countStyleSelector = mkSelector "countStyle"

-- | @Selector@ for @setCountStyle:@
setCountStyleSelector :: Selector
setCountStyleSelector = mkSelector "setCountStyle:"

-- | @Selector@ for @allowsNonnumericFormatting@
allowsNonnumericFormattingSelector :: Selector
allowsNonnumericFormattingSelector = mkSelector "allowsNonnumericFormatting"

-- | @Selector@ for @setAllowsNonnumericFormatting:@
setAllowsNonnumericFormattingSelector :: Selector
setAllowsNonnumericFormattingSelector = mkSelector "setAllowsNonnumericFormatting:"

-- | @Selector@ for @includesUnit@
includesUnitSelector :: Selector
includesUnitSelector = mkSelector "includesUnit"

-- | @Selector@ for @setIncludesUnit:@
setIncludesUnitSelector :: Selector
setIncludesUnitSelector = mkSelector "setIncludesUnit:"

-- | @Selector@ for @includesCount@
includesCountSelector :: Selector
includesCountSelector = mkSelector "includesCount"

-- | @Selector@ for @setIncludesCount:@
setIncludesCountSelector :: Selector
setIncludesCountSelector = mkSelector "setIncludesCount:"

-- | @Selector@ for @includesActualByteCount@
includesActualByteCountSelector :: Selector
includesActualByteCountSelector = mkSelector "includesActualByteCount"

-- | @Selector@ for @setIncludesActualByteCount:@
setIncludesActualByteCountSelector :: Selector
setIncludesActualByteCountSelector = mkSelector "setIncludesActualByteCount:"

-- | @Selector@ for @adaptive@
adaptiveSelector :: Selector
adaptiveSelector = mkSelector "adaptive"

-- | @Selector@ for @setAdaptive:@
setAdaptiveSelector :: Selector
setAdaptiveSelector = mkSelector "setAdaptive:"

-- | @Selector@ for @zeroPadsFractionDigits@
zeroPadsFractionDigitsSelector :: Selector
zeroPadsFractionDigitsSelector = mkSelector "zeroPadsFractionDigits"

-- | @Selector@ for @setZeroPadsFractionDigits:@
setZeroPadsFractionDigitsSelector :: Selector
setZeroPadsFractionDigitsSelector = mkSelector "setZeroPadsFractionDigits:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector
setFormattingContextSelector = mkSelector "setFormattingContext:"

