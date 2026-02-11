{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFeatureRange
--
-- ICScannerFeatureRange object is used to represent a property of a scanner functional unit whose value lies within a range.
--
-- Generated bindings for @ICScannerFeatureRange@.
module ObjC.ImageCaptureCore.ICScannerFeatureRange
  ( ICScannerFeatureRange
  , IsICScannerFeatureRange(..)
  , currentValue
  , setCurrentValue
  , defaultValue
  , minValue
  , maxValue
  , stepSize
  , currentValueSelector
  , setCurrentValueSelector
  , defaultValueSelector
  , minValueSelector
  , maxValueSelector
  , stepSizeSelector


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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | currentValue
--
-- ￼The current value. Attempting to set the current value to a value that is not coincident with a step will result in a value corresponding to the nearest step being assigned to the current value.
--
-- ObjC selector: @- currentValue@
currentValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
currentValue icScannerFeatureRange  =
  sendMsg icScannerFeatureRange (mkSelector "currentValue") retCDouble []

-- | currentValue
--
-- ￼The current value. Attempting to set the current value to a value that is not coincident with a step will result in a value corresponding to the nearest step being assigned to the current value.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> CDouble -> IO ()
setCurrentValue icScannerFeatureRange  value =
  sendMsg icScannerFeatureRange (mkSelector "setCurrentValue:") retVoid [argCDouble (fromIntegral value)]

-- | defaultValue
--
-- The default value￼. Attempting to set the default value to a value that is not coincident with a step will result in a value corresponding to the nearest step being assigned to the default value.
--
-- ObjC selector: @- defaultValue@
defaultValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
defaultValue icScannerFeatureRange  =
  sendMsg icScannerFeatureRange (mkSelector "defaultValue") retCDouble []

-- | minValue
--
-- The minimum value.
--
-- ObjC selector: @- minValue@
minValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
minValue icScannerFeatureRange  =
  sendMsg icScannerFeatureRange (mkSelector "minValue") retCDouble []

-- | maxValue
--
-- ￼The maximum value.
--
-- ObjC selector: @- maxValue@
maxValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
maxValue icScannerFeatureRange  =
  sendMsg icScannerFeatureRange (mkSelector "maxValue") retCDouble []

-- | stepSize
--
-- ￼The step size.
--
-- ObjC selector: @- stepSize@
stepSize :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
stepSize icScannerFeatureRange  =
  sendMsg icScannerFeatureRange (mkSelector "stepSize") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector
setCurrentValueSelector = mkSelector "setCurrentValue:"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @stepSize@
stepSizeSelector :: Selector
stepSizeSelector = mkSelector "stepSize"

