{-# LANGUAGE DataKinds #-}
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
  , defaultValueSelector
  , maxValueSelector
  , minValueSelector
  , setCurrentValueSelector
  , stepSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
currentValue icScannerFeatureRange =
  sendMessage icScannerFeatureRange currentValueSelector

-- | currentValue
--
-- ￼The current value. Attempting to set the current value to a value that is not coincident with a step will result in a value corresponding to the nearest step being assigned to the current value.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> CDouble -> IO ()
setCurrentValue icScannerFeatureRange value =
  sendMessage icScannerFeatureRange setCurrentValueSelector value

-- | defaultValue
--
-- The default value￼. Attempting to set the default value to a value that is not coincident with a step will result in a value corresponding to the nearest step being assigned to the default value.
--
-- ObjC selector: @- defaultValue@
defaultValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
defaultValue icScannerFeatureRange =
  sendMessage icScannerFeatureRange defaultValueSelector

-- | minValue
--
-- The minimum value.
--
-- ObjC selector: @- minValue@
minValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
minValue icScannerFeatureRange =
  sendMessage icScannerFeatureRange minValueSelector

-- | maxValue
--
-- ￼The maximum value.
--
-- ObjC selector: @- maxValue@
maxValue :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
maxValue icScannerFeatureRange =
  sendMessage icScannerFeatureRange maxValueSelector

-- | stepSize
--
-- ￼The step size.
--
-- ObjC selector: @- stepSize@
stepSize :: IsICScannerFeatureRange icScannerFeatureRange => icScannerFeatureRange -> IO CDouble
stepSize icScannerFeatureRange =
  sendMessage icScannerFeatureRange stepSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] CDouble
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[CDouble] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector '[] CDouble
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @stepSize@
stepSizeSelector :: Selector '[] CDouble
stepSizeSelector = mkSelector "stepSize"

