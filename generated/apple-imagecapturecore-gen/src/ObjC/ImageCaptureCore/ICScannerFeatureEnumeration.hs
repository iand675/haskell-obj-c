{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFeatureEnumeration
--
-- ICScannerFeatureEnumeration object is used to represent a feature of a scanner functional unit that can have one of several discrete values.
--
-- Generated bindings for @ICScannerFeatureEnumeration@.
module ObjC.ImageCaptureCore.ICScannerFeatureEnumeration
  ( ICScannerFeatureEnumeration
  , IsICScannerFeatureEnumeration(..)
  , currentValue
  , setCurrentValue
  , defaultValue
  , values
  , menuItemLabels
  , menuItemLabelsTooltips
  , currentValueSelector
  , defaultValueSelector
  , menuItemLabelsSelector
  , menuItemLabelsTooltipsSelector
  , setCurrentValueSelector
  , valuesSelector


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
-- The current value. The current value can be set to one of the possible values in the "values" property below￼.
--
-- ObjC selector: @- currentValue@
currentValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO RawId
currentValue icScannerFeatureEnumeration =
  sendMessage icScannerFeatureEnumeration currentValueSelector

-- | currentValue
--
-- The current value. The current value can be set to one of the possible values in the "values" property below￼.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> RawId -> IO ()
setCurrentValue icScannerFeatureEnumeration value =
  sendMessage icScannerFeatureEnumeration setCurrentValueSelector value

-- | defaultValue
--
-- ￼The default value. The default value can be set to one of the possible values in the "values" property below.
--
-- ObjC selector: @- defaultValue@
defaultValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO RawId
defaultValue icScannerFeatureEnumeration =
  sendMessage icScannerFeatureEnumeration defaultValueSelector

-- | values
--
-- An array of possible values. All items in this array must be of same type￼.
--
-- ObjC selector: @- values@
values :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
values icScannerFeatureEnumeration =
  sendMessage icScannerFeatureEnumeration valuesSelector

-- | menuItemLabels
--
-- ￼The human readable menu item labels to be used in a menu to allow the user to select the current value from an array of possible values.
--
-- ObjC selector: @- menuItemLabels@
menuItemLabels :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
menuItemLabels icScannerFeatureEnumeration =
  sendMessage icScannerFeatureEnumeration menuItemLabelsSelector

-- | menuItemLabelsTooltips
--
-- ￼Tooltip text associated with the menu items.
--
-- ObjC selector: @- menuItemLabelsTooltips@
menuItemLabelsTooltips :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
menuItemLabelsTooltips icScannerFeatureEnumeration =
  sendMessage icScannerFeatureEnumeration menuItemLabelsTooltipsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] RawId
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[RawId] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector '[] RawId
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @values@
valuesSelector :: Selector '[] (Id NSArray)
valuesSelector = mkSelector "values"

-- | @Selector@ for @menuItemLabels@
menuItemLabelsSelector :: Selector '[] (Id NSArray)
menuItemLabelsSelector = mkSelector "menuItemLabels"

-- | @Selector@ for @menuItemLabelsTooltips@
menuItemLabelsTooltipsSelector :: Selector '[] (Id NSArray)
menuItemLabelsTooltipsSelector = mkSelector "menuItemLabelsTooltips"

