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
  , setCurrentValueSelector
  , defaultValueSelector
  , valuesSelector
  , menuItemLabelsSelector
  , menuItemLabelsTooltipsSelector


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
-- The current value. The current value can be set to one of the possible values in the "values" property below￼.
--
-- ObjC selector: @- currentValue@
currentValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO RawId
currentValue icScannerFeatureEnumeration  =
  fmap (RawId . castPtr) $ sendMsg icScannerFeatureEnumeration (mkSelector "currentValue") (retPtr retVoid) []

-- | currentValue
--
-- The current value. The current value can be set to one of the possible values in the "values" property below￼.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> RawId -> IO ()
setCurrentValue icScannerFeatureEnumeration  value =
  sendMsg icScannerFeatureEnumeration (mkSelector "setCurrentValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | defaultValue
--
-- ￼The default value. The default value can be set to one of the possible values in the "values" property below.
--
-- ObjC selector: @- defaultValue@
defaultValue :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO RawId
defaultValue icScannerFeatureEnumeration  =
  fmap (RawId . castPtr) $ sendMsg icScannerFeatureEnumeration (mkSelector "defaultValue") (retPtr retVoid) []

-- | values
--
-- An array of possible values. All items in this array must be of same type￼.
--
-- ObjC selector: @- values@
values :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
values icScannerFeatureEnumeration  =
  sendMsg icScannerFeatureEnumeration (mkSelector "values") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | menuItemLabels
--
-- ￼The human readable menu item labels to be used in a menu to allow the user to select the current value from an array of possible values.
--
-- ObjC selector: @- menuItemLabels@
menuItemLabels :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
menuItemLabels icScannerFeatureEnumeration  =
  sendMsg icScannerFeatureEnumeration (mkSelector "menuItemLabels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | menuItemLabelsTooltips
--
-- ￼Tooltip text associated with the menu items.
--
-- ObjC selector: @- menuItemLabelsTooltips@
menuItemLabelsTooltips :: IsICScannerFeatureEnumeration icScannerFeatureEnumeration => icScannerFeatureEnumeration -> IO (Id NSArray)
menuItemLabelsTooltips icScannerFeatureEnumeration  =
  sendMsg icScannerFeatureEnumeration (mkSelector "menuItemLabelsTooltips") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @values@
valuesSelector :: Selector
valuesSelector = mkSelector "values"

-- | @Selector@ for @menuItemLabels@
menuItemLabelsSelector :: Selector
menuItemLabelsSelector = mkSelector "menuItemLabels"

-- | @Selector@ for @menuItemLabelsTooltips@
menuItemLabelsTooltipsSelector :: Selector
menuItemLabelsTooltipsSelector = mkSelector "menuItemLabelsTooltips"

