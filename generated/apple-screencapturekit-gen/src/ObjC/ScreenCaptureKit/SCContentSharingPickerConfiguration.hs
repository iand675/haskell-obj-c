{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCContentSharingPickerConfiguration
--
-- SCContentSharingPickerConfiguration is an object which can optionally be set on the SCContentSharingPicker for customized configuration.
--
-- Generated bindings for @SCContentSharingPickerConfiguration@.
module ObjC.ScreenCaptureKit.SCContentSharingPickerConfiguration
  ( SCContentSharingPickerConfiguration
  , IsSCContentSharingPickerConfiguration(..)
  , allowedPickerModes
  , setAllowedPickerModes
  , excludedWindowIDs
  , setExcludedWindowIDs
  , excludedBundleIDs
  , setExcludedBundleIDs
  , allowsChangingSelectedContent
  , setAllowsChangingSelectedContent
  , allowedPickerModesSelector
  , allowsChangingSelectedContentSelector
  , excludedBundleIDsSelector
  , excludedWindowIDsSelector
  , setAllowedPickerModesSelector
  , setAllowsChangingSelectedContentSelector
  , setExcludedBundleIDsSelector
  , setExcludedWindowIDsSelector

  -- * Enum types
  , SCContentSharingPickerMode(SCContentSharingPickerMode)
  , pattern SCContentSharingPickerModeSingleWindow
  , pattern SCContentSharingPickerModeMultipleWindows
  , pattern SCContentSharingPickerModeSingleApplication
  , pattern SCContentSharingPickerModeMultipleApplications
  , pattern SCContentSharingPickerModeSingleDisplay

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | allowedPickerModes Limits the type of selections available to the user when the picker is presented. Default is 0, no excluded picking modes
--
-- ObjC selector: @- allowedPickerModes@
allowedPickerModes :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO SCContentSharingPickerMode
allowedPickerModes scContentSharingPickerConfiguration =
  sendMessage scContentSharingPickerConfiguration allowedPickerModesSelector

-- | allowedPickerModes Limits the type of selections available to the user when the picker is presented. Default is 0, no excluded picking modes
--
-- ObjC selector: @- setAllowedPickerModes:@
setAllowedPickerModes :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> SCContentSharingPickerMode -> IO ()
setAllowedPickerModes scContentSharingPickerConfiguration value =
  sendMessage scContentSharingPickerConfiguration setAllowedPickerModesSelector value

-- | excludedWindowIDs Excludes CGWindowIDs for picking
--
-- ObjC selector: @- excludedWindowIDs@
excludedWindowIDs :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO (Id NSArray)
excludedWindowIDs scContentSharingPickerConfiguration =
  sendMessage scContentSharingPickerConfiguration excludedWindowIDsSelector

-- | excludedWindowIDs Excludes CGWindowIDs for picking
--
-- ObjC selector: @- setExcludedWindowIDs:@
setExcludedWindowIDs :: (IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration, IsNSArray value) => scContentSharingPickerConfiguration -> value -> IO ()
setExcludedWindowIDs scContentSharingPickerConfiguration value =
  sendMessage scContentSharingPickerConfiguration setExcludedWindowIDsSelector (toNSArray value)

-- | excludedBundleIDs Excludes bundle IDs for picking
--
-- ObjC selector: @- excludedBundleIDs@
excludedBundleIDs :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO (Id NSArray)
excludedBundleIDs scContentSharingPickerConfiguration =
  sendMessage scContentSharingPickerConfiguration excludedBundleIDsSelector

-- | excludedBundleIDs Excludes bundle IDs for picking
--
-- ObjC selector: @- setExcludedBundleIDs:@
setExcludedBundleIDs :: (IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration, IsNSArray value) => scContentSharingPickerConfiguration -> value -> IO ()
setExcludedBundleIDs scContentSharingPickerConfiguration value =
  sendMessage scContentSharingPickerConfiguration setExcludedBundleIDsSelector (toNSArray value)

-- | allowsChangingSelectedContent Controls if the user can make updates to the content filter after the initial selection. Defaults is YES.
--
-- ObjC selector: @- allowsChangingSelectedContent@
allowsChangingSelectedContent :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO Bool
allowsChangingSelectedContent scContentSharingPickerConfiguration =
  sendMessage scContentSharingPickerConfiguration allowsChangingSelectedContentSelector

-- | allowsChangingSelectedContent Controls if the user can make updates to the content filter after the initial selection. Defaults is YES.
--
-- ObjC selector: @- setAllowsChangingSelectedContent:@
setAllowsChangingSelectedContent :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> Bool -> IO ()
setAllowsChangingSelectedContent scContentSharingPickerConfiguration value =
  sendMessage scContentSharingPickerConfiguration setAllowsChangingSelectedContentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowedPickerModes@
allowedPickerModesSelector :: Selector '[] SCContentSharingPickerMode
allowedPickerModesSelector = mkSelector "allowedPickerModes"

-- | @Selector@ for @setAllowedPickerModes:@
setAllowedPickerModesSelector :: Selector '[SCContentSharingPickerMode] ()
setAllowedPickerModesSelector = mkSelector "setAllowedPickerModes:"

-- | @Selector@ for @excludedWindowIDs@
excludedWindowIDsSelector :: Selector '[] (Id NSArray)
excludedWindowIDsSelector = mkSelector "excludedWindowIDs"

-- | @Selector@ for @setExcludedWindowIDs:@
setExcludedWindowIDsSelector :: Selector '[Id NSArray] ()
setExcludedWindowIDsSelector = mkSelector "setExcludedWindowIDs:"

-- | @Selector@ for @excludedBundleIDs@
excludedBundleIDsSelector :: Selector '[] (Id NSArray)
excludedBundleIDsSelector = mkSelector "excludedBundleIDs"

-- | @Selector@ for @setExcludedBundleIDs:@
setExcludedBundleIDsSelector :: Selector '[Id NSArray] ()
setExcludedBundleIDsSelector = mkSelector "setExcludedBundleIDs:"

-- | @Selector@ for @allowsChangingSelectedContent@
allowsChangingSelectedContentSelector :: Selector '[] Bool
allowsChangingSelectedContentSelector = mkSelector "allowsChangingSelectedContent"

-- | @Selector@ for @setAllowsChangingSelectedContent:@
setAllowsChangingSelectedContentSelector :: Selector '[Bool] ()
setAllowsChangingSelectedContentSelector = mkSelector "setAllowsChangingSelectedContent:"

