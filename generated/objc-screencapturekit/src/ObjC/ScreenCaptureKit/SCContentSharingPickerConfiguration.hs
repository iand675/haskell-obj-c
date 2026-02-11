{-# LANGUAGE PatternSynonyms #-}
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
  , setAllowedPickerModesSelector
  , excludedWindowIDsSelector
  , setExcludedWindowIDsSelector
  , excludedBundleIDsSelector
  , setExcludedBundleIDsSelector
  , allowsChangingSelectedContentSelector
  , setAllowsChangingSelectedContentSelector

  -- * Enum types
  , SCContentSharingPickerMode(SCContentSharingPickerMode)
  , pattern SCContentSharingPickerModeSingleWindow
  , pattern SCContentSharingPickerModeMultipleWindows
  , pattern SCContentSharingPickerModeSingleApplication
  , pattern SCContentSharingPickerModeMultipleApplications
  , pattern SCContentSharingPickerModeSingleDisplay

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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | allowedPickerModes Limits the type of selections available to the user when the picker is presented. Default is 0, no excluded picking modes
--
-- ObjC selector: @- allowedPickerModes@
allowedPickerModes :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO SCContentSharingPickerMode
allowedPickerModes scContentSharingPickerConfiguration  =
  fmap (coerce :: CULong -> SCContentSharingPickerMode) $ sendMsg scContentSharingPickerConfiguration (mkSelector "allowedPickerModes") retCULong []

-- | allowedPickerModes Limits the type of selections available to the user when the picker is presented. Default is 0, no excluded picking modes
--
-- ObjC selector: @- setAllowedPickerModes:@
setAllowedPickerModes :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> SCContentSharingPickerMode -> IO ()
setAllowedPickerModes scContentSharingPickerConfiguration  value =
  sendMsg scContentSharingPickerConfiguration (mkSelector "setAllowedPickerModes:") retVoid [argCULong (coerce value)]

-- | excludedWindowIDs Excludes CGWindowIDs for picking
--
-- ObjC selector: @- excludedWindowIDs@
excludedWindowIDs :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO (Id NSArray)
excludedWindowIDs scContentSharingPickerConfiguration  =
  sendMsg scContentSharingPickerConfiguration (mkSelector "excludedWindowIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedWindowIDs Excludes CGWindowIDs for picking
--
-- ObjC selector: @- setExcludedWindowIDs:@
setExcludedWindowIDs :: (IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration, IsNSArray value) => scContentSharingPickerConfiguration -> value -> IO ()
setExcludedWindowIDs scContentSharingPickerConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scContentSharingPickerConfiguration (mkSelector "setExcludedWindowIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedBundleIDs Excludes bundle IDs for picking
--
-- ObjC selector: @- excludedBundleIDs@
excludedBundleIDs :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO (Id NSArray)
excludedBundleIDs scContentSharingPickerConfiguration  =
  sendMsg scContentSharingPickerConfiguration (mkSelector "excludedBundleIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedBundleIDs Excludes bundle IDs for picking
--
-- ObjC selector: @- setExcludedBundleIDs:@
setExcludedBundleIDs :: (IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration, IsNSArray value) => scContentSharingPickerConfiguration -> value -> IO ()
setExcludedBundleIDs scContentSharingPickerConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scContentSharingPickerConfiguration (mkSelector "setExcludedBundleIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | allowsChangingSelectedContent Controls if the user can make updates to the content filter after the initial selection. Defaults is YES.
--
-- ObjC selector: @- allowsChangingSelectedContent@
allowsChangingSelectedContent :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> IO Bool
allowsChangingSelectedContent scContentSharingPickerConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scContentSharingPickerConfiguration (mkSelector "allowsChangingSelectedContent") retCULong []

-- | allowsChangingSelectedContent Controls if the user can make updates to the content filter after the initial selection. Defaults is YES.
--
-- ObjC selector: @- setAllowsChangingSelectedContent:@
setAllowsChangingSelectedContent :: IsSCContentSharingPickerConfiguration scContentSharingPickerConfiguration => scContentSharingPickerConfiguration -> Bool -> IO ()
setAllowsChangingSelectedContent scContentSharingPickerConfiguration  value =
  sendMsg scContentSharingPickerConfiguration (mkSelector "setAllowsChangingSelectedContent:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowedPickerModes@
allowedPickerModesSelector :: Selector
allowedPickerModesSelector = mkSelector "allowedPickerModes"

-- | @Selector@ for @setAllowedPickerModes:@
setAllowedPickerModesSelector :: Selector
setAllowedPickerModesSelector = mkSelector "setAllowedPickerModes:"

-- | @Selector@ for @excludedWindowIDs@
excludedWindowIDsSelector :: Selector
excludedWindowIDsSelector = mkSelector "excludedWindowIDs"

-- | @Selector@ for @setExcludedWindowIDs:@
setExcludedWindowIDsSelector :: Selector
setExcludedWindowIDsSelector = mkSelector "setExcludedWindowIDs:"

-- | @Selector@ for @excludedBundleIDs@
excludedBundleIDsSelector :: Selector
excludedBundleIDsSelector = mkSelector "excludedBundleIDs"

-- | @Selector@ for @setExcludedBundleIDs:@
setExcludedBundleIDsSelector :: Selector
setExcludedBundleIDsSelector = mkSelector "setExcludedBundleIDs:"

-- | @Selector@ for @allowsChangingSelectedContent@
allowsChangingSelectedContentSelector :: Selector
allowsChangingSelectedContentSelector = mkSelector "allowsChangingSelectedContent"

-- | @Selector@ for @setAllowsChangingSelectedContent:@
setAllowsChangingSelectedContentSelector :: Selector
setAllowsChangingSelectedContentSelector = mkSelector "setAllowsChangingSelectedContent:"

