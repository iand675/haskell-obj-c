{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSSystemExtensionProperties@.
module ObjC.SystemExtensions.OSSystemExtensionProperties
  ( OSSystemExtensionProperties
  , IsOSSystemExtensionProperties(..)
  , bundleIdentifier
  , bundleVersion
  , bundleShortVersion
  , isEnabled
  , isAwaitingUserApproval
  , isUninstalling
  , bundleIdentifierSelector
  , bundleVersionSelector
  , bundleShortVersionSelector
  , isEnabledSelector
  , isAwaitingUserApprovalSelector
  , isUninstallingSelector


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

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The bundle identifier of the extension (CFBundleIdentifier)
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleIdentifier osSystemExtensionProperties  =
  sendMsg osSystemExtensionProperties (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bundle version of the extension (CFBundleVersion)
--
-- ObjC selector: @- bundleVersion@
bundleVersion :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleVersion osSystemExtensionProperties  =
  sendMsg osSystemExtensionProperties (mkSelector "bundleVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bundle short version string of the extension (CFBundleShortVersionString)
--
-- ObjC selector: @- bundleShortVersion@
bundleShortVersion :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleShortVersion osSystemExtensionProperties  =
  sendMsg osSystemExtensionProperties (mkSelector "bundleShortVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the enabled state of the extension
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isEnabled osSystemExtensionProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osSystemExtensionProperties (mkSelector "isEnabled") retCULong []

-- | Returns whether an extension is waiting for user approval
--
-- ObjC selector: @- isAwaitingUserApproval@
isAwaitingUserApproval :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isAwaitingUserApproval osSystemExtensionProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osSystemExtensionProperties (mkSelector "isAwaitingUserApproval") retCULong []

-- | Returns if an extension is being uninstalled
--
-- ObjC selector: @- isUninstalling@
isUninstalling :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isUninstalling osSystemExtensionProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osSystemExtensionProperties (mkSelector "isUninstalling") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @bundleVersion@
bundleVersionSelector :: Selector
bundleVersionSelector = mkSelector "bundleVersion"

-- | @Selector@ for @bundleShortVersion@
bundleShortVersionSelector :: Selector
bundleShortVersionSelector = mkSelector "bundleShortVersion"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @isAwaitingUserApproval@
isAwaitingUserApprovalSelector :: Selector
isAwaitingUserApprovalSelector = mkSelector "isAwaitingUserApproval"

-- | @Selector@ for @isUninstalling@
isUninstallingSelector :: Selector
isUninstallingSelector = mkSelector "isUninstalling"

