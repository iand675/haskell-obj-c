{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSSystemExtensionProperties@.
module ObjC.SystemExtensions.OSSystemExtensionProperties
  ( OSSystemExtensionProperties
  , IsOSSystemExtensionProperties(..)
  , url
  , bundleIdentifier
  , bundleVersion
  , bundleShortVersion
  , isEnabled
  , isAwaitingUserApproval
  , isUninstalling
  , bundleIdentifierSelector
  , bundleShortVersionSelector
  , bundleVersionSelector
  , isAwaitingUserApprovalSelector
  , isEnabledSelector
  , isUninstallingSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The file URL locating an indicating the extension bundle these properties were retreived from.
--
-- ObjC selector: @- URL@
url :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSURL)
url osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties urlSelector

-- | The bundle identifier of the extension (CFBundleIdentifier)
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleIdentifier osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties bundleIdentifierSelector

-- | The bundle version of the extension (CFBundleVersion)
--
-- ObjC selector: @- bundleVersion@
bundleVersion :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleVersion osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties bundleVersionSelector

-- | The bundle short version string of the extension (CFBundleShortVersionString)
--
-- ObjC selector: @- bundleShortVersion@
bundleShortVersion :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO (Id NSString)
bundleShortVersion osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties bundleShortVersionSelector

-- | Returns the enabled state of the extension
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isEnabled osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties isEnabledSelector

-- | Returns whether an extension is waiting for user approval
--
-- ObjC selector: @- isAwaitingUserApproval@
isAwaitingUserApproval :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isAwaitingUserApproval osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties isAwaitingUserApprovalSelector

-- | Returns if an extension is being uninstalled
--
-- ObjC selector: @- isUninstalling@
isUninstalling :: IsOSSystemExtensionProperties osSystemExtensionProperties => osSystemExtensionProperties -> IO Bool
isUninstalling osSystemExtensionProperties =
  sendMessage osSystemExtensionProperties isUninstallingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @bundleVersion@
bundleVersionSelector :: Selector '[] (Id NSString)
bundleVersionSelector = mkSelector "bundleVersion"

-- | @Selector@ for @bundleShortVersion@
bundleShortVersionSelector :: Selector '[] (Id NSString)
bundleShortVersionSelector = mkSelector "bundleShortVersion"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @isAwaitingUserApproval@
isAwaitingUserApprovalSelector :: Selector '[] Bool
isAwaitingUserApprovalSelector = mkSelector "isAwaitingUserApproval"

-- | @Selector@ for @isUninstalling@
isUninstallingSelector :: Selector '[] Bool
isUninstallingSelector = mkSelector "isUninstalling"

