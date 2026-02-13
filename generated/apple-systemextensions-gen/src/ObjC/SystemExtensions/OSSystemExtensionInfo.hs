{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSSystemExtensionInfo@.
module ObjC.SystemExtensions.OSSystemExtensionInfo
  ( OSSystemExtensionInfo
  , IsOSSystemExtensionInfo(..)
  , bundleIdentifier
  , bundleVersion
  , bundleShortVersion
  , bundleIdentifierSelector
  , bundleShortVersionSelector
  , bundleVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The bundle identifier of the extension (CFBundleIdentifier)
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleIdentifier osSystemExtensionInfo =
  sendMessage osSystemExtensionInfo bundleIdentifierSelector

-- | The bundle version of the extension (CFBundleVersion)
--
-- ObjC selector: @- bundleVersion@
bundleVersion :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleVersion osSystemExtensionInfo =
  sendMessage osSystemExtensionInfo bundleVersionSelector

-- | The bundle short version string of the extension (CFBundleShortVersionString)
--
-- ObjC selector: @- bundleShortVersion@
bundleShortVersion :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleShortVersion osSystemExtensionInfo =
  sendMessage osSystemExtensionInfo bundleShortVersionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @bundleVersion@
bundleVersionSelector :: Selector '[] (Id NSString)
bundleVersionSelector = mkSelector "bundleVersion"

-- | @Selector@ for @bundleShortVersion@
bundleShortVersionSelector :: Selector '[] (Id NSString)
bundleShortVersionSelector = mkSelector "bundleShortVersion"

