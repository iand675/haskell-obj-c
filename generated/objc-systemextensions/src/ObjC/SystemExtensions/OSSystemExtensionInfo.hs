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
  , bundleVersionSelector
  , bundleShortVersionSelector


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
bundleIdentifier :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleIdentifier osSystemExtensionInfo  =
  sendMsg osSystemExtensionInfo (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bundle version of the extension (CFBundleVersion)
--
-- ObjC selector: @- bundleVersion@
bundleVersion :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleVersion osSystemExtensionInfo  =
  sendMsg osSystemExtensionInfo (mkSelector "bundleVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bundle short version string of the extension (CFBundleShortVersionString)
--
-- ObjC selector: @- bundleShortVersion@
bundleShortVersion :: IsOSSystemExtensionInfo osSystemExtensionInfo => osSystemExtensionInfo -> IO (Id NSString)
bundleShortVersion osSystemExtensionInfo  =
  sendMsg osSystemExtensionInfo (mkSelector "bundleShortVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

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

