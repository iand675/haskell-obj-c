{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ServiceManagement.Internal.Classes (
    module ObjC.ServiceManagement.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SMAppService ----------

-- | An SMAppService is used to control helper executables that live inside of an app's main bundle.
--
-- For SMAppServices initialized as LoginItems, the register and unregister APIs provide a replacement for SMLoginItemSetEnabled.
--
-- Apps that use SMAppService APIs must be code signed.
--
-- For SMAppServices initialized as LaunchAgents, the register and unregister APIs provide a replacement for installing plists in ~/Library/LaunchAgents or /Library/LaunchAgents.
--
-- For SMAppServices initialized as LaunchDaemons, the register and unregister APIs provide a replacement for installing plists in /Library/LaunchDaemons. Apps that contain LaunchDaemons must be notarized.
--
-- Legacy LaunchDaemons installed in /Library/LaunchDaemons will continue to be bootstrapped without explicit approval in System Settings since writing to /Library is protected  with filesystem permissions.
--
-- If an app updates either the plist or the executable for a LaunchAgent or LaunchDaemon, the SMAppService must be re-registered or it may not launch. It is recommended to also call unregister before re-registering if the executable has been changed.
-- 
-- Phantom type for @SMAppService@.
data SMAppService

instance IsObjCObject (Id SMAppService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SMAppService"

class IsNSObject a => IsSMAppService a where
  toSMAppService :: a -> Id SMAppService

instance IsSMAppService (Id SMAppService) where
  toSMAppService = unsafeCastId

instance IsNSObject (Id SMAppService) where
  toNSObject = unsafeCastId
