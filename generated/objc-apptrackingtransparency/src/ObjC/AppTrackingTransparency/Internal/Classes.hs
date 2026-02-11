{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AppTrackingTransparency.Internal.Classes (
    module ObjC.AppTrackingTransparency.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ATTrackingManager ----------

-- | A class that provides a tracking authorization request and the tracking authorization status of the app.
-- 
-- Phantom type for @ATTrackingManager@.
data ATTrackingManager

instance IsObjCObject (Id ATTrackingManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ATTrackingManager"

class IsNSObject a => IsATTrackingManager a where
  toATTrackingManager :: a -> Id ATTrackingManager

instance IsATTrackingManager (Id ATTrackingManager) where
  toATTrackingManager = unsafeCastId

instance IsNSObject (Id ATTrackingManager) where
  toNSObject = unsafeCastId
