{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SecurityFoundation.Internal.Classes (
    module ObjC.SecurityFoundation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SFAuthorization ----------

-- | SFAuthorization
--
-- SFAuthorization APIs are used for implementing access control in applications and daemons. It has NSCoder support for proxied objects SFAuthorization is a wrapper for using the Authorization API.
-- 
-- Phantom type for @SFAuthorization@.
data SFAuthorization

instance IsObjCObject (Id SFAuthorization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAuthorization"

class IsNSObject a => IsSFAuthorization a where
  toSFAuthorization :: a -> Id SFAuthorization

instance IsSFAuthorization (Id SFAuthorization) where
  toSFAuthorization = unsafeCastId

instance IsNSObject (Id SFAuthorization) where
  toNSObject = unsafeCastId
