{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Social.Internal.Classes (
    module ObjC.Social.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SLRequest ----------

-- | Phantom type for @SLRequest@.
data SLRequest

instance IsObjCObject (Id SLRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SLRequest"

class IsNSObject a => IsSLRequest a where
  toSLRequest :: a -> Id SLRequest

instance IsSLRequest (Id SLRequest) where
  toSLRequest = unsafeCastId

instance IsNSObject (Id SLRequest) where
  toNSObject = unsafeCastId

-- ---------- SLComposeServiceViewController ----------

-- | Phantom type for @SLComposeServiceViewController@.
data SLComposeServiceViewController

instance IsObjCObject (Id SLComposeServiceViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SLComposeServiceViewController"

class IsNSViewController a => IsSLComposeServiceViewController a where
  toSLComposeServiceViewController :: a -> Id SLComposeServiceViewController

instance IsSLComposeServiceViewController (Id SLComposeServiceViewController) where
  toSLComposeServiceViewController = unsafeCastId

instance IsNSObject (Id SLComposeServiceViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SLComposeServiceViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id SLComposeServiceViewController) where
  toNSViewController = unsafeCastId
