{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PreferencePanes.Internal.Classes (
    module ObjC.PreferencePanes.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- NSPreferencePane ----------

-- | Phantom type for @NSPreferencePane@.
data NSPreferencePane

instance IsObjCObject (Id NSPreferencePane) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPreferencePane"

class IsNSObject a => IsNSPreferencePane a where
  toNSPreferencePane :: a -> Id NSPreferencePane

instance IsNSPreferencePane (Id NSPreferencePane) where
  toNSPreferencePane = unsafeCastId

instance IsNSObject (Id NSPreferencePane) where
  toNSObject = unsafeCastId
