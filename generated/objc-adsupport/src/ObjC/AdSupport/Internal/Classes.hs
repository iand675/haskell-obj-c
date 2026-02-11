{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AdSupport.Internal.Classes (
    module ObjC.AdSupport.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ASIdentifierManager ----------

-- | The object that contains the advertising identifier.
-- 
-- Phantom type for @ASIdentifierManager@.
data ASIdentifierManager

instance IsObjCObject (Id ASIdentifierManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASIdentifierManager"

class IsNSObject a => IsASIdentifierManager a where
  toASIdentifierManager :: a -> Id ASIdentifierManager

instance IsASIdentifierManager (Id ASIdentifierManager) where
  toASIdentifierManager = unsafeCastId

instance IsNSObject (Id ASIdentifierManager) where
  toNSObject = unsafeCastId
