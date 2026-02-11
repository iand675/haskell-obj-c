{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ExceptionHandling.Internal.Classes (
    module ObjC.ExceptionHandling.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- NSExceptionHandler ----------

-- | Phantom type for @NSExceptionHandler@.
data NSExceptionHandler

instance IsObjCObject (Id NSExceptionHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSExceptionHandler"

class IsNSObject a => IsNSExceptionHandler a where
  toNSExceptionHandler :: a -> Id NSExceptionHandler

instance IsNSExceptionHandler (Id NSExceptionHandler) where
  toNSExceptionHandler = unsafeCastId

instance IsNSObject (Id NSExceptionHandler) where
  toNSObject = unsafeCastId
