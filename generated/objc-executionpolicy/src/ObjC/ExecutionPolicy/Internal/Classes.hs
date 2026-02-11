{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ExecutionPolicy.Internal.Classes (
    module ObjC.ExecutionPolicy.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- EPDeveloperTool ----------

-- | Phantom type for @EPDeveloperTool@.
data EPDeveloperTool

instance IsObjCObject (Id EPDeveloperTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EPDeveloperTool"

class IsNSObject a => IsEPDeveloperTool a where
  toEPDeveloperTool :: a -> Id EPDeveloperTool

instance IsEPDeveloperTool (Id EPDeveloperTool) where
  toEPDeveloperTool = unsafeCastId

instance IsNSObject (Id EPDeveloperTool) where
  toNSObject = unsafeCastId

-- ---------- EPExecutionPolicy ----------

-- | Phantom type for @EPExecutionPolicy@.
data EPExecutionPolicy

instance IsObjCObject (Id EPExecutionPolicy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EPExecutionPolicy"

class IsNSObject a => IsEPExecutionPolicy a where
  toEPExecutionPolicy :: a -> Id EPExecutionPolicy

instance IsEPExecutionPolicy (Id EPExecutionPolicy) where
  toEPExecutionPolicy = unsafeCastId

instance IsNSObject (Id EPExecutionPolicy) where
  toNSObject = unsafeCastId
