{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SyncServices.Internal.Classes (
    module ObjC.SyncServices.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ISyncChange ----------

-- | Phantom type for @ISyncChange@.
data ISyncChange

instance IsObjCObject (Id ISyncChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncChange"

class IsNSObject a => IsISyncChange a where
  toISyncChange :: a -> Id ISyncChange

instance IsISyncChange (Id ISyncChange) where
  toISyncChange = unsafeCastId

instance IsNSObject (Id ISyncChange) where
  toNSObject = unsafeCastId

-- ---------- ISyncClient ----------

-- | Phantom type for @ISyncClient@.
data ISyncClient

instance IsObjCObject (Id ISyncClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncClient"

class IsNSObject a => IsISyncClient a where
  toISyncClient :: a -> Id ISyncClient

instance IsISyncClient (Id ISyncClient) where
  toISyncClient = unsafeCastId

instance IsNSObject (Id ISyncClient) where
  toNSObject = unsafeCastId

-- ---------- ISyncFilter ----------

-- | Phantom type for @ISyncFilter@.
data ISyncFilter

instance IsObjCObject (Id ISyncFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncFilter"

class IsNSObject a => IsISyncFilter a where
  toISyncFilter :: a -> Id ISyncFilter

instance IsISyncFilter (Id ISyncFilter) where
  toISyncFilter = unsafeCastId

instance IsNSObject (Id ISyncFilter) where
  toNSObject = unsafeCastId

-- ---------- ISyncManager ----------

-- | Phantom type for @ISyncManager@.
data ISyncManager

instance IsObjCObject (Id ISyncManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncManager"

class IsNSObject a => IsISyncManager a where
  toISyncManager :: a -> Id ISyncManager

instance IsISyncManager (Id ISyncManager) where
  toISyncManager = unsafeCastId

instance IsNSObject (Id ISyncManager) where
  toNSObject = unsafeCastId

-- ---------- ISyncRecordSnapshot ----------

-- | Phantom type for @ISyncRecordSnapshot@.
data ISyncRecordSnapshot

instance IsObjCObject (Id ISyncRecordSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncRecordSnapshot"

class IsNSObject a => IsISyncRecordSnapshot a where
  toISyncRecordSnapshot :: a -> Id ISyncRecordSnapshot

instance IsISyncRecordSnapshot (Id ISyncRecordSnapshot) where
  toISyncRecordSnapshot = unsafeCastId

instance IsNSObject (Id ISyncRecordSnapshot) where
  toNSObject = unsafeCastId

-- ---------- ISyncSession ----------

-- | Phantom type for @ISyncSession@.
data ISyncSession

instance IsObjCObject (Id ISyncSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncSession"

class IsNSObject a => IsISyncSession a where
  toISyncSession :: a -> Id ISyncSession

instance IsISyncSession (Id ISyncSession) where
  toISyncSession = unsafeCastId

instance IsNSObject (Id ISyncSession) where
  toNSObject = unsafeCastId

-- ---------- ISyncSessionDriver ----------

-- | Phantom type for @ISyncSessionDriver@.
data ISyncSessionDriver

instance IsObjCObject (Id ISyncSessionDriver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ISyncSessionDriver"

class IsNSObject a => IsISyncSessionDriver a where
  toISyncSessionDriver :: a -> Id ISyncSessionDriver

instance IsISyncSessionDriver (Id ISyncSessionDriver) where
  toISyncSessionDriver = unsafeCastId

instance IsNSObject (Id ISyncSessionDriver) where
  toNSObject = unsafeCastId
