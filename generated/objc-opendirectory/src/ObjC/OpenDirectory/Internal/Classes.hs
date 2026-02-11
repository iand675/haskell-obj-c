{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.OpenDirectory.Internal.Classes (
    module ObjC.OpenDirectory.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SecurityFoundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- ---------- ODAttributeMap ----------

-- | Phantom type for @ODAttributeMap@.
data ODAttributeMap

instance IsObjCObject (Id ODAttributeMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODAttributeMap"

class IsNSObject a => IsODAttributeMap a where
  toODAttributeMap :: a -> Id ODAttributeMap

instance IsODAttributeMap (Id ODAttributeMap) where
  toODAttributeMap = unsafeCastId

instance IsNSObject (Id ODAttributeMap) where
  toNSObject = unsafeCastId

-- ---------- ODConfiguration ----------

-- | Phantom type for @ODConfiguration@.
data ODConfiguration

instance IsObjCObject (Id ODConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODConfiguration"

class IsNSObject a => IsODConfiguration a where
  toODConfiguration :: a -> Id ODConfiguration

instance IsODConfiguration (Id ODConfiguration) where
  toODConfiguration = unsafeCastId

instance IsNSObject (Id ODConfiguration) where
  toNSObject = unsafeCastId

-- ---------- ODMappings ----------

-- | Phantom type for @ODMappings@.
data ODMappings

instance IsObjCObject (Id ODMappings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODMappings"

class IsNSObject a => IsODMappings a where
  toODMappings :: a -> Id ODMappings

instance IsODMappings (Id ODMappings) where
  toODMappings = unsafeCastId

instance IsNSObject (Id ODMappings) where
  toNSObject = unsafeCastId

-- ---------- ODModuleEntry ----------

-- | Phantom type for @ODModuleEntry@.
data ODModuleEntry

instance IsObjCObject (Id ODModuleEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODModuleEntry"

class IsNSObject a => IsODModuleEntry a where
  toODModuleEntry :: a -> Id ODModuleEntry

instance IsODModuleEntry (Id ODModuleEntry) where
  toODModuleEntry = unsafeCastId

instance IsNSObject (Id ODModuleEntry) where
  toNSObject = unsafeCastId

-- ---------- ODNode ----------

-- | ODNode
--
-- This class is used to work with OpenDirectory nodes.
--
-- OpenDirectory uses nodes to represent different sources of directory information, via the local disk, LDAP, etc.
-- 
-- Phantom type for @ODNode@.
data ODNode

instance IsObjCObject (Id ODNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODNode"

class IsNSObject a => IsODNode a where
  toODNode :: a -> Id ODNode

instance IsODNode (Id ODNode) where
  toODNode = unsafeCastId

instance IsNSObject (Id ODNode) where
  toNSObject = unsafeCastId

-- ---------- ODQuery ----------

-- | ODQuery
--
-- Class used for querying OpenDirectory.
--
-- OpenDirectory queries may be used to search for different types of records, e.g. users, groups.
-- 
-- Phantom type for @ODQuery@.
data ODQuery

instance IsObjCObject (Id ODQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODQuery"

class IsNSObject a => IsODQuery a where
  toODQuery :: a -> Id ODQuery

instance IsODQuery (Id ODQuery) where
  toODQuery = unsafeCastId

instance IsNSObject (Id ODQuery) where
  toNSObject = unsafeCastId

-- ---------- ODRecord ----------

-- | ODRecord
--
-- This class is used to read, update and modify records within the directory
--
-- This class is used to read, update and modify records within the directory.  outError is optional parameter,                 nil can be passed if error details are not needed.
-- 
-- Phantom type for @ODRecord@.
data ODRecord

instance IsObjCObject (Id ODRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODRecord"

class IsNSObject a => IsODRecord a where
  toODRecord :: a -> Id ODRecord

instance IsODRecord (Id ODRecord) where
  toODRecord = unsafeCastId

instance IsNSObject (Id ODRecord) where
  toNSObject = unsafeCastId

-- ---------- ODRecordMap ----------

-- | Phantom type for @ODRecordMap@.
data ODRecordMap

instance IsObjCObject (Id ODRecordMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODRecordMap"

class IsNSObject a => IsODRecordMap a where
  toODRecordMap :: a -> Id ODRecordMap

instance IsODRecordMap (Id ODRecordMap) where
  toODRecordMap = unsafeCastId

instance IsNSObject (Id ODRecordMap) where
  toNSObject = unsafeCastId

-- ---------- ODSession ----------

-- | ODSession
--
-- Class for working with OpenDirectory sessions.
--
-- Class for working with OpenDirectory sessions.
-- 
-- Phantom type for @ODSession@.
data ODSession

instance IsObjCObject (Id ODSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ODSession"

class IsNSObject a => IsODSession a where
  toODSession :: a -> Id ODSession

instance IsODSession (Id ODSession) where
  toODSession = unsafeCastId

instance IsNSObject (Id ODSession) where
  toNSObject = unsafeCastId
