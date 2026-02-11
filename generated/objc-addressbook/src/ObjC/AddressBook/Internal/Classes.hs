{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AddressBook.Internal.Classes (
    module ObjC.AddressBook.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ABAddressBook ----------

-- | Phantom type for @ABAddressBook@.
data ABAddressBook

instance IsObjCObject (Id ABAddressBook) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABAddressBook"

class IsNSObject a => IsABAddressBook a where
  toABAddressBook :: a -> Id ABAddressBook

instance IsABAddressBook (Id ABAddressBook) where
  toABAddressBook = unsafeCastId

instance IsNSObject (Id ABAddressBook) where
  toNSObject = unsafeCastId

-- ---------- ABMultiValue ----------

-- | Phantom type for @ABMultiValue@.
data ABMultiValue

instance IsObjCObject (Id ABMultiValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABMultiValue"

class IsNSObject a => IsABMultiValue a where
  toABMultiValue :: a -> Id ABMultiValue

instance IsABMultiValue (Id ABMultiValue) where
  toABMultiValue = unsafeCastId

instance IsNSObject (Id ABMultiValue) where
  toNSObject = unsafeCastId

-- ---------- ABRecord ----------

-- | Phantom type for @ABRecord@.
data ABRecord

instance IsObjCObject (Id ABRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABRecord"

class IsNSObject a => IsABRecord a where
  toABRecord :: a -> Id ABRecord

instance IsABRecord (Id ABRecord) where
  toABRecord = unsafeCastId

instance IsNSObject (Id ABRecord) where
  toNSObject = unsafeCastId

-- ---------- ABSearchElement ----------

-- | Phantom type for @ABSearchElement@.
data ABSearchElement

instance IsObjCObject (Id ABSearchElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABSearchElement"

class IsNSObject a => IsABSearchElement a where
  toABSearchElement :: a -> Id ABSearchElement

instance IsABSearchElement (Id ABSearchElement) where
  toABSearchElement = unsafeCastId

instance IsNSObject (Id ABSearchElement) where
  toNSObject = unsafeCastId

-- ---------- ABMutableMultiValue ----------

-- | Phantom type for @ABMutableMultiValue@.
data ABMutableMultiValue

instance IsObjCObject (Id ABMutableMultiValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABMutableMultiValue"

class IsABMultiValue a => IsABMutableMultiValue a where
  toABMutableMultiValue :: a -> Id ABMutableMultiValue

instance IsABMutableMultiValue (Id ABMutableMultiValue) where
  toABMutableMultiValue = unsafeCastId

instance IsABMultiValue (Id ABMutableMultiValue) where
  toABMultiValue = unsafeCastId

instance IsNSObject (Id ABMutableMultiValue) where
  toNSObject = unsafeCastId

-- ---------- ABGroup ----------

-- | Phantom type for @ABGroup@.
data ABGroup

instance IsObjCObject (Id ABGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABGroup"

class IsABRecord a => IsABGroup a where
  toABGroup :: a -> Id ABGroup

instance IsABGroup (Id ABGroup) where
  toABGroup = unsafeCastId

instance IsABRecord (Id ABGroup) where
  toABRecord = unsafeCastId

instance IsNSObject (Id ABGroup) where
  toNSObject = unsafeCastId

-- ---------- ABPerson ----------

-- | Phantom type for @ABPerson@.
data ABPerson

instance IsObjCObject (Id ABPerson) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ABPerson"

class IsABRecord a => IsABPerson a where
  toABPerson :: a -> Id ABPerson

instance IsABPerson (Id ABPerson) where
  toABPerson = unsafeCastId

instance IsABRecord (Id ABPerson) where
  toABRecord = unsafeCastId

instance IsNSObject (Id ABPerson) where
  toNSObject = unsafeCastId
