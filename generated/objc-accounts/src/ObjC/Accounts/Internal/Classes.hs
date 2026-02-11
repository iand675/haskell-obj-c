{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Accounts.Internal.Classes (
    module ObjC.Accounts.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ACAccount ----------

-- | Phantom type for @ACAccount@.
data ACAccount

instance IsObjCObject (Id ACAccount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ACAccount"

class IsNSObject a => IsACAccount a where
  toACAccount :: a -> Id ACAccount

instance IsACAccount (Id ACAccount) where
  toACAccount = unsafeCastId

instance IsNSObject (Id ACAccount) where
  toNSObject = unsafeCastId

-- ---------- ACAccountCredential ----------

-- | Phantom type for @ACAccountCredential@.
data ACAccountCredential

instance IsObjCObject (Id ACAccountCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ACAccountCredential"

class IsNSObject a => IsACAccountCredential a where
  toACAccountCredential :: a -> Id ACAccountCredential

instance IsACAccountCredential (Id ACAccountCredential) where
  toACAccountCredential = unsafeCastId

instance IsNSObject (Id ACAccountCredential) where
  toNSObject = unsafeCastId

-- ---------- ACAccountStore ----------

-- | Phantom type for @ACAccountStore@.
data ACAccountStore

instance IsObjCObject (Id ACAccountStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ACAccountStore"

class IsNSObject a => IsACAccountStore a where
  toACAccountStore :: a -> Id ACAccountStore

instance IsACAccountStore (Id ACAccountStore) where
  toACAccountStore = unsafeCastId

instance IsNSObject (Id ACAccountStore) where
  toNSObject = unsafeCastId

-- ---------- ACAccountType ----------

-- | Phantom type for @ACAccountType@.
data ACAccountType

instance IsObjCObject (Id ACAccountType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ACAccountType"

class IsNSObject a => IsACAccountType a where
  toACAccountType :: a -> Id ACAccountType

instance IsACAccountType (Id ACAccountType) where
  toACAccountType = unsafeCastId

instance IsNSObject (Id ACAccountType) where
  toNSObject = unsafeCastId
