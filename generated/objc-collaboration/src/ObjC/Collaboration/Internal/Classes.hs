{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Collaboration.Internal.Classes (
    module ObjC.Collaboration.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CBIdentity ----------

-- | A @CBIdentity@ object is used for accessing the attributes of an identity stored in an identity authority. You can use an identity object for finding identities, and storing them in an access control list (ACL). If you need to edit these attributes, take advantage of the @CSIdentity@ class in Core Services.
--
-- You can obtain a @CBIdentity@ object from one of the following class factory methods: ``CBIdentity/identityWithName:authority:``, ``CBIdentity/identityWithUUIDString:authority:``, ``CBIdentity/identityWithPersistentReference:``, or ``CBIdentity/identityWithCSIdentity:``.
--
-- A @CBIdentity@ object has methods to support for interoperability with the Core Services Identity API. Send ``CBIdentity/CSIdentity`` to your @CBIdentity@ object to return an opaque object for use in the Core Services Identity API. Similarly, call ``CBIdentity/identityWithCSIdentity:`` to use an Core Services Identity opaque object in the Collaboration framework.
--
-- There are two subclasses of @CBIdentity@: @CBGroupIdentity@ and @CBUserIdentity@. If you are working specifically with a group identity, use @CBGroupIdentity@. Similarly, if you are working with a user identity, use @CBUserIdentity@.
-- 
-- Phantom type for @CBIdentity@.
data CBIdentity

instance IsObjCObject (Id CBIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBIdentity"

class IsNSObject a => IsCBIdentity a where
  toCBIdentity :: a -> Id CBIdentity

instance IsCBIdentity (Id CBIdentity) where
  toCBIdentity = unsafeCastId

instance IsNSObject (Id CBIdentity) where
  toNSObject = unsafeCastId

-- ---------- CBIdentityAuthority ----------

-- | An identity authority is a database that stores information about identities. The @CBIdentityAuthority@ class defines one or more identity authorities. This database can be searched for identities in conjunction with the @CBIdentity@ class factory methods.
-- 
-- Phantom type for @CBIdentityAuthority@.
data CBIdentityAuthority

instance IsObjCObject (Id CBIdentityAuthority) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBIdentityAuthority"

class IsNSObject a => IsCBIdentityAuthority a where
  toCBIdentityAuthority :: a -> Id CBIdentityAuthority

instance IsCBIdentityAuthority (Id CBIdentityAuthority) where
  toCBIdentityAuthority = unsafeCastId

instance IsNSObject (Id CBIdentityAuthority) where
  toNSObject = unsafeCastId

-- ---------- CBIdentityPicker ----------

-- | A @CBIdentityPicker@ object allows a user to select identities—for example, user or group objects—that it wants one or more services or shared resources to have access to. An identity picker can be displayed either as an application-modal dialog or as a sheet attached to a document window. An identity picker returns the selected records to be added to access control lists using Collaboration. If a selected record is not a user or group identity, then an identity picker prompts the user for additional information—such as a password—to promote that record to a sharing account.
-- 
-- Phantom type for @CBIdentityPicker@.
data CBIdentityPicker

instance IsObjCObject (Id CBIdentityPicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBIdentityPicker"

class IsNSObject a => IsCBIdentityPicker a where
  toCBIdentityPicker :: a -> Id CBIdentityPicker

instance IsCBIdentityPicker (Id CBIdentityPicker) where
  toCBIdentityPicker = unsafeCastId

instance IsNSObject (Id CBIdentityPicker) where
  toNSObject = unsafeCastId

-- ---------- CBGroupIdentity ----------

-- | An object of the @CBGroupIdentity@ class represents a group identity and is used for viewing the attributes of group identities from an identity authority. The principal attributes of a @CBGroupIdentity@ object are a POSIX group identifier (GID) and a list of members.
-- 
-- Phantom type for @CBGroupIdentity@.
data CBGroupIdentity

instance IsObjCObject (Id CBGroupIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBGroupIdentity"

class IsCBIdentity a => IsCBGroupIdentity a where
  toCBGroupIdentity :: a -> Id CBGroupIdentity

instance IsCBGroupIdentity (Id CBGroupIdentity) where
  toCBGroupIdentity = unsafeCastId

instance IsCBIdentity (Id CBGroupIdentity) where
  toCBIdentity = unsafeCastId

instance IsNSObject (Id CBGroupIdentity) where
  toNSObject = unsafeCastId

-- ---------- CBUserIdentity ----------

-- | An object of the @CBUserIdentity@ class represents a user identity and is used for accessing the attributes of a user identity from an identity authority. The principal attributes of @CBUserIdentity@ are a POSIX user identifier (UID), password, and certificate.
-- 
-- Phantom type for @CBUserIdentity@.
data CBUserIdentity

instance IsObjCObject (Id CBUserIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBUserIdentity"

class IsCBIdentity a => IsCBUserIdentity a where
  toCBUserIdentity :: a -> Id CBUserIdentity

instance IsCBUserIdentity (Id CBUserIdentity) where
  toCBUserIdentity = unsafeCastId

instance IsCBIdentity (Id CBUserIdentity) where
  toCBIdentity = unsafeCastId

instance IsNSObject (Id CBUserIdentity) where
  toNSObject = unsafeCastId
