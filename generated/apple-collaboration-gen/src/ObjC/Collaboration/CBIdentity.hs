{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A @CBIdentity@ object is used for accessing the attributes of an identity stored in an identity authority. You can use an identity object for finding identities, and storing them in an access control list (ACL). If you need to edit these attributes, take advantage of the @CSIdentity@ class in Core Services.
--
-- You can obtain a @CBIdentity@ object from one of the following class factory methods: ``CBIdentity/identityWithName:authority:``, ``CBIdentity/identityWithUUIDString:authority:``, ``CBIdentity/identityWithPersistentReference:``, or ``CBIdentity/identityWithCSIdentity:``.
--
-- A @CBIdentity@ object has methods to support for interoperability with the Core Services Identity API. Send ``CBIdentity/CSIdentity`` to your @CBIdentity@ object to return an opaque object for use in the Core Services Identity API. Similarly, call ``CBIdentity/identityWithCSIdentity:`` to use an Core Services Identity opaque object in the Collaboration framework.
--
-- There are two subclasses of @CBIdentity@: @CBGroupIdentity@ and @CBUserIdentity@. If you are working specifically with a group identity, use @CBGroupIdentity@. Similarly, if you are working with a user identity, use @CBUserIdentity@.
--
-- Generated bindings for @CBIdentity@.
module ObjC.Collaboration.CBIdentity
  ( CBIdentity
  , IsCBIdentity(..)
  , identityWithName_authority
  , identityWithUniqueIdentifier_authority
  , identityWithUUIDString_authority
  , identityWithPersistentReference
  , identityWithCSIdentity
  , isMemberOfGroup
  , authority
  , uniqueIdentifier
  , uuidString
  , fullName
  , posixName
  , aliases
  , emailAddress
  , image
  , persistentReference
  , hidden
  , csIdentity
  , aliasesSelector
  , authoritySelector
  , csIdentitySelector
  , emailAddressSelector
  , fullNameSelector
  , hiddenSelector
  , identityWithCSIdentitySelector
  , identityWithName_authoritySelector
  , identityWithPersistentReferenceSelector
  , identityWithUUIDString_authoritySelector
  , identityWithUniqueIdentifier_authoritySelector
  , imageSelector
  , isMemberOfGroupSelector
  , persistentReferenceSelector
  , posixNameSelector
  , uniqueIdentifierSelector
  , uuidStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Collaboration.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the identity object with the given name from the specified identity authority.
--
-- The name is compared against all valid identity names, including full names, short names, email addresses, and aliases.
--
-- - Parameters:   - name: The name of the identity.
--
-- - authority: The identity authority to search.
--
-- - Returns: The identity object, or @nil@ if no identity is found with the specified name.
--
-- ObjC selector: @+ identityWithName:authority:@
identityWithName_authority :: (IsNSString name, IsCBIdentityAuthority authority) => name -> authority -> IO (Id CBIdentity)
identityWithName_authority name authority =
  do
    cls' <- getRequiredClass "CBIdentity"
    sendClassMessage cls' identityWithName_authoritySelector (toNSString name) (toCBIdentityAuthority authority)

-- | @+ identityWithUniqueIdentifier:authority:@
identityWithUniqueIdentifier_authority :: (IsNSUUID uuid, IsCBIdentityAuthority authority) => uuid -> authority -> IO (Id CBIdentity)
identityWithUniqueIdentifier_authority uuid authority =
  do
    cls' <- getRequiredClass "CBIdentity"
    sendClassMessage cls' identityWithUniqueIdentifier_authoritySelector (toNSUUID uuid) (toCBIdentityAuthority authority)

-- | Returns the identity object with the given UUID from the specified identity authority.
--
-- - Parameters:   - uuid: The UUID of the identity you are searching for.
--
-- - authority: The identity authority to search.
--
-- - Returns: The identity object, or @nil@ if no identity is found with the matching criteria.
--
-- ObjC selector: @+ identityWithUUIDString:authority:@
identityWithUUIDString_authority :: (IsNSString uuid, IsCBIdentityAuthority authority) => uuid -> authority -> IO (Id CBIdentity)
identityWithUUIDString_authority uuid authority =
  do
    cls' <- getRequiredClass "CBIdentity"
    sendClassMessage cls' identityWithUUIDString_authoritySelector (toNSString uuid) (toCBIdentityAuthority authority)

-- | Returns the identity object matching the persistent reference data.
--
-- A persistent reference is an opaque data object suitable for persistent storage.
--
-- - Parameters:   - data: The persistent data object that refers to an identity.
--
-- - Returns: The identity object matching the persistent data object, or @nil@ if the identity is not found.
--
-- ObjC selector: @+ identityWithPersistentReference:@
identityWithPersistentReference :: IsNSData data_ => data_ -> IO (Id CBIdentity)
identityWithPersistentReference data_ =
  do
    cls' <- getRequiredClass "CBIdentity"
    sendClassMessage cls' identityWithPersistentReferenceSelector (toNSData data_)

-- | Returns an identity object created from the specified Core Services Identity opaque object.
--
-- This method is used for interoperability with the Core Services Identity API.
--
-- - Parameters:   - csIdentity: The Core Services Identity opaque object.
--
-- - Returns: The identity object for use with the Collaboration framework.
--
-- ObjC selector: @+ identityWithCSIdentity:@
identityWithCSIdentity :: Ptr () -> IO (Id CBIdentity)
identityWithCSIdentity csIdentity =
  do
    cls' <- getRequiredClass "CBIdentity"
    sendClassMessage cls' identityWithCSIdentitySelector csIdentity

-- | Returns a Boolean value indicating whether the identity is a member of the specified group.
--
-- - Parameters:   - group: The group to check for membership.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the identity is a member of the group; <doc://com.apple.documentation/documentation/objectivec/no> if it is not.
--
-- ObjC selector: @- isMemberOfGroup:@
isMemberOfGroup :: (IsCBIdentity cbIdentity, IsCBGroupIdentity group) => cbIdentity -> group -> IO Bool
isMemberOfGroup cbIdentity group =
  sendMessage cbIdentity isMemberOfGroupSelector (toCBGroupIdentity group)

-- | Returns the identity authority where the identity is stored.
--
-- - Returns: The identity authority where the identity is stored.
--
-- ObjC selector: @- authority@
authority :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id CBIdentityAuthority)
authority cbIdentity =
  sendMessage cbIdentity authoritySelector

-- | @- uniqueIdentifier@
uniqueIdentifier :: IsCBIdentity cbIdentity => cbIdentity -> IO RawId
uniqueIdentifier cbIdentity =
  sendMessage cbIdentity uniqueIdentifierSelector

-- | Returns the UUID of the identity as a string.
--
-- The UUID string is generated so it is unique across all identity authorities. When storing ACLs, one method is to store the UUID of each identity. However, it is recommended that you use a persistent data object instead (see ``CBIdentity/persistentReference``).
--
-- - Returns: The UUID string of the identity.
--
-- ObjC selector: @- UUIDString@
uuidString :: IsCBIdentity cbIdentity => cbIdentity -> IO RawId
uuidString cbIdentity =
  sendMessage cbIdentity uuidStringSelector

-- | Returns the full name of the identity.
--
-- - Returns: The full name for the identity.
--
-- ObjC selector: @- fullName@
fullName :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
fullName cbIdentity =
  sendMessage cbIdentity fullNameSelector

-- | Returns the POSIX name of the identity.
--
-- The POSIX name is also referred to as the “short name” for an identity. It can only contain the characters A-Z, a-z, 0-9, -, _, ., and .
--
-- - Returns: The POSIX name of the identity.
--
-- ObjC selector: @- posixName@
posixName :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
posixName cbIdentity =
  sendMessage cbIdentity posixNameSelector

-- | Returns an array of aliases (alternate names) for the identity.
--
-- An identity can have zero or more aliases. Like the full and short names, two identities cannot share an alias.
--
-- - Returns: An array of @NSString@ objects containing the alternate names for the identity.
--
-- ObjC selector: @- aliases@
aliases :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSArray)
aliases cbIdentity =
  sendMessage cbIdentity aliasesSelector

-- | Returns the email address of an identity.
--
-- - Returns: The email address of an identity or @nil@ if none exists.
--
-- ObjC selector: @- emailAddress@
emailAddress :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
emailAddress cbIdentity =
  sendMessage cbIdentity emailAddressSelector

-- | Returns the image associated with an identity.
--
-- - Returns: The image associated with an identity, or @nil@ if none exists.
--
-- ObjC selector: @- image@
image :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSImage)
image cbIdentity =
  sendMessage cbIdentity imageSelector

-- | Returns a persistent reference to store a reference to an identity.
--
-- A persistent reference data object is an object generated from an identity. Persistent data objects can be written to and read from a file, making them extremely useful for storing identities in an ACL.
--
-- - Returns: A data object that uniquely references an identity.
--
-- ObjC selector: @- persistentReference@
persistentReference :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSData)
persistentReference cbIdentity =
  sendMessage cbIdentity persistentReferenceSelector

-- | Returns a Boolean value indicating the state of the identity’s hidden property.
--
-- A hidden identity does not show up in the Identity Picker. A hidden identity refers to system identities such as @root@, @www@, and @wheel@.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the identity is hidden; <doc://com.apple.documentation/documentation/objectivec/no> if it is not.
--
-- ObjC selector: @- hidden@
hidden :: IsCBIdentity cbIdentity => cbIdentity -> IO Bool
hidden cbIdentity =
  sendMessage cbIdentity hiddenSelector

-- | Returns an opaque object for use with the Core Services Identity API.
--
-- This method, along with ``CBIdentity/identityWithCSIdentity:``, is used for interoperability with the Core Services Identity API.
--
-- - Returns: The opaque object for use with the Core Services Identity API.
--
-- ObjC selector: @- CSIdentity@
csIdentity :: IsCBIdentity cbIdentity => cbIdentity -> IO (Ptr ())
csIdentity cbIdentity =
  sendMessage cbIdentity csIdentitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identityWithName:authority:@
identityWithName_authoritySelector :: Selector '[Id NSString, Id CBIdentityAuthority] (Id CBIdentity)
identityWithName_authoritySelector = mkSelector "identityWithName:authority:"

-- | @Selector@ for @identityWithUniqueIdentifier:authority:@
identityWithUniqueIdentifier_authoritySelector :: Selector '[Id NSUUID, Id CBIdentityAuthority] (Id CBIdentity)
identityWithUniqueIdentifier_authoritySelector = mkSelector "identityWithUniqueIdentifier:authority:"

-- | @Selector@ for @identityWithUUIDString:authority:@
identityWithUUIDString_authoritySelector :: Selector '[Id NSString, Id CBIdentityAuthority] (Id CBIdentity)
identityWithUUIDString_authoritySelector = mkSelector "identityWithUUIDString:authority:"

-- | @Selector@ for @identityWithPersistentReference:@
identityWithPersistentReferenceSelector :: Selector '[Id NSData] (Id CBIdentity)
identityWithPersistentReferenceSelector = mkSelector "identityWithPersistentReference:"

-- | @Selector@ for @identityWithCSIdentity:@
identityWithCSIdentitySelector :: Selector '[Ptr ()] (Id CBIdentity)
identityWithCSIdentitySelector = mkSelector "identityWithCSIdentity:"

-- | @Selector@ for @isMemberOfGroup:@
isMemberOfGroupSelector :: Selector '[Id CBGroupIdentity] Bool
isMemberOfGroupSelector = mkSelector "isMemberOfGroup:"

-- | @Selector@ for @authority@
authoritySelector :: Selector '[] (Id CBIdentityAuthority)
authoritySelector = mkSelector "authority"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] RawId
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector '[] RawId
uuidStringSelector = mkSelector "UUIDString"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector '[] (Id NSString)
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @posixName@
posixNameSelector :: Selector '[] (Id NSString)
posixNameSelector = mkSelector "posixName"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector '[] (Id NSArray)
aliasesSelector = mkSelector "aliases"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @persistentReference@
persistentReferenceSelector :: Selector '[] (Id NSData)
persistentReferenceSelector = mkSelector "persistentReference"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @CSIdentity@
csIdentitySelector :: Selector '[] (Ptr ())
csIdentitySelector = mkSelector "CSIdentity"

