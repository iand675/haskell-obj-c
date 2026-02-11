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
  , fullName
  , posixName
  , aliases
  , emailAddress
  , image
  , persistentReference
  , hidden
  , csIdentity
  , identityWithName_authoritySelector
  , identityWithUniqueIdentifier_authoritySelector
  , identityWithUUIDString_authoritySelector
  , identityWithPersistentReferenceSelector
  , identityWithCSIdentitySelector
  , isMemberOfGroupSelector
  , authoritySelector
  , fullNameSelector
  , posixNameSelector
  , aliasesSelector
  , emailAddressSelector
  , imageSelector
  , persistentReferenceSelector
  , hiddenSelector
  , csIdentitySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    withObjCPtr name $ \raw_name ->
      withObjCPtr authority $ \raw_authority ->
        sendClassMsg cls' (mkSelector "identityWithName:authority:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_authority :: Ptr ())] >>= retainedObject . castPtr

-- | @+ identityWithUniqueIdentifier:authority:@
identityWithUniqueIdentifier_authority :: (IsNSUUID uuid, IsCBIdentityAuthority authority) => uuid -> authority -> IO (Id CBIdentity)
identityWithUniqueIdentifier_authority uuid authority =
  do
    cls' <- getRequiredClass "CBIdentity"
    withObjCPtr uuid $ \raw_uuid ->
      withObjCPtr authority $ \raw_authority ->
        sendClassMsg cls' (mkSelector "identityWithUniqueIdentifier:authority:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_authority :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr uuid $ \raw_uuid ->
      withObjCPtr authority $ \raw_authority ->
        sendClassMsg cls' (mkSelector "identityWithUUIDString:authority:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_authority :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "identityWithPersistentReference:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "identityWithCSIdentity:") (retPtr retVoid) [argPtr csIdentity] >>= retainedObject . castPtr

-- | Returns a Boolean value indicating whether the identity is a member of the specified group.
--
-- - Parameters:   - group: The group to check for membership.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the identity is a member of the group; <doc://com.apple.documentation/documentation/objectivec/no> if it is not.
--
-- ObjC selector: @- isMemberOfGroup:@
isMemberOfGroup :: (IsCBIdentity cbIdentity, IsCBGroupIdentity group) => cbIdentity -> group -> IO Bool
isMemberOfGroup cbIdentity  group =
withObjCPtr group $ \raw_group ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbIdentity (mkSelector "isMemberOfGroup:") retCULong [argPtr (castPtr raw_group :: Ptr ())]

-- | Returns the identity authority where the identity is stored.
--
-- - Returns: The identity authority where the identity is stored.
--
-- ObjC selector: @- authority@
authority :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id CBIdentityAuthority)
authority cbIdentity  =
  sendMsg cbIdentity (mkSelector "authority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the full name of the identity.
--
-- - Returns: The full name for the identity.
--
-- ObjC selector: @- fullName@
fullName :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
fullName cbIdentity  =
  sendMsg cbIdentity (mkSelector "fullName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the POSIX name of the identity.
--
-- The POSIX name is also referred to as the “short name” for an identity. It can only contain the characters A-Z, a-z, 0-9, -, _, ., and .
--
-- - Returns: The POSIX name of the identity.
--
-- ObjC selector: @- posixName@
posixName :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
posixName cbIdentity  =
  sendMsg cbIdentity (mkSelector "posixName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an array of aliases (alternate names) for the identity.
--
-- An identity can have zero or more aliases. Like the full and short names, two identities cannot share an alias.
--
-- - Returns: An array of @NSString@ objects containing the alternate names for the identity.
--
-- ObjC selector: @- aliases@
aliases :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSArray)
aliases cbIdentity  =
  sendMsg cbIdentity (mkSelector "aliases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the email address of an identity.
--
-- - Returns: The email address of an identity or @nil@ if none exists.
--
-- ObjC selector: @- emailAddress@
emailAddress :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSString)
emailAddress cbIdentity  =
  sendMsg cbIdentity (mkSelector "emailAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the image associated with an identity.
--
-- - Returns: The image associated with an identity, or @nil@ if none exists.
--
-- ObjC selector: @- image@
image :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSImage)
image cbIdentity  =
  sendMsg cbIdentity (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a persistent reference to store a reference to an identity.
--
-- A persistent reference data object is an object generated from an identity. Persistent data objects can be written to and read from a file, making them extremely useful for storing identities in an ACL.
--
-- - Returns: A data object that uniquely references an identity.
--
-- ObjC selector: @- persistentReference@
persistentReference :: IsCBIdentity cbIdentity => cbIdentity -> IO (Id NSData)
persistentReference cbIdentity  =
  sendMsg cbIdentity (mkSelector "persistentReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a Boolean value indicating the state of the identity’s hidden property.
--
-- A hidden identity does not show up in the Identity Picker. A hidden identity refers to system identities such as @root@, @www@, and @wheel@.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the identity is hidden; <doc://com.apple.documentation/documentation/objectivec/no> if it is not.
--
-- ObjC selector: @- hidden@
hidden :: IsCBIdentity cbIdentity => cbIdentity -> IO Bool
hidden cbIdentity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbIdentity (mkSelector "hidden") retCULong []

-- | Returns an opaque object for use with the Core Services Identity API.
--
-- This method, along with ``CBIdentity/identityWithCSIdentity:``, is used for interoperability with the Core Services Identity API.
--
-- - Returns: The opaque object for use with the Core Services Identity API.
--
-- ObjC selector: @- CSIdentity@
csIdentity :: IsCBIdentity cbIdentity => cbIdentity -> IO (Ptr ())
csIdentity cbIdentity  =
  fmap castPtr $ sendMsg cbIdentity (mkSelector "CSIdentity") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identityWithName:authority:@
identityWithName_authoritySelector :: Selector
identityWithName_authoritySelector = mkSelector "identityWithName:authority:"

-- | @Selector@ for @identityWithUniqueIdentifier:authority:@
identityWithUniqueIdentifier_authoritySelector :: Selector
identityWithUniqueIdentifier_authoritySelector = mkSelector "identityWithUniqueIdentifier:authority:"

-- | @Selector@ for @identityWithUUIDString:authority:@
identityWithUUIDString_authoritySelector :: Selector
identityWithUUIDString_authoritySelector = mkSelector "identityWithUUIDString:authority:"

-- | @Selector@ for @identityWithPersistentReference:@
identityWithPersistentReferenceSelector :: Selector
identityWithPersistentReferenceSelector = mkSelector "identityWithPersistentReference:"

-- | @Selector@ for @identityWithCSIdentity:@
identityWithCSIdentitySelector :: Selector
identityWithCSIdentitySelector = mkSelector "identityWithCSIdentity:"

-- | @Selector@ for @isMemberOfGroup:@
isMemberOfGroupSelector :: Selector
isMemberOfGroupSelector = mkSelector "isMemberOfGroup:"

-- | @Selector@ for @authority@
authoritySelector :: Selector
authoritySelector = mkSelector "authority"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @posixName@
posixNameSelector :: Selector
posixNameSelector = mkSelector "posixName"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector
aliasesSelector = mkSelector "aliases"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @persistentReference@
persistentReferenceSelector :: Selector
persistentReferenceSelector = mkSelector "persistentReference"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @CSIdentity@
csIdentitySelector :: Selector
csIdentitySelector = mkSelector "CSIdentity"

