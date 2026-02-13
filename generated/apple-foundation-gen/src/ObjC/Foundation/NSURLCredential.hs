{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLCredential
--
-- This class is an immutable object representing an authentication credential.  The actual type of the credential is determined by the constructor called in the categories declared below.
--
-- Generated bindings for @NSURLCredential@.
module ObjC.Foundation.NSURLCredential
  ( NSURLCredential
  , IsNSURLCredential(..)
  , initWithTrust
  , credentialForTrust
  , initWithIdentity_certificates_persistence
  , credentialWithIdentity_certificates_persistence
  , initWithUser_password_persistence
  , credentialWithUser_password_persistence
  , persistence
  , identity
  , certificates
  , user
  , password
  , hasPassword
  , certificatesSelector
  , credentialForTrustSelector
  , credentialWithIdentity_certificates_persistenceSelector
  , credentialWithUser_password_persistenceSelector
  , hasPasswordSelector
  , identitySelector
  , initWithIdentity_certificates_persistenceSelector
  , initWithTrustSelector
  , initWithUser_password_persistenceSelector
  , passwordSelector
  , persistenceSelector
  , userSelector

  -- * Enum types
  , NSURLCredentialPersistence(NSURLCredentialPersistence)
  , pattern NSURLCredentialPersistenceNone
  , pattern NSURLCredentialPersistenceForSession
  , pattern NSURLCredentialPersistencePermanent
  , pattern NSURLCredentialPersistenceSynchronizable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | initWithTrust:
--
-- Initialize a new NSURLCredential which specifies that the specified trust has been accepted.
--
-- Returns: the Initialized NSURLCredential
--
-- ObjC selector: @- initWithTrust:@
initWithTrust :: IsNSURLCredential nsurlCredential => nsurlCredential -> Ptr () -> IO (Id NSURLCredential)
initWithTrust nsurlCredential trust =
  sendOwnedMessage nsurlCredential initWithTrustSelector trust

-- | credentialForTrust:
--
-- Create a new NSURLCredential which specifies that a handshake has been trusted.
--
-- Returns: The new autoreleased NSURLCredential
--
-- ObjC selector: @+ credentialForTrust:@
credentialForTrust :: Ptr () -> IO (Id NSURLCredential)
credentialForTrust trust =
  do
    cls' <- getRequiredClass "NSURLCredential"
    sendClassMessage cls' credentialForTrustSelector trust

-- | initWithIdentity:certificates:persistence:
--
-- Initialize an NSURLCredential with an identity and array of at least 1 client certificates (SecCertificateRef)
--
-- @identity@ — a SecIdentityRef object
--
-- @certArray@ — an array containing at least one SecCertificateRef objects
--
-- @persistence@ — enum that says to store per session, permanently or not at all
--
-- Returns: the Initialized NSURLCredential
--
-- ObjC selector: @- initWithIdentity:certificates:persistence:@
initWithIdentity_certificates_persistence :: (IsNSURLCredential nsurlCredential, IsNSArray certArray) => nsurlCredential -> Ptr () -> certArray -> NSURLCredentialPersistence -> IO (Id NSURLCredential)
initWithIdentity_certificates_persistence nsurlCredential identity certArray persistence =
  sendOwnedMessage nsurlCredential initWithIdentity_certificates_persistenceSelector identity (toNSArray certArray) persistence

-- | credentialWithIdentity:certificates:persistence:
--
-- Create a new NSURLCredential with an identity and certificate array
--
-- @identity@ — a SecIdentityRef object
--
-- @certArray@ — an array containing at least one SecCertificateRef objects
--
-- @persistence@ — enum that says to store per session, permanently or not at all
--
-- Returns: The new autoreleased NSURLCredential
--
-- ObjC selector: @+ credentialWithIdentity:certificates:persistence:@
credentialWithIdentity_certificates_persistence :: IsNSArray certArray => Ptr () -> certArray -> NSURLCredentialPersistence -> IO (Id NSURLCredential)
credentialWithIdentity_certificates_persistence identity certArray persistence =
  do
    cls' <- getRequiredClass "NSURLCredential"
    sendClassMessage cls' credentialWithIdentity_certificates_persistenceSelector identity (toNSArray certArray) persistence

-- | initWithUser:password:persistence:
--
-- Initialize a NSURLCredential with a user and password
--
-- @user@ — the username
--
-- @password@ — the password
--
-- @persistence@ — enum that says to store per session, permanently or not at all
--
-- Returns: The initialized NSURLCredential
--
-- ObjC selector: @- initWithUser:password:persistence:@
initWithUser_password_persistence :: (IsNSURLCredential nsurlCredential, IsNSString user, IsNSString password) => nsurlCredential -> user -> password -> NSURLCredentialPersistence -> IO (Id NSURLCredential)
initWithUser_password_persistence nsurlCredential user password persistence =
  sendOwnedMessage nsurlCredential initWithUser_password_persistenceSelector (toNSString user) (toNSString password) persistence

-- | credentialWithUser:password:persistence:
--
-- Create a new NSURLCredential with a user and password
--
-- @user@ — the username
--
-- @password@ — the password
--
-- @persistence@ — enum that says to store per session, permanently or not at all
--
-- Returns: The new autoreleased NSURLCredential
--
-- ObjC selector: @+ credentialWithUser:password:persistence:@
credentialWithUser_password_persistence :: (IsNSString user, IsNSString password) => user -> password -> NSURLCredentialPersistence -> IO (Id NSURLCredential)
credentialWithUser_password_persistence user password persistence =
  do
    cls' <- getRequiredClass "NSURLCredential"
    sendClassMessage cls' credentialWithUser_password_persistenceSelector (toNSString user) (toNSString password) persistence

-- | Determine whether this credential is or should be stored persistently
--
-- Returns: A value indicating whether this credential is stored permanently, per session or not at all.
--
-- ObjC selector: @- persistence@
persistence :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO NSURLCredentialPersistence
persistence nsurlCredential =
  sendMessage nsurlCredential persistenceSelector

-- | Returns the SecIdentityRef of this credential, if it was created with a certificate and identity
--
-- Returns: A SecIdentityRef or NULL if this is a username/password credential
--
-- ObjC selector: @- identity@
identity :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO (Ptr ())
identity nsurlCredential =
  sendMessage nsurlCredential identitySelector

-- | Returns an NSArray of SecCertificateRef objects representing the client certificate for this credential, if this credential was created with an identity and certificate.
--
-- Returns: an NSArray of SecCertificateRef or NULL if this is a username/password credential
--
-- ObjC selector: @- certificates@
certificates :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO (Id NSArray)
certificates nsurlCredential =
  sendMessage nsurlCredential certificatesSelector

-- | Get the username
--
-- Returns: The user string
--
-- ObjC selector: @- user@
user :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO (Id NSString)
user nsurlCredential =
  sendMessage nsurlCredential userSelector

-- | Get the password
--
-- Returns: The password string
--
-- This method might actually attempt to retrieve the    password from an external store, possible resulting in prompting,    so do not call it unless needed.
--
-- ObjC selector: @- password@
password :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO (Id NSString)
password nsurlCredential =
  sendMessage nsurlCredential passwordSelector

-- | Find out if this credential has a password, without trying to get it
--
-- Returns: YES if this credential has a password, otherwise NO
--
-- If this credential's password is actually kept in an    external store, the password method may return nil even if this    method returns YES, since getting the password may fail, or the    user may refuse access.
--
-- ObjC selector: @- hasPassword@
hasPassword :: IsNSURLCredential nsurlCredential => nsurlCredential -> IO Bool
hasPassword nsurlCredential =
  sendMessage nsurlCredential hasPasswordSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTrust:@
initWithTrustSelector :: Selector '[Ptr ()] (Id NSURLCredential)
initWithTrustSelector = mkSelector "initWithTrust:"

-- | @Selector@ for @credentialForTrust:@
credentialForTrustSelector :: Selector '[Ptr ()] (Id NSURLCredential)
credentialForTrustSelector = mkSelector "credentialForTrust:"

-- | @Selector@ for @initWithIdentity:certificates:persistence:@
initWithIdentity_certificates_persistenceSelector :: Selector '[Ptr (), Id NSArray, NSURLCredentialPersistence] (Id NSURLCredential)
initWithIdentity_certificates_persistenceSelector = mkSelector "initWithIdentity:certificates:persistence:"

-- | @Selector@ for @credentialWithIdentity:certificates:persistence:@
credentialWithIdentity_certificates_persistenceSelector :: Selector '[Ptr (), Id NSArray, NSURLCredentialPersistence] (Id NSURLCredential)
credentialWithIdentity_certificates_persistenceSelector = mkSelector "credentialWithIdentity:certificates:persistence:"

-- | @Selector@ for @initWithUser:password:persistence:@
initWithUser_password_persistenceSelector :: Selector '[Id NSString, Id NSString, NSURLCredentialPersistence] (Id NSURLCredential)
initWithUser_password_persistenceSelector = mkSelector "initWithUser:password:persistence:"

-- | @Selector@ for @credentialWithUser:password:persistence:@
credentialWithUser_password_persistenceSelector :: Selector '[Id NSString, Id NSString, NSURLCredentialPersistence] (Id NSURLCredential)
credentialWithUser_password_persistenceSelector = mkSelector "credentialWithUser:password:persistence:"

-- | @Selector@ for @persistence@
persistenceSelector :: Selector '[] NSURLCredentialPersistence
persistenceSelector = mkSelector "persistence"

-- | @Selector@ for @identity@
identitySelector :: Selector '[] (Ptr ())
identitySelector = mkSelector "identity"

-- | @Selector@ for @certificates@
certificatesSelector :: Selector '[] (Id NSArray)
certificatesSelector = mkSelector "certificates"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @hasPassword@
hasPasswordSelector :: Selector '[] Bool
hasPasswordSelector = mkSelector "hasPassword"

