{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object of the @CBUserIdentity@ class represents a user identity and is used for accessing the attributes of a user identity from an identity authority. The principal attributes of @CBUserIdentity@ are a POSIX user identifier (UID), password, and certificate.
--
-- Generated bindings for @CBUserIdentity@.
module ObjC.Collaboration.CBUserIdentity
  ( CBUserIdentity
  , IsCBUserIdentity(..)
  , userIdentityWithPosixUID_authority
  , authenticateWithPassword
  , posixUID
  , certificate
  , enabled
  , authenticateWithPasswordSelector
  , certificateSelector
  , enabledSelector
  , posixUIDSelector
  , userIdentityWithPosixUID_authoritySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Collaboration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the user identity with the given POSIX UID in the specified identity authority.
--
-- - Parameters:   - uid: The UID of the identity you are searching for.
--
-- - authority: The identity authority to search.
--
-- - Returns: The user identity with the given UID in the specified identity authority, or @nil@ if no identity exists with the specified UID.
--
-- ObjC selector: @+ userIdentityWithPosixUID:authority:@
userIdentityWithPosixUID_authority :: IsCBIdentityAuthority authority => CUInt -> authority -> IO (Id CBUserIdentity)
userIdentityWithPosixUID_authority uid authority =
  do
    cls' <- getRequiredClass "CBUserIdentity"
    sendClassMessage cls' userIdentityWithPosixUID_authoritySelector uid (toCBIdentityAuthority authority)

-- | Returns a Boolean value indicating whether the given password is correct for the identity.
--
-- - Parameters:   - password: The password to test for the identity.
--
-- - Returns: @TRUE@ if the password is correct; otherwise, @FALSE@.
--
-- ObjC selector: @- authenticateWithPassword:@
authenticateWithPassword :: (IsCBUserIdentity cbUserIdentity, IsNSString password) => cbUserIdentity -> password -> IO Bool
authenticateWithPassword cbUserIdentity password =
  sendMessage cbUserIdentity authenticateWithPasswordSelector (toNSString password)

-- | Returns the POSIX UID of the identity.
--
-- The POSIX UID is a integer that can identify a user within an identity authority. UIDs are not guaranteed to be unique within an identity authority.
--
-- - Returns: The POSIX UID of the identity.
--
-- ObjC selector: @- posixUID@
posixUID :: IsCBUserIdentity cbUserIdentity => cbUserIdentity -> IO CUInt
posixUID cbUserIdentity =
  sendMessage cbUserIdentity posixUIDSelector

-- | Returns the public authentication certificate associated with a user identity.
--
-- The Collaboration framework supports certificate-based authentication in addition to passwords. If a certificate is stored for a user identity, it will be the default method of authentication.
--
-- When a .Mac account is associated with a user identity, the authentication certificate is automatically downloaded from the .Mac servers.
--
-- - Returns: The public authentication certificate, or @nil@ if none exists.
--
-- ObjC selector: @- certificate@
certificate :: IsCBUserIdentity cbUserIdentity => cbUserIdentity -> IO (Ptr ())
certificate cbUserIdentity =
  sendMessage cbUserIdentity certificateSelector

-- | Returns a Boolean value indicating whether the identity is allowed to authenticate.
--
-- If the identity does not have authentication credentials (a password or certificate), it is not able to log in. However, an identity with authentication credentials does not ensure that it is enabled. Any identity can be disabled.
--
-- - Returns: @TRUE@ if the identity can authenticate; otherwise, @FALSE@.
--
-- ObjC selector: @- enabled@
enabled :: IsCBUserIdentity cbUserIdentity => cbUserIdentity -> IO Bool
enabled cbUserIdentity =
  sendMessage cbUserIdentity enabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userIdentityWithPosixUID:authority:@
userIdentityWithPosixUID_authoritySelector :: Selector '[CUInt, Id CBIdentityAuthority] (Id CBUserIdentity)
userIdentityWithPosixUID_authoritySelector = mkSelector "userIdentityWithPosixUID:authority:"

-- | @Selector@ for @authenticateWithPassword:@
authenticateWithPasswordSelector :: Selector '[Id NSString] Bool
authenticateWithPasswordSelector = mkSelector "authenticateWithPassword:"

-- | @Selector@ for @posixUID@
posixUIDSelector :: Selector '[] CUInt
posixUIDSelector = mkSelector "posixUID"

-- | @Selector@ for @certificate@
certificateSelector :: Selector '[] (Ptr ())
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

