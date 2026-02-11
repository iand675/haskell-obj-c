{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An identity authority is a database that stores information about identities. The @CBIdentityAuthority@ class defines one or more identity authorities. This database can be searched for identities in conjunction with the @CBIdentity@ class factory methods.
--
-- Generated bindings for @CBIdentityAuthority@.
module ObjC.Collaboration.CBIdentityAuthority
  ( CBIdentityAuthority
  , IsCBIdentityAuthority(..)
  , localIdentityAuthority
  , managedIdentityAuthority
  , defaultIdentityAuthority
  , identityAuthorityWithCSIdentityAuthority
  , csIdentityAuthority
  , localizedName
  , localIdentityAuthoritySelector
  , managedIdentityAuthoritySelector
  , defaultIdentityAuthoritySelector
  , identityAuthorityWithCSIdentityAuthoritySelector
  , csIdentityAuthoritySelector
  , localizedNameSelector


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
import ObjC.Foundation.Internal.Classes

-- | Returns the identity authority on the local system.
--
-- Any identities stored on the local system are contained within this identity authority.
--
-- - Returns: The identity authority on the local system.
--
-- ObjC selector: @+ localIdentityAuthority@
localIdentityAuthority :: IO (Id CBIdentityAuthority)
localIdentityAuthority  =
  do
    cls' <- getRequiredClass "CBIdentityAuthority"
    sendClassMsg cls' (mkSelector "localIdentityAuthority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the identity authority that contains all the identities in bound network directory servers.
--
-- If you are bound to a network directory server (such as an LDAP server) that has an identity authority, use this method to search those authorities.
--
-- - Returns: The identity authorities in bound network directory servers.
--
-- ObjC selector: @+ managedIdentityAuthority@
managedIdentityAuthority :: IO (Id CBIdentityAuthority)
managedIdentityAuthority  =
  do
    cls' <- getRequiredClass "CBIdentityAuthority"
    sendClassMsg cls' (mkSelector "managedIdentityAuthority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an identity authority that contains the identities in both the local and the network-bound authorities.
--
-- The default identity authority is the logical union of the identities in the local and managed authorities.
--
-- - Returns: The local and network-bound identity authorities.
--
-- ObjC selector: @+ defaultIdentityAuthority@
defaultIdentityAuthority :: IO (Id CBIdentityAuthority)
defaultIdentityAuthority  =
  do
    cls' <- getRequiredClass "CBIdentityAuthority"
    sendClassMsg cls' (mkSelector "defaultIdentityAuthority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an identity authority specified by a given Core Services Identity authority object.
--
-- This method, along with ``CBIdentityAuthority/CSIdentityAuthority``, is used for interoperability with the Core Services Identity API.
--
-- - Parameters:   - CSIdentityAuthority: The Core Services Identity opaque object.
--
-- - Returns: The identity authority object for use with the Collaboration framework.
--
-- ObjC selector: @+ identityAuthorityWithCSIdentityAuthority:@
identityAuthorityWithCSIdentityAuthority :: Ptr () -> IO (Id CBIdentityAuthority)
identityAuthorityWithCSIdentityAuthority csIdentityAuthority =
  do
    cls' <- getRequiredClass "CBIdentityAuthority"
    sendClassMsg cls' (mkSelector "identityAuthorityWithCSIdentityAuthority:") (retPtr retVoid) [argPtr csIdentityAuthority] >>= retainedObject . castPtr

-- | Returns an identity authority for use with the Core Services Identity API.
--
-- This method, along with ``CBIdentityAuthority/identityAuthorityWithCSIdentityAuthority:``, is used for interoperability with the Core Services Identity API.
--
-- - Returns: The opaque authority object for use with the Core Services Identity API.
--
-- ObjC selector: @- CSIdentityAuthority@
csIdentityAuthority :: IsCBIdentityAuthority cbIdentityAuthority => cbIdentityAuthority -> IO (Ptr ())
csIdentityAuthority cbIdentityAuthority  =
  fmap castPtr $ sendMsg cbIdentityAuthority (mkSelector "CSIdentityAuthority") (retPtr retVoid) []

-- | Returns the localized name of the identity authority.
--
-- - Returns: The computer’s name if the authority is local, or Managed Network Directory if the authority is managed.
--
-- ObjC selector: @- localizedName@
localizedName :: IsCBIdentityAuthority cbIdentityAuthority => cbIdentityAuthority -> IO (Id NSString)
localizedName cbIdentityAuthority  =
  sendMsg cbIdentityAuthority (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localIdentityAuthority@
localIdentityAuthoritySelector :: Selector
localIdentityAuthoritySelector = mkSelector "localIdentityAuthority"

-- | @Selector@ for @managedIdentityAuthority@
managedIdentityAuthoritySelector :: Selector
managedIdentityAuthoritySelector = mkSelector "managedIdentityAuthority"

-- | @Selector@ for @defaultIdentityAuthority@
defaultIdentityAuthoritySelector :: Selector
defaultIdentityAuthoritySelector = mkSelector "defaultIdentityAuthority"

-- | @Selector@ for @identityAuthorityWithCSIdentityAuthority:@
identityAuthorityWithCSIdentityAuthoritySelector :: Selector
identityAuthorityWithCSIdentityAuthoritySelector = mkSelector "identityAuthorityWithCSIdentityAuthority:"

-- | @Selector@ for @CSIdentityAuthority@
csIdentityAuthoritySelector :: Selector
csIdentityAuthoritySelector = mkSelector "CSIdentityAuthority"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

