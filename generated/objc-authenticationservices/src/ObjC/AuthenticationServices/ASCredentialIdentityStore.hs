{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASCredentialIdentityStore@.
module ObjC.AuthenticationServices.ASCredentialIdentityStore
  ( ASCredentialIdentityStore
  , IsASCredentialIdentityStore(..)
  , init_
  , getCredentialIdentityStoreStateWithCompletion
  , saveCredentialIdentities_completion
  , saveCredentialIdentityEntries_completion
  , removeCredentialIdentities_completion
  , removeCredentialIdentityEntries_completion
  , removeAllCredentialIdentitiesWithCompletion
  , replaceCredentialIdentitiesWithIdentities_completion
  , replaceCredentialIdentityEntries_completion
  , sharedStore
  , initSelector
  , getCredentialIdentityStoreStateWithCompletionSelector
  , saveCredentialIdentities_completionSelector
  , saveCredentialIdentityEntries_completionSelector
  , removeCredentialIdentities_completionSelector
  , removeCredentialIdentityEntries_completionSelector
  , removeAllCredentialIdentitiesWithCompletionSelector
  , replaceCredentialIdentitiesWithIdentities_completionSelector
  , replaceCredentialIdentityEntries_completionSelector
  , sharedStoreSelector

  -- * Enum types
  , ASCredentialIdentityTypes(ASCredentialIdentityTypes)
  , pattern ASCredentialIdentityTypesAll
  , pattern ASCredentialIdentityTypesPassword
  , pattern ASCredentialIdentityTypesPasskey
  , pattern ASCredentialIdentityTypesOneTimeCode

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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> IO (Id ASCredentialIdentityStore)
init_ asCredentialIdentityStore  =
  sendMsg asCredentialIdentityStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get the state of the credential identity store.
--
-- @completion@ — completion handler to be called with the current state of the store.
--
-- Call this method to find out the current state of the store before attempting to call other store methods. Use the provided ASCredentialIdentityStoreState to find out if the store is enabled and whether it supports incremental updates.
--
-- ObjC selector: @- getCredentialIdentityStoreStateWithCompletion:@
getCredentialIdentityStoreStateWithCompletion :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> Ptr () -> IO ()
getCredentialIdentityStoreStateWithCompletion asCredentialIdentityStore  completion =
  sendMsg asCredentialIdentityStore (mkSelector "getCredentialIdentityStoreStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Save the given credential identities to the store.
--
-- @credentialIdentities@ — array of ASPasswordCredentialIdentity objects to save to the store.
--
-- @completion@ — optional completion handler to be called after adding the credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the objects in credentialIdentities will be saved to the store.
--
-- If the store supports incremental updates, call this method to add new credential identities since the last time the store was updated. Otherwise, call this method to pass all credential identities. If some credential identities in credentialIdentities already exist in the store, they will be replaced by those from credentialIdentities.
--
-- ObjC selector: @- saveCredentialIdentities:completion:@
saveCredentialIdentities_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray credentialIdentities) => asCredentialIdentityStore -> credentialIdentities -> Ptr () -> IO ()
saveCredentialIdentities_completion asCredentialIdentityStore  credentialIdentities completion =
withObjCPtr credentialIdentities $ \raw_credentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "saveCredentialIdentities:completion:") retVoid [argPtr (castPtr raw_credentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Save the given credential identities to the store.
--
-- @credentialIdentities@ — array of ASCredentialIdentity objects to save to the store.
--
-- @completion@ — optional completion handler to be called after adding the credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the objects in credentialIdentities will be saved to the store.
--
-- If the store supports incremental updates, call this method to add new credential identities since the last time the store was updated. Otherwise, call this method to pass all credential identities. If some credential identities in credentialIdentities already exist in the store, they will be replaced by those from credentialIdentities.
--
-- ObjC selector: @- saveCredentialIdentityEntries:completion:@
saveCredentialIdentityEntries_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray credentialIdentities) => asCredentialIdentityStore -> credentialIdentities -> Ptr () -> IO ()
saveCredentialIdentityEntries_completion asCredentialIdentityStore  credentialIdentities completion =
withObjCPtr credentialIdentities $ \raw_credentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "saveCredentialIdentityEntries:completion:") retVoid [argPtr (castPtr raw_credentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Remove the given credential identities from the store.
--
-- @credentialIdentities@ — array of ASPasswordCredentialIdentity objects to remove from the store.
--
-- @completion@ — optional completion handler to be called after removing the credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the objects in credentialIdentities will be removed from the store.
--
-- Use this method only if the store supports incremental updates to remove previously added credentials to the store.
--
-- ObjC selector: @- removeCredentialIdentities:completion:@
removeCredentialIdentities_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray credentialIdentities) => asCredentialIdentityStore -> credentialIdentities -> Ptr () -> IO ()
removeCredentialIdentities_completion asCredentialIdentityStore  credentialIdentities completion =
withObjCPtr credentialIdentities $ \raw_credentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "removeCredentialIdentities:completion:") retVoid [argPtr (castPtr raw_credentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Remove the given credential identities from the store.
--
-- @credentialIdentities@ — array of ASCredentialIdentity objects to remove from the store.
--
-- @completion@ — optional completion handler to be called after removing the credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the objects in credentialIdentities will be removed from the store.
--
-- Use this method only if the store supports incremental updates to remove previously added credentials to the store.
--
-- ObjC selector: @- removeCredentialIdentityEntries:completion:@
removeCredentialIdentityEntries_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray credentialIdentities) => asCredentialIdentityStore -> credentialIdentities -> Ptr () -> IO ()
removeCredentialIdentityEntries_completion asCredentialIdentityStore  credentialIdentities completion =
withObjCPtr credentialIdentities $ \raw_credentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "removeCredentialIdentityEntries:completion:") retVoid [argPtr (castPtr raw_credentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Remove all existing credential identities from the store.
--
-- @completion@ — optional completion handler to be called after removing all existing credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the existing credential identities will be removed from the store.
--
-- ObjC selector: @- removeAllCredentialIdentitiesWithCompletion:@
removeAllCredentialIdentitiesWithCompletion :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> Ptr () -> IO ()
removeAllCredentialIdentitiesWithCompletion asCredentialIdentityStore  completion =
  sendMsg asCredentialIdentityStore (mkSelector "removeAllCredentialIdentitiesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Replace existing credential identities with new credential identities.
--
-- @newCredentialIdentities@ — array of new credential identity objects to replace the old ones.
--
-- @completion@ — an optional completion block to be called after the operation is finished.
--
-- This method will delete all existing credential identities that are persisted in the store and replace them with the provided array of credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the new credential identities will be saved.
--
-- ObjC selector: @- replaceCredentialIdentitiesWithIdentities:completion:@
replaceCredentialIdentitiesWithIdentities_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray newCredentialIdentities) => asCredentialIdentityStore -> newCredentialIdentities -> Ptr () -> IO ()
replaceCredentialIdentitiesWithIdentities_completion asCredentialIdentityStore  newCredentialIdentities completion =
withObjCPtr newCredentialIdentities $ \raw_newCredentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "replaceCredentialIdentitiesWithIdentities:completion:") retVoid [argPtr (castPtr raw_newCredentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Replace existing credential identities with new credential identities.
--
-- @newCredentialIdentities@ — array of new credential identity objects to replace the old ones.
--
-- @completion@ — an optional completion block to be called after the operation is finished.
--
-- This method will delete all existing credential identities that are persisted in the store and replace them with the provided array of credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the new credential identities will be saved.
--
-- ObjC selector: @- replaceCredentialIdentityEntries:completion:@
replaceCredentialIdentityEntries_completion :: (IsASCredentialIdentityStore asCredentialIdentityStore, IsNSArray newCredentialIdentities) => asCredentialIdentityStore -> newCredentialIdentities -> Ptr () -> IO ()
replaceCredentialIdentityEntries_completion asCredentialIdentityStore  newCredentialIdentities completion =
withObjCPtr newCredentialIdentities $ \raw_newCredentialIdentities ->
    sendMsg asCredentialIdentityStore (mkSelector "replaceCredentialIdentityEntries:completion:") retVoid [argPtr (castPtr raw_newCredentialIdentities :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ sharedStore@
sharedStore :: IO (Id ASCredentialIdentityStore)
sharedStore  =
  do
    cls' <- getRequiredClass "ASCredentialIdentityStore"
    sendClassMsg cls' (mkSelector "sharedStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getCredentialIdentityStoreStateWithCompletion:@
getCredentialIdentityStoreStateWithCompletionSelector :: Selector
getCredentialIdentityStoreStateWithCompletionSelector = mkSelector "getCredentialIdentityStoreStateWithCompletion:"

-- | @Selector@ for @saveCredentialIdentities:completion:@
saveCredentialIdentities_completionSelector :: Selector
saveCredentialIdentities_completionSelector = mkSelector "saveCredentialIdentities:completion:"

-- | @Selector@ for @saveCredentialIdentityEntries:completion:@
saveCredentialIdentityEntries_completionSelector :: Selector
saveCredentialIdentityEntries_completionSelector = mkSelector "saveCredentialIdentityEntries:completion:"

-- | @Selector@ for @removeCredentialIdentities:completion:@
removeCredentialIdentities_completionSelector :: Selector
removeCredentialIdentities_completionSelector = mkSelector "removeCredentialIdentities:completion:"

-- | @Selector@ for @removeCredentialIdentityEntries:completion:@
removeCredentialIdentityEntries_completionSelector :: Selector
removeCredentialIdentityEntries_completionSelector = mkSelector "removeCredentialIdentityEntries:completion:"

-- | @Selector@ for @removeAllCredentialIdentitiesWithCompletion:@
removeAllCredentialIdentitiesWithCompletionSelector :: Selector
removeAllCredentialIdentitiesWithCompletionSelector = mkSelector "removeAllCredentialIdentitiesWithCompletion:"

-- | @Selector@ for @replaceCredentialIdentitiesWithIdentities:completion:@
replaceCredentialIdentitiesWithIdentities_completionSelector :: Selector
replaceCredentialIdentitiesWithIdentities_completionSelector = mkSelector "replaceCredentialIdentitiesWithIdentities:completion:"

-- | @Selector@ for @replaceCredentialIdentityEntries:completion:@
replaceCredentialIdentityEntries_completionSelector :: Selector
replaceCredentialIdentityEntries_completionSelector = mkSelector "replaceCredentialIdentityEntries:completion:"

-- | @Selector@ for @sharedStore@
sharedStoreSelector :: Selector
sharedStoreSelector = mkSelector "sharedStore"

