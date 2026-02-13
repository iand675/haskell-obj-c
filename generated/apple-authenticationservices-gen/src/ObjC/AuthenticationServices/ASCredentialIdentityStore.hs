{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , getCredentialIdentityStoreStateWithCompletionSelector
  , initSelector
  , removeAllCredentialIdentitiesWithCompletionSelector
  , removeCredentialIdentities_completionSelector
  , removeCredentialIdentityEntries_completionSelector
  , replaceCredentialIdentitiesWithIdentities_completionSelector
  , replaceCredentialIdentityEntries_completionSelector
  , saveCredentialIdentities_completionSelector
  , saveCredentialIdentityEntries_completionSelector
  , sharedStoreSelector

  -- * Enum types
  , ASCredentialIdentityTypes(ASCredentialIdentityTypes)
  , pattern ASCredentialIdentityTypesAll
  , pattern ASCredentialIdentityTypesPassword
  , pattern ASCredentialIdentityTypesPasskey
  , pattern ASCredentialIdentityTypesOneTimeCode

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> IO (Id ASCredentialIdentityStore)
init_ asCredentialIdentityStore =
  sendOwnedMessage asCredentialIdentityStore initSelector

-- | Get the state of the credential identity store.
--
-- @completion@ — completion handler to be called with the current state of the store.
--
-- Call this method to find out the current state of the store before attempting to call other store methods. Use the provided ASCredentialIdentityStoreState to find out if the store is enabled and whether it supports incremental updates.
--
-- ObjC selector: @- getCredentialIdentityStoreStateWithCompletion:@
getCredentialIdentityStoreStateWithCompletion :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> Ptr () -> IO ()
getCredentialIdentityStoreStateWithCompletion asCredentialIdentityStore completion =
  sendMessage asCredentialIdentityStore getCredentialIdentityStoreStateWithCompletionSelector completion

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
saveCredentialIdentities_completion asCredentialIdentityStore credentialIdentities completion =
  sendMessage asCredentialIdentityStore saveCredentialIdentities_completionSelector (toNSArray credentialIdentities) completion

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
saveCredentialIdentityEntries_completion asCredentialIdentityStore credentialIdentities completion =
  sendMessage asCredentialIdentityStore saveCredentialIdentityEntries_completionSelector (toNSArray credentialIdentities) completion

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
removeCredentialIdentities_completion asCredentialIdentityStore credentialIdentities completion =
  sendMessage asCredentialIdentityStore removeCredentialIdentities_completionSelector (toNSArray credentialIdentities) completion

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
removeCredentialIdentityEntries_completion asCredentialIdentityStore credentialIdentities completion =
  sendMessage asCredentialIdentityStore removeCredentialIdentityEntries_completionSelector (toNSArray credentialIdentities) completion

-- | Remove all existing credential identities from the store.
--
-- @completion@ — optional completion handler to be called after removing all existing credential identities. If the operation fails, an error with domain ASCredentialIdentityStoreErrorDomain will be provided and none of the existing credential identities will be removed from the store.
--
-- ObjC selector: @- removeAllCredentialIdentitiesWithCompletion:@
removeAllCredentialIdentitiesWithCompletion :: IsASCredentialIdentityStore asCredentialIdentityStore => asCredentialIdentityStore -> Ptr () -> IO ()
removeAllCredentialIdentitiesWithCompletion asCredentialIdentityStore completion =
  sendMessage asCredentialIdentityStore removeAllCredentialIdentitiesWithCompletionSelector completion

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
replaceCredentialIdentitiesWithIdentities_completion asCredentialIdentityStore newCredentialIdentities completion =
  sendMessage asCredentialIdentityStore replaceCredentialIdentitiesWithIdentities_completionSelector (toNSArray newCredentialIdentities) completion

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
replaceCredentialIdentityEntries_completion asCredentialIdentityStore newCredentialIdentities completion =
  sendMessage asCredentialIdentityStore replaceCredentialIdentityEntries_completionSelector (toNSArray newCredentialIdentities) completion

-- | @+ sharedStore@
sharedStore :: IO (Id ASCredentialIdentityStore)
sharedStore  =
  do
    cls' <- getRequiredClass "ASCredentialIdentityStore"
    sendClassMessage cls' sharedStoreSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASCredentialIdentityStore)
initSelector = mkSelector "init"

-- | @Selector@ for @getCredentialIdentityStoreStateWithCompletion:@
getCredentialIdentityStoreStateWithCompletionSelector :: Selector '[Ptr ()] ()
getCredentialIdentityStoreStateWithCompletionSelector = mkSelector "getCredentialIdentityStoreStateWithCompletion:"

-- | @Selector@ for @saveCredentialIdentities:completion:@
saveCredentialIdentities_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
saveCredentialIdentities_completionSelector = mkSelector "saveCredentialIdentities:completion:"

-- | @Selector@ for @saveCredentialIdentityEntries:completion:@
saveCredentialIdentityEntries_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
saveCredentialIdentityEntries_completionSelector = mkSelector "saveCredentialIdentityEntries:completion:"

-- | @Selector@ for @removeCredentialIdentities:completion:@
removeCredentialIdentities_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
removeCredentialIdentities_completionSelector = mkSelector "removeCredentialIdentities:completion:"

-- | @Selector@ for @removeCredentialIdentityEntries:completion:@
removeCredentialIdentityEntries_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
removeCredentialIdentityEntries_completionSelector = mkSelector "removeCredentialIdentityEntries:completion:"

-- | @Selector@ for @removeAllCredentialIdentitiesWithCompletion:@
removeAllCredentialIdentitiesWithCompletionSelector :: Selector '[Ptr ()] ()
removeAllCredentialIdentitiesWithCompletionSelector = mkSelector "removeAllCredentialIdentitiesWithCompletion:"

-- | @Selector@ for @replaceCredentialIdentitiesWithIdentities:completion:@
replaceCredentialIdentitiesWithIdentities_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
replaceCredentialIdentitiesWithIdentities_completionSelector = mkSelector "replaceCredentialIdentitiesWithIdentities:completion:"

-- | @Selector@ for @replaceCredentialIdentityEntries:completion:@
replaceCredentialIdentityEntries_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
replaceCredentialIdentityEntries_completionSelector = mkSelector "replaceCredentialIdentityEntries:completion:"

-- | @Selector@ for @sharedStore@
sharedStoreSelector :: Selector '[] (Id ASCredentialIdentityStore)
sharedStoreSelector = mkSelector "sharedStore"

