{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Persistent storage for @LARight@ instances.
--
-- Generated bindings for @LARightStore@.
module ObjC.LocalAuthentication.LARightStore
  ( LARightStore
  , IsLARightStore(..)
  , rightForIdentifier_completion
  , saveRight_identifier_completion
  , saveRight_identifier_secret_completion
  , removeRight_completion
  , removeRightForIdentifier_completion
  , removeAllRightsWithCompletion
  , new
  , init_
  , sharedStore
  , initSelector
  , newSelector
  , removeAllRightsWithCompletionSelector
  , removeRightForIdentifier_completionSelector
  , removeRight_completionSelector
  , rightForIdentifier_completionSelector
  , saveRight_identifier_completionSelector
  , saveRight_identifier_secret_completionSelector
  , sharedStoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Fetches a right stored under the given identifier.
--
-- @identifier@ — Identifier associated with a previously stored right.
--
-- @handler@ — Completion handler with the fetched right or an error on failure.
--
-- ObjC selector: @- rightForIdentifier:completion:@
rightForIdentifier_completion :: (IsLARightStore laRightStore, IsNSString identifier) => laRightStore -> identifier -> Ptr () -> IO ()
rightForIdentifier_completion laRightStore identifier handler =
  sendMessage laRightStore rightForIdentifier_completionSelector (toNSString identifier) handler

-- | Persists a right for later usage.
--
-- @right@ — @LARight@ instance to store.
--
-- @identifier@ — Identifier to be associated with the right. Useful for later retrieval.
--
-- @handler@ — Completion handler with the persisted right or an error on failure.
--
-- ObjC selector: @- saveRight:identifier:completion:@
saveRight_identifier_completion :: (IsLARightStore laRightStore, IsLARight right, IsNSString identifier) => laRightStore -> right -> identifier -> Ptr () -> IO ()
saveRight_identifier_completion laRightStore right identifier handler =
  sendMessage laRightStore saveRight_identifier_completionSelector (toLARight right) (toNSString identifier) handler

-- | Persists a right for later usage.
--
-- @right@ — @LARight@ instance to store.
--
-- @identifier@ — Identifier to be associated with the right. Useful for later retrieval.
--
-- @secret@ — Secret data to be associated with the provided right.
--
-- @handler@ — Completion handler with the persisted right or an error on failure.
--
-- ObjC selector: @- saveRight:identifier:secret:completion:@
saveRight_identifier_secret_completion :: (IsLARightStore laRightStore, IsLARight right, IsNSString identifier, IsNSData secret) => laRightStore -> right -> identifier -> secret -> Ptr () -> IO ()
saveRight_identifier_secret_completion laRightStore right identifier secret handler =
  sendMessage laRightStore saveRight_identifier_secret_completionSelector (toLARight right) (toNSString identifier) (toNSData secret) handler

-- | Removes a right from the persistent storage along with its associated resources.
--
-- @right@ — @LAPersistedRight@ instance to remove.
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeRight:completion:@
removeRight_completion :: (IsLARightStore laRightStore, IsLAPersistedRight right) => laRightStore -> right -> Ptr () -> IO ()
removeRight_completion laRightStore right handler =
  sendMessage laRightStore removeRight_completionSelector (toLAPersistedRight right) handler

-- | Removes right with provided identifier from persistant storage.
--
-- @identifier@ — Identifier of @LAPersistedRight@ instance to remove.
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeRightForIdentifier:completion:@
removeRightForIdentifier_completion :: (IsLARightStore laRightStore, IsNSString identifier) => laRightStore -> identifier -> Ptr () -> IO ()
removeRightForIdentifier_completion laRightStore identifier handler =
  sendMessage laRightStore removeRightForIdentifier_completionSelector (toNSString identifier) handler

-- | Removes all rights stored by the client
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeAllRightsWithCompletion:@
removeAllRightsWithCompletion :: IsLARightStore laRightStore => laRightStore -> Ptr () -> IO ()
removeAllRightsWithCompletion laRightStore handler =
  sendMessage laRightStore removeAllRightsWithCompletionSelector handler

-- | Clients should rely on the @shared@ instance instead
--
-- ObjC selector: @+ new@
new :: IO (Id LARightStore)
new  =
  do
    cls' <- getRequiredClass "LARightStore"
    sendOwnedClassMessage cls' newSelector

-- | Clients should rely on the @shared@ instance instead
--
-- ObjC selector: @- init@
init_ :: IsLARightStore laRightStore => laRightStore -> IO (Id LARightStore)
init_ laRightStore =
  sendOwnedMessage laRightStore initSelector

-- | Shared instance of @LARightStore.@
--
-- ObjC selector: @+ sharedStore@
sharedStore :: IO (Id LARightStore)
sharedStore  =
  do
    cls' <- getRequiredClass "LARightStore"
    sendClassMessage cls' sharedStoreSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rightForIdentifier:completion:@
rightForIdentifier_completionSelector :: Selector '[Id NSString, Ptr ()] ()
rightForIdentifier_completionSelector = mkSelector "rightForIdentifier:completion:"

-- | @Selector@ for @saveRight:identifier:completion:@
saveRight_identifier_completionSelector :: Selector '[Id LARight, Id NSString, Ptr ()] ()
saveRight_identifier_completionSelector = mkSelector "saveRight:identifier:completion:"

-- | @Selector@ for @saveRight:identifier:secret:completion:@
saveRight_identifier_secret_completionSelector :: Selector '[Id LARight, Id NSString, Id NSData, Ptr ()] ()
saveRight_identifier_secret_completionSelector = mkSelector "saveRight:identifier:secret:completion:"

-- | @Selector@ for @removeRight:completion:@
removeRight_completionSelector :: Selector '[Id LAPersistedRight, Ptr ()] ()
removeRight_completionSelector = mkSelector "removeRight:completion:"

-- | @Selector@ for @removeRightForIdentifier:completion:@
removeRightForIdentifier_completionSelector :: Selector '[Id NSString, Ptr ()] ()
removeRightForIdentifier_completionSelector = mkSelector "removeRightForIdentifier:completion:"

-- | @Selector@ for @removeAllRightsWithCompletion:@
removeAllRightsWithCompletionSelector :: Selector '[Ptr ()] ()
removeAllRightsWithCompletionSelector = mkSelector "removeAllRightsWithCompletion:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LARightStore)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LARightStore)
initSelector = mkSelector "init"

-- | @Selector@ for @sharedStore@
sharedStoreSelector :: Selector '[] (Id LARightStore)
sharedStoreSelector = mkSelector "sharedStore"

