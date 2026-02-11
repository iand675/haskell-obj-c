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
  , rightForIdentifier_completionSelector
  , saveRight_identifier_completionSelector
  , saveRight_identifier_secret_completionSelector
  , removeRight_completionSelector
  , removeRightForIdentifier_completionSelector
  , removeAllRightsWithCompletionSelector
  , newSelector
  , initSelector
  , sharedStoreSelector


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
rightForIdentifier_completion laRightStore  identifier handler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg laRightStore (mkSelector "rightForIdentifier:completion:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

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
saveRight_identifier_completion laRightStore  right identifier handler =
withObjCPtr right $ \raw_right ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg laRightStore (mkSelector "saveRight:identifier:completion:") retVoid [argPtr (castPtr raw_right :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

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
saveRight_identifier_secret_completion laRightStore  right identifier secret handler =
withObjCPtr right $ \raw_right ->
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr secret $ \raw_secret ->
        sendMsg laRightStore (mkSelector "saveRight:identifier:secret:completion:") retVoid [argPtr (castPtr raw_right :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_secret :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Removes a right from the persistent storage along with its associated resources.
--
-- @right@ — @LAPersistedRight@ instance to remove.
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeRight:completion:@
removeRight_completion :: (IsLARightStore laRightStore, IsLAPersistedRight right) => laRightStore -> right -> Ptr () -> IO ()
removeRight_completion laRightStore  right handler =
withObjCPtr right $ \raw_right ->
    sendMsg laRightStore (mkSelector "removeRight:completion:") retVoid [argPtr (castPtr raw_right :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Removes right with provided identifier from persistant storage.
--
-- @identifier@ — Identifier of @LAPersistedRight@ instance to remove.
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeRightForIdentifier:completion:@
removeRightForIdentifier_completion :: (IsLARightStore laRightStore, IsNSString identifier) => laRightStore -> identifier -> Ptr () -> IO ()
removeRightForIdentifier_completion laRightStore  identifier handler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg laRightStore (mkSelector "removeRightForIdentifier:completion:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Removes all rights stored by the client
--
-- @handler@ — Completion handler with an error on failure.
--
-- ObjC selector: @- removeAllRightsWithCompletion:@
removeAllRightsWithCompletion :: IsLARightStore laRightStore => laRightStore -> Ptr () -> IO ()
removeAllRightsWithCompletion laRightStore  handler =
  sendMsg laRightStore (mkSelector "removeAllRightsWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Clients should rely on the @shared@ instance instead
--
-- ObjC selector: @+ new@
new :: IO (Id LARightStore)
new  =
  do
    cls' <- getRequiredClass "LARightStore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Clients should rely on the @shared@ instance instead
--
-- ObjC selector: @- init@
init_ :: IsLARightStore laRightStore => laRightStore -> IO (Id LARightStore)
init_ laRightStore  =
  sendMsg laRightStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Shared instance of @LARightStore.@
--
-- ObjC selector: @+ sharedStore@
sharedStore :: IO (Id LARightStore)
sharedStore  =
  do
    cls' <- getRequiredClass "LARightStore"
    sendClassMsg cls' (mkSelector "sharedStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rightForIdentifier:completion:@
rightForIdentifier_completionSelector :: Selector
rightForIdentifier_completionSelector = mkSelector "rightForIdentifier:completion:"

-- | @Selector@ for @saveRight:identifier:completion:@
saveRight_identifier_completionSelector :: Selector
saveRight_identifier_completionSelector = mkSelector "saveRight:identifier:completion:"

-- | @Selector@ for @saveRight:identifier:secret:completion:@
saveRight_identifier_secret_completionSelector :: Selector
saveRight_identifier_secret_completionSelector = mkSelector "saveRight:identifier:secret:completion:"

-- | @Selector@ for @removeRight:completion:@
removeRight_completionSelector :: Selector
removeRight_completionSelector = mkSelector "removeRight:completion:"

-- | @Selector@ for @removeRightForIdentifier:completion:@
removeRightForIdentifier_completionSelector :: Selector
removeRightForIdentifier_completionSelector = mkSelector "removeRightForIdentifier:completion:"

-- | @Selector@ for @removeAllRightsWithCompletion:@
removeAllRightsWithCompletionSelector :: Selector
removeAllRightsWithCompletionSelector = mkSelector "removeAllRightsWithCompletion:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sharedStore@
sharedStoreSelector :: Selector
sharedStoreSelector = mkSelector "sharedStore"

