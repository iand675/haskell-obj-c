{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncChange@.
module ObjC.SyncServices.ISyncChange
  ( ISyncChange
  , IsISyncChange(..)
  , changeWithType_recordIdentifier_changes
  , initWithChangeType_recordIdentifier_changes
  , type_
  , recordIdentifier
  , record
  , changes
  , changeWithType_recordIdentifier_changesSelector
  , changesSelector
  , initWithChangeType_recordIdentifier_changesSelector
  , recordIdentifierSelector
  , recordSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ changeWithType:recordIdentifier:changes:@
changeWithType_recordIdentifier_changes :: (IsNSString recordIdentifier, IsNSArray changes) => CInt -> recordIdentifier -> changes -> IO RawId
changeWithType_recordIdentifier_changes type_ recordIdentifier changes =
  do
    cls' <- getRequiredClass "ISyncChange"
    sendClassMessage cls' changeWithType_recordIdentifier_changesSelector type_ (toNSString recordIdentifier) (toNSArray changes)

-- | @- initWithChangeType:recordIdentifier:changes:@
initWithChangeType_recordIdentifier_changes :: (IsISyncChange iSyncChange, IsNSString recordIdentifier, IsNSArray changes) => iSyncChange -> CInt -> recordIdentifier -> changes -> IO RawId
initWithChangeType_recordIdentifier_changes iSyncChange type_ recordIdentifier changes =
  sendOwnedMessage iSyncChange initWithChangeType_recordIdentifier_changesSelector type_ (toNSString recordIdentifier) (toNSArray changes)

-- | @- type@
type_ :: IsISyncChange iSyncChange => iSyncChange -> IO CInt
type_ iSyncChange =
  sendMessage iSyncChange typeSelector

-- | @- recordIdentifier@
recordIdentifier :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSString)
recordIdentifier iSyncChange =
  sendMessage iSyncChange recordIdentifierSelector

-- | @- record@
record :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSDictionary)
record iSyncChange =
  sendMessage iSyncChange recordSelector

-- | @- changes@
changes :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSArray)
changes iSyncChange =
  sendMessage iSyncChange changesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeWithType:recordIdentifier:changes:@
changeWithType_recordIdentifier_changesSelector :: Selector '[CInt, Id NSString, Id NSArray] RawId
changeWithType_recordIdentifier_changesSelector = mkSelector "changeWithType:recordIdentifier:changes:"

-- | @Selector@ for @initWithChangeType:recordIdentifier:changes:@
initWithChangeType_recordIdentifier_changesSelector :: Selector '[CInt, Id NSString, Id NSArray] RawId
initWithChangeType_recordIdentifier_changesSelector = mkSelector "initWithChangeType:recordIdentifier:changes:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CInt
typeSelector = mkSelector "type"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector '[] (Id NSString)
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @record@
recordSelector :: Selector '[] (Id NSDictionary)
recordSelector = mkSelector "record"

-- | @Selector@ for @changes@
changesSelector :: Selector '[] (Id NSArray)
changesSelector = mkSelector "changes"

