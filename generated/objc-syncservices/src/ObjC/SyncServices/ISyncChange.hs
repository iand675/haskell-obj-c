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
  , initWithChangeType_recordIdentifier_changesSelector
  , typeSelector
  , recordIdentifierSelector
  , recordSelector
  , changesSelector


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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ changeWithType:recordIdentifier:changes:@
changeWithType_recordIdentifier_changes :: (IsNSString recordIdentifier, IsNSArray changes) => CInt -> recordIdentifier -> changes -> IO RawId
changeWithType_recordIdentifier_changes type_ recordIdentifier changes =
  do
    cls' <- getRequiredClass "ISyncChange"
    withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
      withObjCPtr changes $ \raw_changes ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "changeWithType:recordIdentifier:changes:") (retPtr retVoid) [argCInt (fromIntegral type_), argPtr (castPtr raw_recordIdentifier :: Ptr ()), argPtr (castPtr raw_changes :: Ptr ())]

-- | @- initWithChangeType:recordIdentifier:changes:@
initWithChangeType_recordIdentifier_changes :: (IsISyncChange iSyncChange, IsNSString recordIdentifier, IsNSArray changes) => iSyncChange -> CInt -> recordIdentifier -> changes -> IO RawId
initWithChangeType_recordIdentifier_changes iSyncChange  type_ recordIdentifier changes =
withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
  withObjCPtr changes $ \raw_changes ->
      fmap (RawId . castPtr) $ sendMsg iSyncChange (mkSelector "initWithChangeType:recordIdentifier:changes:") (retPtr retVoid) [argCInt (fromIntegral type_), argPtr (castPtr raw_recordIdentifier :: Ptr ()), argPtr (castPtr raw_changes :: Ptr ())]

-- | @- type@
type_ :: IsISyncChange iSyncChange => iSyncChange -> IO CInt
type_ iSyncChange  =
  sendMsg iSyncChange (mkSelector "type") retCInt []

-- | @- recordIdentifier@
recordIdentifier :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSString)
recordIdentifier iSyncChange  =
  sendMsg iSyncChange (mkSelector "recordIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- record@
record :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSDictionary)
record iSyncChange  =
  sendMsg iSyncChange (mkSelector "record") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changes@
changes :: IsISyncChange iSyncChange => iSyncChange -> IO (Id NSArray)
changes iSyncChange  =
  sendMsg iSyncChange (mkSelector "changes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeWithType:recordIdentifier:changes:@
changeWithType_recordIdentifier_changesSelector :: Selector
changeWithType_recordIdentifier_changesSelector = mkSelector "changeWithType:recordIdentifier:changes:"

-- | @Selector@ for @initWithChangeType:recordIdentifier:changes:@
initWithChangeType_recordIdentifier_changesSelector :: Selector
initWithChangeType_recordIdentifier_changesSelector = mkSelector "initWithChangeType:recordIdentifier:changes:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @record@
recordSelector :: Selector
recordSelector = mkSelector "record"

-- | @Selector@ for @changes@
changesSelector :: Selector
changesSelector = mkSelector "changes"

