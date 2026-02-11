{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncRecordSnapshot@.
module ObjC.SyncServices.ISyncRecordSnapshot
  ( ISyncRecordSnapshot
  , IsISyncRecordSnapshot(..)
  , recordsWithIdentifiers
  , targetIdentifiersForRelationshipName_withSourceIdentifier
  , sourceIdentifiersForRelationshipName_withTargetIdentifier
  , recordsWithMatchingAttributes
  , recordsWithIdentifiersSelector
  , targetIdentifiersForRelationshipName_withSourceIdentifierSelector
  , sourceIdentifiersForRelationshipName_withTargetIdentifierSelector
  , recordsWithMatchingAttributesSelector


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

-- | @- recordsWithIdentifiers:@
recordsWithIdentifiers :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSArray recordIds) => iSyncRecordSnapshot -> recordIds -> IO (Id NSDictionary)
recordsWithIdentifiers iSyncRecordSnapshot  recordIds =
withObjCPtr recordIds $ \raw_recordIds ->
    sendMsg iSyncRecordSnapshot (mkSelector "recordsWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_recordIds :: Ptr ())] >>= retainedObject . castPtr

-- | @- targetIdentifiersForRelationshipName:withSourceIdentifier:@
targetIdentifiersForRelationshipName_withSourceIdentifier :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSString relationshipName, IsNSString sourceId) => iSyncRecordSnapshot -> relationshipName -> sourceId -> IO (Id NSArray)
targetIdentifiersForRelationshipName_withSourceIdentifier iSyncRecordSnapshot  relationshipName sourceId =
withObjCPtr relationshipName $ \raw_relationshipName ->
  withObjCPtr sourceId $ \raw_sourceId ->
      sendMsg iSyncRecordSnapshot (mkSelector "targetIdentifiersForRelationshipName:withSourceIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relationshipName :: Ptr ()), argPtr (castPtr raw_sourceId :: Ptr ())] >>= retainedObject . castPtr

-- | @- sourceIdentifiersForRelationshipName:withTargetIdentifier:@
sourceIdentifiersForRelationshipName_withTargetIdentifier :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSString relationshipName, IsNSString sourceId) => iSyncRecordSnapshot -> relationshipName -> sourceId -> IO (Id NSArray)
sourceIdentifiersForRelationshipName_withTargetIdentifier iSyncRecordSnapshot  relationshipName sourceId =
withObjCPtr relationshipName $ \raw_relationshipName ->
  withObjCPtr sourceId $ \raw_sourceId ->
      sendMsg iSyncRecordSnapshot (mkSelector "sourceIdentifiersForRelationshipName:withTargetIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relationshipName :: Ptr ()), argPtr (castPtr raw_sourceId :: Ptr ())] >>= retainedObject . castPtr

-- | @- recordsWithMatchingAttributes:@
recordsWithMatchingAttributes :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSDictionary attributes) => iSyncRecordSnapshot -> attributes -> IO (Id NSDictionary)
recordsWithMatchingAttributes iSyncRecordSnapshot  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg iSyncRecordSnapshot (mkSelector "recordsWithMatchingAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordsWithIdentifiers:@
recordsWithIdentifiersSelector :: Selector
recordsWithIdentifiersSelector = mkSelector "recordsWithIdentifiers:"

-- | @Selector@ for @targetIdentifiersForRelationshipName:withSourceIdentifier:@
targetIdentifiersForRelationshipName_withSourceIdentifierSelector :: Selector
targetIdentifiersForRelationshipName_withSourceIdentifierSelector = mkSelector "targetIdentifiersForRelationshipName:withSourceIdentifier:"

-- | @Selector@ for @sourceIdentifiersForRelationshipName:withTargetIdentifier:@
sourceIdentifiersForRelationshipName_withTargetIdentifierSelector :: Selector
sourceIdentifiersForRelationshipName_withTargetIdentifierSelector = mkSelector "sourceIdentifiersForRelationshipName:withTargetIdentifier:"

-- | @Selector@ for @recordsWithMatchingAttributes:@
recordsWithMatchingAttributesSelector :: Selector
recordsWithMatchingAttributesSelector = mkSelector "recordsWithMatchingAttributes:"

