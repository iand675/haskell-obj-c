{-# LANGUAGE DataKinds #-}
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
  , recordReferenceForRecordWithIdentifier
  , recordIdentifierForReference_isModified
  , recordIdentifierForReference_isModifiedSelector
  , recordReferenceForRecordWithIdentifierSelector
  , recordsWithIdentifiersSelector
  , recordsWithMatchingAttributesSelector
  , sourceIdentifiersForRelationshipName_withTargetIdentifierSelector
  , targetIdentifiersForRelationshipName_withSourceIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- recordsWithIdentifiers:@
recordsWithIdentifiers :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSArray recordIds) => iSyncRecordSnapshot -> recordIds -> IO (Id NSDictionary)
recordsWithIdentifiers iSyncRecordSnapshot recordIds =
  sendMessage iSyncRecordSnapshot recordsWithIdentifiersSelector (toNSArray recordIds)

-- | @- targetIdentifiersForRelationshipName:withSourceIdentifier:@
targetIdentifiersForRelationshipName_withSourceIdentifier :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSString relationshipName, IsNSString sourceId) => iSyncRecordSnapshot -> relationshipName -> sourceId -> IO (Id NSArray)
targetIdentifiersForRelationshipName_withSourceIdentifier iSyncRecordSnapshot relationshipName sourceId =
  sendMessage iSyncRecordSnapshot targetIdentifiersForRelationshipName_withSourceIdentifierSelector (toNSString relationshipName) (toNSString sourceId)

-- | @- sourceIdentifiersForRelationshipName:withTargetIdentifier:@
sourceIdentifiersForRelationshipName_withTargetIdentifier :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSString relationshipName, IsNSString sourceId) => iSyncRecordSnapshot -> relationshipName -> sourceId -> IO (Id NSArray)
sourceIdentifiersForRelationshipName_withTargetIdentifier iSyncRecordSnapshot relationshipName sourceId =
  sendMessage iSyncRecordSnapshot sourceIdentifiersForRelationshipName_withTargetIdentifierSelector (toNSString relationshipName) (toNSString sourceId)

-- | @- recordsWithMatchingAttributes:@
recordsWithMatchingAttributes :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSDictionary attributes) => iSyncRecordSnapshot -> attributes -> IO (Id NSDictionary)
recordsWithMatchingAttributes iSyncRecordSnapshot attributes =
  sendMessage iSyncRecordSnapshot recordsWithMatchingAttributesSelector (toNSDictionary attributes)

-- | @- recordReferenceForRecordWithIdentifier:@
recordReferenceForRecordWithIdentifier :: (IsISyncRecordSnapshot iSyncRecordSnapshot, IsNSString identifier) => iSyncRecordSnapshot -> identifier -> IO RawId
recordReferenceForRecordWithIdentifier iSyncRecordSnapshot identifier =
  sendMessage iSyncRecordSnapshot recordReferenceForRecordWithIdentifierSelector (toNSString identifier)

-- | @- recordIdentifierForReference:isModified:@
recordIdentifierForReference_isModified :: IsISyncRecordSnapshot iSyncRecordSnapshot => iSyncRecordSnapshot -> RawId -> Ptr Bool -> IO (Id NSString)
recordIdentifierForReference_isModified iSyncRecordSnapshot reference pModified =
  sendMessage iSyncRecordSnapshot recordIdentifierForReference_isModifiedSelector reference pModified

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordsWithIdentifiers:@
recordsWithIdentifiersSelector :: Selector '[Id NSArray] (Id NSDictionary)
recordsWithIdentifiersSelector = mkSelector "recordsWithIdentifiers:"

-- | @Selector@ for @targetIdentifiersForRelationshipName:withSourceIdentifier:@
targetIdentifiersForRelationshipName_withSourceIdentifierSelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
targetIdentifiersForRelationshipName_withSourceIdentifierSelector = mkSelector "targetIdentifiersForRelationshipName:withSourceIdentifier:"

-- | @Selector@ for @sourceIdentifiersForRelationshipName:withTargetIdentifier:@
sourceIdentifiersForRelationshipName_withTargetIdentifierSelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
sourceIdentifiersForRelationshipName_withTargetIdentifierSelector = mkSelector "sourceIdentifiersForRelationshipName:withTargetIdentifier:"

-- | @Selector@ for @recordsWithMatchingAttributes:@
recordsWithMatchingAttributesSelector :: Selector '[Id NSDictionary] (Id NSDictionary)
recordsWithMatchingAttributesSelector = mkSelector "recordsWithMatchingAttributes:"

-- | @Selector@ for @recordReferenceForRecordWithIdentifier:@
recordReferenceForRecordWithIdentifierSelector :: Selector '[Id NSString] RawId
recordReferenceForRecordWithIdentifierSelector = mkSelector "recordReferenceForRecordWithIdentifier:"

-- | @Selector@ for @recordIdentifierForReference:isModified:@
recordIdentifierForReference_isModifiedSelector :: Selector '[RawId, Ptr Bool] (Id NSString)
recordIdentifierForReference_isModifiedSelector = mkSelector "recordIdentifierForReference:isModified:"

