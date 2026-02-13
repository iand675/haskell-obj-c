{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOrderedCollectionChange@.
module ObjC.Foundation.NSOrderedCollectionChange
  ( NSOrderedCollectionChange
  , IsNSOrderedCollectionChange(..)
  , changeWithObject_type_index
  , changeWithObject_type_index_associatedIndex
  , init_
  , initWithObject_type_index
  , initWithObject_type_index_associatedIndex
  , object
  , changeType
  , index
  , associatedIndex
  , associatedIndexSelector
  , changeTypeSelector
  , changeWithObject_type_indexSelector
  , changeWithObject_type_index_associatedIndexSelector
  , indexSelector
  , initSelector
  , initWithObject_type_indexSelector
  , initWithObject_type_index_associatedIndexSelector
  , objectSelector

  -- * Enum types
  , NSCollectionChangeType(NSCollectionChangeType)
  , pattern NSCollectionChangeInsert
  , pattern NSCollectionChangeRemove

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ changeWithObject:type:index:@
changeWithObject_type_index :: RawId -> NSCollectionChangeType -> CULong -> IO (Id NSOrderedCollectionChange)
changeWithObject_type_index anObject type_ index =
  do
    cls' <- getRequiredClass "NSOrderedCollectionChange"
    sendClassMessage cls' changeWithObject_type_indexSelector anObject type_ index

-- | @+ changeWithObject:type:index:associatedIndex:@
changeWithObject_type_index_associatedIndex :: RawId -> NSCollectionChangeType -> CULong -> CULong -> IO (Id NSOrderedCollectionChange)
changeWithObject_type_index_associatedIndex anObject type_ index associatedIndex =
  do
    cls' <- getRequiredClass "NSOrderedCollectionChange"
    sendClassMessage cls' changeWithObject_type_index_associatedIndexSelector anObject type_ index associatedIndex

-- | @- init@
init_ :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO RawId
init_ nsOrderedCollectionChange =
  sendOwnedMessage nsOrderedCollectionChange initSelector

-- | @- initWithObject:type:index:@
initWithObject_type_index :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> RawId -> NSCollectionChangeType -> CULong -> IO (Id NSOrderedCollectionChange)
initWithObject_type_index nsOrderedCollectionChange anObject type_ index =
  sendOwnedMessage nsOrderedCollectionChange initWithObject_type_indexSelector anObject type_ index

-- | @- initWithObject:type:index:associatedIndex:@
initWithObject_type_index_associatedIndex :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> RawId -> NSCollectionChangeType -> CULong -> CULong -> IO (Id NSOrderedCollectionChange)
initWithObject_type_index_associatedIndex nsOrderedCollectionChange anObject type_ index associatedIndex =
  sendOwnedMessage nsOrderedCollectionChange initWithObject_type_index_associatedIndexSelector anObject type_ index associatedIndex

-- | @- object@
object :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO RawId
object nsOrderedCollectionChange =
  sendMessage nsOrderedCollectionChange objectSelector

-- | @- changeType@
changeType :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO NSCollectionChangeType
changeType nsOrderedCollectionChange =
  sendMessage nsOrderedCollectionChange changeTypeSelector

-- | @- index@
index :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO CULong
index nsOrderedCollectionChange =
  sendMessage nsOrderedCollectionChange indexSelector

-- | @- associatedIndex@
associatedIndex :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO CULong
associatedIndex nsOrderedCollectionChange =
  sendMessage nsOrderedCollectionChange associatedIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeWithObject:type:index:@
changeWithObject_type_indexSelector :: Selector '[RawId, NSCollectionChangeType, CULong] (Id NSOrderedCollectionChange)
changeWithObject_type_indexSelector = mkSelector "changeWithObject:type:index:"

-- | @Selector@ for @changeWithObject:type:index:associatedIndex:@
changeWithObject_type_index_associatedIndexSelector :: Selector '[RawId, NSCollectionChangeType, CULong, CULong] (Id NSOrderedCollectionChange)
changeWithObject_type_index_associatedIndexSelector = mkSelector "changeWithObject:type:index:associatedIndex:"

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObject:type:index:@
initWithObject_type_indexSelector :: Selector '[RawId, NSCollectionChangeType, CULong] (Id NSOrderedCollectionChange)
initWithObject_type_indexSelector = mkSelector "initWithObject:type:index:"

-- | @Selector@ for @initWithObject:type:index:associatedIndex:@
initWithObject_type_index_associatedIndexSelector :: Selector '[RawId, NSCollectionChangeType, CULong, CULong] (Id NSOrderedCollectionChange)
initWithObject_type_index_associatedIndexSelector = mkSelector "initWithObject:type:index:associatedIndex:"

-- | @Selector@ for @object@
objectSelector :: Selector '[] RawId
objectSelector = mkSelector "object"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector '[] NSCollectionChangeType
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @associatedIndex@
associatedIndexSelector :: Selector '[] CULong
associatedIndexSelector = mkSelector "associatedIndex"

