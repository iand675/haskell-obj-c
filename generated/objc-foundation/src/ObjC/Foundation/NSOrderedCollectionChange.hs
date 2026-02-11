{-# LANGUAGE PatternSynonyms #-}
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
  , changeWithObject_type_indexSelector
  , changeWithObject_type_index_associatedIndexSelector
  , initSelector
  , initWithObject_type_indexSelector
  , initWithObject_type_index_associatedIndexSelector
  , objectSelector
  , changeTypeSelector
  , indexSelector
  , associatedIndexSelector

  -- * Enum types
  , NSCollectionChangeType(NSCollectionChangeType)
  , pattern NSCollectionChangeInsert
  , pattern NSCollectionChangeRemove

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ changeWithObject:type:index:@
changeWithObject_type_index :: RawId -> NSCollectionChangeType -> CULong -> IO (Id NSOrderedCollectionChange)
changeWithObject_type_index anObject type_ index =
  do
    cls' <- getRequiredClass "NSOrderedCollectionChange"
    sendClassMsg cls' (mkSelector "changeWithObject:type:index:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ()), argCLong (coerce type_), argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @+ changeWithObject:type:index:associatedIndex:@
changeWithObject_type_index_associatedIndex :: RawId -> NSCollectionChangeType -> CULong -> CULong -> IO (Id NSOrderedCollectionChange)
changeWithObject_type_index_associatedIndex anObject type_ index associatedIndex =
  do
    cls' <- getRequiredClass "NSOrderedCollectionChange"
    sendClassMsg cls' (mkSelector "changeWithObject:type:index:associatedIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ()), argCLong (coerce type_), argCULong (fromIntegral index), argCULong (fromIntegral associatedIndex)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO RawId
init_ nsOrderedCollectionChange  =
  fmap (RawId . castPtr) $ sendMsg nsOrderedCollectionChange (mkSelector "init") (retPtr retVoid) []

-- | @- initWithObject:type:index:@
initWithObject_type_index :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> RawId -> NSCollectionChangeType -> CULong -> IO (Id NSOrderedCollectionChange)
initWithObject_type_index nsOrderedCollectionChange  anObject type_ index =
  sendMsg nsOrderedCollectionChange (mkSelector "initWithObject:type:index:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ()), argCLong (coerce type_), argCULong (fromIntegral index)] >>= ownedObject . castPtr

-- | @- initWithObject:type:index:associatedIndex:@
initWithObject_type_index_associatedIndex :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> RawId -> NSCollectionChangeType -> CULong -> CULong -> IO (Id NSOrderedCollectionChange)
initWithObject_type_index_associatedIndex nsOrderedCollectionChange  anObject type_ index associatedIndex =
  sendMsg nsOrderedCollectionChange (mkSelector "initWithObject:type:index:associatedIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ()), argCLong (coerce type_), argCULong (fromIntegral index), argCULong (fromIntegral associatedIndex)] >>= ownedObject . castPtr

-- | @- object@
object :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO RawId
object nsOrderedCollectionChange  =
  fmap (RawId . castPtr) $ sendMsg nsOrderedCollectionChange (mkSelector "object") (retPtr retVoid) []

-- | @- changeType@
changeType :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO NSCollectionChangeType
changeType nsOrderedCollectionChange  =
  fmap (coerce :: CLong -> NSCollectionChangeType) $ sendMsg nsOrderedCollectionChange (mkSelector "changeType") retCLong []

-- | @- index@
index :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO CULong
index nsOrderedCollectionChange  =
  sendMsg nsOrderedCollectionChange (mkSelector "index") retCULong []

-- | @- associatedIndex@
associatedIndex :: IsNSOrderedCollectionChange nsOrderedCollectionChange => nsOrderedCollectionChange -> IO CULong
associatedIndex nsOrderedCollectionChange  =
  sendMsg nsOrderedCollectionChange (mkSelector "associatedIndex") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeWithObject:type:index:@
changeWithObject_type_indexSelector :: Selector
changeWithObject_type_indexSelector = mkSelector "changeWithObject:type:index:"

-- | @Selector@ for @changeWithObject:type:index:associatedIndex:@
changeWithObject_type_index_associatedIndexSelector :: Selector
changeWithObject_type_index_associatedIndexSelector = mkSelector "changeWithObject:type:index:associatedIndex:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObject:type:index:@
initWithObject_type_indexSelector :: Selector
initWithObject_type_indexSelector = mkSelector "initWithObject:type:index:"

-- | @Selector@ for @initWithObject:type:index:associatedIndex:@
initWithObject_type_index_associatedIndexSelector :: Selector
initWithObject_type_index_associatedIndexSelector = mkSelector "initWithObject:type:index:associatedIndex:"

-- | @Selector@ for @object@
objectSelector :: Selector
objectSelector = mkSelector "object"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @associatedIndex@
associatedIndexSelector :: Selector
associatedIndexSelector = mkSelector "associatedIndex"

