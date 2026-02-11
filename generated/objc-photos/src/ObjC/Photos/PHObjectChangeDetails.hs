{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHObjectChangeDetails@.
module ObjC.Photos.PHObjectChangeDetails
  ( PHObjectChangeDetails
  , IsPHObjectChangeDetails(..)
  , objectBeforeChanges
  , objectAfterChanges
  , assetContentChanged
  , objectWasDeleted
  , objectBeforeChangesSelector
  , objectAfterChangesSelector
  , assetContentChangedSelector
  , objectWasDeletedSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- objectBeforeChanges@
objectBeforeChanges :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO (Id PHObject)
objectBeforeChanges phObjectChangeDetails  =
  sendMsg phObjectChangeDetails (mkSelector "objectBeforeChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectAfterChanges@
objectAfterChanges :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO (Id PHObject)
objectAfterChanges phObjectChangeDetails  =
  sendMsg phObjectChangeDetails (mkSelector "objectAfterChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- assetContentChanged@
assetContentChanged :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO Bool
assetContentChanged phObjectChangeDetails  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phObjectChangeDetails (mkSelector "assetContentChanged") retCULong []

-- | @- objectWasDeleted@
objectWasDeleted :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO Bool
objectWasDeleted phObjectChangeDetails  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phObjectChangeDetails (mkSelector "objectWasDeleted") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectBeforeChanges@
objectBeforeChangesSelector :: Selector
objectBeforeChangesSelector = mkSelector "objectBeforeChanges"

-- | @Selector@ for @objectAfterChanges@
objectAfterChangesSelector :: Selector
objectAfterChangesSelector = mkSelector "objectAfterChanges"

-- | @Selector@ for @assetContentChanged@
assetContentChangedSelector :: Selector
assetContentChangedSelector = mkSelector "assetContentChanged"

-- | @Selector@ for @objectWasDeleted@
objectWasDeletedSelector :: Selector
objectWasDeletedSelector = mkSelector "objectWasDeleted"

