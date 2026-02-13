{-# LANGUAGE DataKinds #-}
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
  , assetContentChangedSelector
  , objectAfterChangesSelector
  , objectBeforeChangesSelector
  , objectWasDeletedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- objectBeforeChanges@
objectBeforeChanges :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO (Id PHObject)
objectBeforeChanges phObjectChangeDetails =
  sendMessage phObjectChangeDetails objectBeforeChangesSelector

-- | @- objectAfterChanges@
objectAfterChanges :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO (Id PHObject)
objectAfterChanges phObjectChangeDetails =
  sendMessage phObjectChangeDetails objectAfterChangesSelector

-- | @- assetContentChanged@
assetContentChanged :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO Bool
assetContentChanged phObjectChangeDetails =
  sendMessage phObjectChangeDetails assetContentChangedSelector

-- | @- objectWasDeleted@
objectWasDeleted :: IsPHObjectChangeDetails phObjectChangeDetails => phObjectChangeDetails -> IO Bool
objectWasDeleted phObjectChangeDetails =
  sendMessage phObjectChangeDetails objectWasDeletedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectBeforeChanges@
objectBeforeChangesSelector :: Selector '[] (Id PHObject)
objectBeforeChangesSelector = mkSelector "objectBeforeChanges"

-- | @Selector@ for @objectAfterChanges@
objectAfterChangesSelector :: Selector '[] (Id PHObject)
objectAfterChangesSelector = mkSelector "objectAfterChanges"

-- | @Selector@ for @assetContentChanged@
assetContentChangedSelector :: Selector '[] Bool
assetContentChangedSelector = mkSelector "assetContentChanged"

-- | @Selector@ for @objectWasDeleted@
objectWasDeletedSelector :: Selector '[] Bool
objectWasDeletedSelector = mkSelector "objectWasDeleted"

