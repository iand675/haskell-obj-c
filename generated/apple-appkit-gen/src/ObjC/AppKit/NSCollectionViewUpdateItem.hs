{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewUpdateItem@.
module ObjC.AppKit.NSCollectionViewUpdateItem
  ( NSCollectionViewUpdateItem
  , IsNSCollectionViewUpdateItem(..)
  , indexPathBeforeUpdate
  , indexPathAfterUpdate
  , updateAction
  , indexPathAfterUpdateSelector
  , indexPathBeforeUpdateSelector
  , updateActionSelector

  -- * Enum types
  , NSCollectionUpdateAction(NSCollectionUpdateAction)
  , pattern NSCollectionUpdateActionInsert
  , pattern NSCollectionUpdateActionDelete
  , pattern NSCollectionUpdateActionReload
  , pattern NSCollectionUpdateActionMove
  , pattern NSCollectionUpdateActionNone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- indexPathBeforeUpdate@
indexPathBeforeUpdate :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO (Id NSIndexPath)
indexPathBeforeUpdate nsCollectionViewUpdateItem =
  sendMessage nsCollectionViewUpdateItem indexPathBeforeUpdateSelector

-- | @- indexPathAfterUpdate@
indexPathAfterUpdate :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO (Id NSIndexPath)
indexPathAfterUpdate nsCollectionViewUpdateItem =
  sendMessage nsCollectionViewUpdateItem indexPathAfterUpdateSelector

-- | @- updateAction@
updateAction :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO NSCollectionUpdateAction
updateAction nsCollectionViewUpdateItem =
  sendMessage nsCollectionViewUpdateItem updateActionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathBeforeUpdate@
indexPathBeforeUpdateSelector :: Selector '[] (Id NSIndexPath)
indexPathBeforeUpdateSelector = mkSelector "indexPathBeforeUpdate"

-- | @Selector@ for @indexPathAfterUpdate@
indexPathAfterUpdateSelector :: Selector '[] (Id NSIndexPath)
indexPathAfterUpdateSelector = mkSelector "indexPathAfterUpdate"

-- | @Selector@ for @updateAction@
updateActionSelector :: Selector '[] NSCollectionUpdateAction
updateActionSelector = mkSelector "updateAction"

