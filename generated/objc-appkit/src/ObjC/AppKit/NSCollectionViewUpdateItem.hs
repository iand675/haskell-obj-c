{-# LANGUAGE PatternSynonyms #-}
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
  , indexPathBeforeUpdateSelector
  , indexPathAfterUpdateSelector
  , updateActionSelector

  -- * Enum types
  , NSCollectionUpdateAction(NSCollectionUpdateAction)
  , pattern NSCollectionUpdateActionInsert
  , pattern NSCollectionUpdateActionDelete
  , pattern NSCollectionUpdateActionReload
  , pattern NSCollectionUpdateActionMove
  , pattern NSCollectionUpdateActionNone

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- indexPathBeforeUpdate@
indexPathBeforeUpdate :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO (Id NSIndexPath)
indexPathBeforeUpdate nsCollectionViewUpdateItem  =
  sendMsg nsCollectionViewUpdateItem (mkSelector "indexPathBeforeUpdate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexPathAfterUpdate@
indexPathAfterUpdate :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO (Id NSIndexPath)
indexPathAfterUpdate nsCollectionViewUpdateItem  =
  sendMsg nsCollectionViewUpdateItem (mkSelector "indexPathAfterUpdate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updateAction@
updateAction :: IsNSCollectionViewUpdateItem nsCollectionViewUpdateItem => nsCollectionViewUpdateItem -> IO NSCollectionUpdateAction
updateAction nsCollectionViewUpdateItem  =
  fmap (coerce :: CLong -> NSCollectionUpdateAction) $ sendMsg nsCollectionViewUpdateItem (mkSelector "updateAction") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathBeforeUpdate@
indexPathBeforeUpdateSelector :: Selector
indexPathBeforeUpdateSelector = mkSelector "indexPathBeforeUpdate"

-- | @Selector@ for @indexPathAfterUpdate@
indexPathAfterUpdateSelector :: Selector
indexPathAfterUpdateSelector = mkSelector "indexPathAfterUpdate"

-- | @Selector@ for @updateAction@
updateActionSelector :: Selector
updateActionSelector = mkSelector "updateAction"

