{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMediaSelection@.
module ObjC.AVFoundation.AVMediaSelection
  ( AVMediaSelection
  , IsAVMediaSelection(..)
  , selectedMediaOptionInMediaSelectionGroup
  , mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup
  , asset
  , selectedMediaOptionInMediaSelectionGroupSelector
  , mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroupSelector
  , assetSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | selectedMediaOptionInMediaSelectionGroup:
--
-- Indicates the media selection option that's currently selected from the specified group. May be nil.
--
-- @mediaSelectionGroup@ — A media selection group obtained from the receiver's asset.
--
-- Returns: An instance of AVMediaSelectionOption that describes the currently selection option in the group.
--
-- If the value of the property allowsEmptySelection of the AVMediaSelectionGroup is YES, the currently selected option in the group may be nil.
--
-- ObjC selector: @- selectedMediaOptionInMediaSelectionGroup:@
selectedMediaOptionInMediaSelectionGroup :: (IsAVMediaSelection avMediaSelection, IsAVMediaSelectionGroup mediaSelectionGroup) => avMediaSelection -> mediaSelectionGroup -> IO (Id AVMediaSelectionOption)
selectedMediaOptionInMediaSelectionGroup avMediaSelection  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avMediaSelection (mkSelector "selectedMediaOptionInMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup:
--
-- Indicates that specified media selection group is subject to automatic media selection.
--
-- @mediaSelectionGroup@ — A media selection group obtained from the receiver's asset.
--
-- Returns: YES if the group is subject to automatic media selection.
--
-- Automatic application of media selection criteria is suspended in any group in which a specific selection has been made via an invocation of -selectMediaOption:inMediaSelectionGroup:.
--
-- ObjC selector: @- mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup:@
mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup :: (IsAVMediaSelection avMediaSelection, IsAVMediaSelectionGroup mediaSelectionGroup) => avMediaSelection -> mediaSelectionGroup -> IO Bool
mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup avMediaSelection  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMediaSelection (mkSelector "mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup:") retCULong [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())]

-- | @- asset@
asset :: IsAVMediaSelection avMediaSelection => avMediaSelection -> IO (Id AVAsset)
asset avMediaSelection  =
  sendMsg avMediaSelection (mkSelector "asset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectedMediaOptionInMediaSelectionGroup:@
selectedMediaOptionInMediaSelectionGroupSelector :: Selector
selectedMediaOptionInMediaSelectionGroupSelector = mkSelector "selectedMediaOptionInMediaSelectionGroup:"

-- | @Selector@ for @mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup:@
mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroupSelector :: Selector
mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroupSelector = mkSelector "mediaSelectionCriteriaCanBeAppliedAutomaticallyToMediaSelectionGroup:"

-- | @Selector@ for @asset@
assetSelector :: Selector
assetSelector = mkSelector "asset"

