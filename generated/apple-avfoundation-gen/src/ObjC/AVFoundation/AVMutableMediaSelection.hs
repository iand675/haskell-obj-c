{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableMediaSelection@.
module ObjC.AVFoundation.AVMutableMediaSelection
  ( AVMutableMediaSelection
  , IsAVMutableMediaSelection(..)
  , selectMediaOption_inMediaSelectionGroup
  , selectMediaOption_inMediaSelectionGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | selectMediaOption:inMediaSelectionGroup:
--
-- Selects the media option described by the specified instance of AVMediaSelectionOption in the specified AVMediaSelectionGroup and deselects all other options in that group.
--
-- @mediaSelectionOption@ — The option to select.
--
-- @mediaSelectionGroup@ — The media selection group, obtained from the receiver's asset, that contains the specified option.
--
-- If the specified media selection option isn't a member of the specified media selection group, no change in presentation state will result.				If the value of the property allowsEmptySelection of the AVMediaSelectionGroup is YES, you can pass nil for mediaSelectionOption to deselect all media selection options in the group.
--
-- ObjC selector: @- selectMediaOption:inMediaSelectionGroup:@
selectMediaOption_inMediaSelectionGroup :: (IsAVMutableMediaSelection avMutableMediaSelection, IsAVMediaSelectionOption mediaSelectionOption, IsAVMediaSelectionGroup mediaSelectionGroup) => avMutableMediaSelection -> mediaSelectionOption -> mediaSelectionGroup -> IO ()
selectMediaOption_inMediaSelectionGroup avMutableMediaSelection mediaSelectionOption mediaSelectionGroup =
  sendMessage avMutableMediaSelection selectMediaOption_inMediaSelectionGroupSelector (toAVMediaSelectionOption mediaSelectionOption) (toAVMediaSelectionGroup mediaSelectionGroup)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectMediaOption:inMediaSelectionGroup:@
selectMediaOption_inMediaSelectionGroupSelector :: Selector '[Id AVMediaSelectionOption, Id AVMediaSelectionGroup] ()
selectMediaOption_inMediaSelectionGroupSelector = mkSelector "selectMediaOption:inMediaSelectionGroup:"

