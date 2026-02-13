{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableComposition@.
module ObjC.Cinematic.AVMutableComposition
  ( AVMutableComposition
  , IsAVMutableComposition(..)
  , addTracksForCinematicAssetInfo_preferredStartingTrackID
  , addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes

-- | Adds a group of empty tracks associated with a cinematic asset to a mutable composition. - Returns: Information about the composition tracks added to the mutable composition. Be sure to call insertTimeRange on the result to specify at least one time range of cinematic asset you'd like in the composition.
--
-- ObjC selector: @- addTracksForCinematicAssetInfo:preferredStartingTrackID:@
addTracksForCinematicAssetInfo_preferredStartingTrackID :: (IsAVMutableComposition avMutableComposition, IsCNAssetInfo assetInfo) => avMutableComposition -> assetInfo -> CInt -> IO (Id CNCompositionInfo)
addTracksForCinematicAssetInfo_preferredStartingTrackID avMutableComposition assetInfo preferredStartingTrackID =
  sendMessage avMutableComposition addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector (toCNAssetInfo assetInfo) preferredStartingTrackID

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTracksForCinematicAssetInfo:preferredStartingTrackID:@
addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector :: Selector '[Id CNAssetInfo, CInt] (Id CNCompositionInfo)
addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector = mkSelector "addTracksForCinematicAssetInfo:preferredStartingTrackID:"

