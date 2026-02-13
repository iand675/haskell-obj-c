{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information associated with an AVAsset for a cinematic video.
--
-- Generated bindings for @CNAssetInfo@.
module ObjC.Cinematic.CNAssetInfo
  ( CNAssetInfo
  , IsCNAssetInfo(..)
  , checkIfCinematic_completionHandler
  , loadFromAsset_completionHandler
  , init_
  , new
  , asset
  , allCinematicTracks
  , cinematicVideoTrack
  , cinematicDisparityTrack
  , cinematicMetadataTrack
  , frameTimingTrack
  , videoCompositionTracks
  , videoCompositionTrackIDs
  , sampleDataTrackIDs
  , allCinematicTracksSelector
  , assetSelector
  , checkIfCinematic_completionHandlerSelector
  , cinematicDisparityTrackSelector
  , cinematicMetadataTrackSelector
  , cinematicVideoTrackSelector
  , frameTimingTrackSelector
  , initSelector
  , loadFromAsset_completionHandlerSelector
  , newSelector
  , sampleDataTrackIDsSelector
  , videoCompositionTrackIDsSelector
  , videoCompositionTracksSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Check if asset is cinematic asynchronously.
--
-- ObjC selector: @+ checkIfCinematic:completionHandler:@
checkIfCinematic_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
checkIfCinematic_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    sendClassMessage cls' checkIfCinematic_completionHandlerSelector (toAVAsset asset) completionHandler

-- | Load cinematic asset information asynchronously.
--
-- ObjC selector: @+ loadFromAsset:completionHandler:@
loadFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
loadFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    sendClassMessage cls' loadFromAsset_completionHandlerSelector (toAVAsset asset) completionHandler

-- | @- init@
init_ :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id CNAssetInfo)
init_ cnAssetInfo =
  sendOwnedMessage cnAssetInfo initSelector

-- | @+ new@
new :: IO (Id CNAssetInfo)
new  =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- asset@
asset :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAsset)
asset cnAssetInfo =
  sendMessage cnAssetInfo assetSelector

-- | @- allCinematicTracks@
allCinematicTracks :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
allCinematicTracks cnAssetInfo =
  sendMessage cnAssetInfo allCinematicTracksSelector

-- | @- cinematicVideoTrack@
cinematicVideoTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicVideoTrack cnAssetInfo =
  sendMessage cnAssetInfo cinematicVideoTrackSelector

-- | @- cinematicDisparityTrack@
cinematicDisparityTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicDisparityTrack cnAssetInfo =
  sendMessage cnAssetInfo cinematicDisparityTrackSelector

-- | @- cinematicMetadataTrack@
cinematicMetadataTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicMetadataTrack cnAssetInfo =
  sendMessage cnAssetInfo cinematicMetadataTrackSelector

-- | Track to be used for frame timing
--
-- ObjC selector: @- frameTimingTrack@
frameTimingTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
frameTimingTrack cnAssetInfo =
  sendMessage cnAssetInfo frameTimingTrackSelector

-- | Tracks required to construct AVAssetReaderVideoCompositionOutput.
--
-- ObjC selector: @- videoCompositionTracks@
videoCompositionTracks :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
videoCompositionTracks cnAssetInfo =
  sendMessage cnAssetInfo videoCompositionTracksSelector

-- | Source video track IDs required to implement AVVideoCompositionInstruction protocol
--
-- ObjC selector: @- videoCompositionTrackIDs@
videoCompositionTrackIDs :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
videoCompositionTrackIDs cnAssetInfo =
  sendMessage cnAssetInfo videoCompositionTrackIDsSelector

-- | Source metadata track IDs required to implement AVVideoCompositionInstruction protocol
--
-- ObjC selector: @- sampleDataTrackIDs@
sampleDataTrackIDs :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
sampleDataTrackIDs cnAssetInfo =
  sendMessage cnAssetInfo sampleDataTrackIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkIfCinematic:completionHandler:@
checkIfCinematic_completionHandlerSelector :: Selector '[Id AVAsset, Ptr ()] ()
checkIfCinematic_completionHandlerSelector = mkSelector "checkIfCinematic:completionHandler:"

-- | @Selector@ for @loadFromAsset:completionHandler:@
loadFromAsset_completionHandlerSelector :: Selector '[Id AVAsset, Ptr ()] ()
loadFromAsset_completionHandlerSelector = mkSelector "loadFromAsset:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNAssetInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNAssetInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id AVAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @allCinematicTracks@
allCinematicTracksSelector :: Selector '[] (Id NSArray)
allCinematicTracksSelector = mkSelector "allCinematicTracks"

-- | @Selector@ for @cinematicVideoTrack@
cinematicVideoTrackSelector :: Selector '[] (Id AVAssetTrack)
cinematicVideoTrackSelector = mkSelector "cinematicVideoTrack"

-- | @Selector@ for @cinematicDisparityTrack@
cinematicDisparityTrackSelector :: Selector '[] (Id AVAssetTrack)
cinematicDisparityTrackSelector = mkSelector "cinematicDisparityTrack"

-- | @Selector@ for @cinematicMetadataTrack@
cinematicMetadataTrackSelector :: Selector '[] (Id AVAssetTrack)
cinematicMetadataTrackSelector = mkSelector "cinematicMetadataTrack"

-- | @Selector@ for @frameTimingTrack@
frameTimingTrackSelector :: Selector '[] (Id AVAssetTrack)
frameTimingTrackSelector = mkSelector "frameTimingTrack"

-- | @Selector@ for @videoCompositionTracks@
videoCompositionTracksSelector :: Selector '[] (Id NSArray)
videoCompositionTracksSelector = mkSelector "videoCompositionTracks"

-- | @Selector@ for @videoCompositionTrackIDs@
videoCompositionTrackIDsSelector :: Selector '[] (Id NSArray)
videoCompositionTrackIDsSelector = mkSelector "videoCompositionTrackIDs"

-- | @Selector@ for @sampleDataTrackIDs@
sampleDataTrackIDsSelector :: Selector '[] (Id NSArray)
sampleDataTrackIDsSelector = mkSelector "sampleDataTrackIDs"

