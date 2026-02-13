{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tool for using Core Animation in a video composition.
--
-- Instances of AVVideoCompositionCoreAnimationTool are for use with offline rendering (AVAssetExportSession and AVAssetReader), not with AVPlayer. To synchronize real-time playback with other CoreAnimation layers, use AVSynchronizedLayer.
--
-- Any animations will be interpreted on the video's timeline, not real-time, so (a) set animation beginTimes to small positive value such as AVCoreAnimationBeginTimeAtZero rather than 0, because CoreAnimation will replace a value of 0 with CACurrentMediaTime();  (b) set removedOnCompletion to NO on animations so they are not automatically removed; (c) do not use layers associated with UIViews.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoCompositionCoreAnimationTool@.
module ObjC.AVFoundation.AVVideoCompositionCoreAnimationTool
  ( AVVideoCompositionCoreAnimationTool
  , IsAVVideoCompositionCoreAnimationTool(..)
  , videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackID
  , videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayer
  , videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayer
  , videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackIDSelector
  , videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayerSelector
  , videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | Add a Core Animation layer to the video composition
--
-- Include a Core Animation layer as an individual track input in video composition. This layer should not come from, or be added to, another layer tree. trackID should not match any real trackID in the source. Use -[AVAsset unusedTrackID]  to obtain a trackID that's guaranteed not to coincide with the trackID of any track of the asset. AVVideoCompositionInstructions should reference trackID where the rendered animation should be included. For best performance, no transform should be set in the AVVideoCompositionLayerInstruction for this trackID. Be aware that on iOS, CALayers backing a UIView usually have their content flipped (as defined by the -contentsAreFlipped method). It may be required to insert a CALayer with its geometryFlipped property set to YES in the layer hierarchy to get the same result when attaching a CALayer to a AVVideoCompositionCoreAnimationTool as when using it to back a UIView.
--
-- ObjC selector: @+ videoCompositionCoreAnimationToolWithAdditionalLayer:asTrackID:@
videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackID :: IsCALayer layer => layer -> CInt -> IO (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackID layer trackID =
  do
    cls' <- getRequiredClass "AVVideoCompositionCoreAnimationTool"
    sendClassMessage cls' videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackIDSelector (toCALayer layer) trackID

-- | Compose the composited video frames with the Core Animation layer
--
-- Place composited video frames in videoLayer and render animationLayer  to produce the final frame. Normally videoLayer should be in animationLayer's sublayer tree. The animationLayer should not come from, or be added to, another layer tree. Be aware that on iOS, CALayers backing a UIView usually have their content flipped (as defined by the -contentsAreFlipped method). It may be required to insert a CALayer with its geometryFlipped property set to YES in the layer hierarchy to get the same result when attaching a CALayer to a AVVideoCompositionCoreAnimationTool as when using it to back a UIView.
--
-- ObjC selector: @+ videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer:inLayer:@
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayer :: (IsCALayer videoLayer, IsCALayer animationLayer) => videoLayer -> animationLayer -> IO (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayer videoLayer animationLayer =
  do
    cls' <- getRequiredClass "AVVideoCompositionCoreAnimationTool"
    sendClassMessage cls' videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayerSelector (toCALayer videoLayer) (toCALayer animationLayer)

-- | Compose the composited video frames with the Core Animation layer
--
-- Duplicate the composited video frames in each videoLayer and render animationLayer  to produce the final frame. Normally videoLayers should be in animationLayer's sublayer tree. The animationLayer should not come from, or be added to, another layer tree. Be aware that on iOS, CALayers backing a UIView usually have their content flipped (as defined by the -contentsAreFlipped method). It may be required to insert a CALayer with its geometryFlipped property set to YES in the layer hierarchy to get the same result when attaching a CALayer to a AVVideoCompositionCoreAnimationTool as when using it to back a UIView.
--
-- ObjC selector: @+ videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers:inLayer:@
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayer :: (IsNSArray videoLayers, IsCALayer animationLayer) => videoLayers -> animationLayer -> IO (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayer videoLayers animationLayer =
  do
    cls' <- getRequiredClass "AVVideoCompositionCoreAnimationTool"
    sendClassMessage cls' videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayerSelector (toNSArray videoLayers) (toCALayer animationLayer)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionCoreAnimationToolWithAdditionalLayer:asTrackID:@
videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackIDSelector :: Selector '[Id CALayer, CInt] (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithAdditionalLayer_asTrackIDSelector = mkSelector "videoCompositionCoreAnimationToolWithAdditionalLayer:asTrackID:"

-- | @Selector@ for @videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer:inLayer:@
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayerSelector :: Selector '[Id CALayer, Id CALayer] (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer_inLayerSelector = mkSelector "videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayer:inLayer:"

-- | @Selector@ for @videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers:inLayer:@
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayerSelector :: Selector '[Id NSArray, Id CALayer] (Id AVVideoCompositionCoreAnimationTool)
videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers_inLayerSelector = mkSelector "videoCompositionCoreAnimationToolWithPostProcessingAsVideoLayers:inLayer:"

