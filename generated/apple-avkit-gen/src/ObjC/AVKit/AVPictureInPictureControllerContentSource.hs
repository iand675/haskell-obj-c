{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPictureInPictureControllerContentSource
--
-- A content source for AVPictureInPictureController.
--
-- Create a content source with an appropriate layer, and use it to initialize the AVPictureInPictureController.
--
-- Generated bindings for @AVPictureInPictureControllerContentSource@.
module ObjC.AVKit.AVPictureInPictureControllerContentSource
  ( AVPictureInPictureControllerContentSource
  , IsAVPictureInPictureControllerContentSource(..)
  , init_
  , new
  , initWithPlayerLayer
  , initWithSampleBufferDisplayLayer_playbackDelegate
  , playerLayer
  , sampleBufferDisplayLayer
  , sampleBufferPlaybackDelegate
  , initSelector
  , initWithPlayerLayerSelector
  , initWithSampleBufferDisplayLayer_playbackDelegateSelector
  , newSelector
  , playerLayerSelector
  , sampleBufferDisplayLayerSelector
  , sampleBufferPlaybackDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVKit.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVPictureInPictureControllerContentSource)
init_ avPictureInPictureControllerContentSource =
  sendOwnedMessage avPictureInPictureControllerContentSource initSelector

-- | @+ new@
new :: IO (Id AVPictureInPictureControllerContentSource)
new  =
  do
    cls' <- getRequiredClass "AVPictureInPictureControllerContentSource"
    sendOwnedClassMessage cls' newSelector

-- | initWithPlayerLayer:
--
-- @playerLayer@ — The player layer to be shown in Picture in Picture.
--
-- Use this initializer for a content source with a player layer.
--
-- ObjC selector: @- initWithPlayerLayer:@
initWithPlayerLayer :: (IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource, IsAVPlayerLayer playerLayer) => avPictureInPictureControllerContentSource -> playerLayer -> IO (Id AVPictureInPictureControllerContentSource)
initWithPlayerLayer avPictureInPictureControllerContentSource playerLayer =
  sendOwnedMessage avPictureInPictureControllerContentSource initWithPlayerLayerSelector (toAVPlayerLayer playerLayer)

-- | initWithSampleBufferDisplayLayer:
--
-- @sampleBufferDisplayLayer@ — The sample buffer display layer to be shown in Picture in Picture.
--
-- @playbackDelegate@ — The playback delegate for controlling sample buffer display layer's playback in Picture in Picture.
--
-- Use this initializer for a content source with a sample buffer display layer and playback delegate.
--
-- ObjC selector: @- initWithSampleBufferDisplayLayer:playbackDelegate:@
initWithSampleBufferDisplayLayer_playbackDelegate :: (IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource, IsAVSampleBufferDisplayLayer sampleBufferDisplayLayer) => avPictureInPictureControllerContentSource -> sampleBufferDisplayLayer -> RawId -> IO (Id AVPictureInPictureControllerContentSource)
initWithSampleBufferDisplayLayer_playbackDelegate avPictureInPictureControllerContentSource sampleBufferDisplayLayer playbackDelegate =
  sendOwnedMessage avPictureInPictureControllerContentSource initWithSampleBufferDisplayLayer_playbackDelegateSelector (toAVSampleBufferDisplayLayer sampleBufferDisplayLayer) playbackDelegate

-- | playerLayer
--
-- The receiver's player layer.
--
-- ObjC selector: @- playerLayer@
playerLayer :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVPlayerLayer)
playerLayer avPictureInPictureControllerContentSource =
  sendMessage avPictureInPictureControllerContentSource playerLayerSelector

-- | sampleBufferDisplayLayer
--
-- The receiver's sample buffer display layer.
--
-- ObjC selector: @- sampleBufferDisplayLayer@
sampleBufferDisplayLayer :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVSampleBufferDisplayLayer)
sampleBufferDisplayLayer avPictureInPictureControllerContentSource =
  sendMessage avPictureInPictureControllerContentSource sampleBufferDisplayLayerSelector

-- | sampleBufferPlaybackDelegate
--
-- The receiver's sample buffer playback delegate.
--
-- ObjC selector: @- sampleBufferPlaybackDelegate@
sampleBufferPlaybackDelegate :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO RawId
sampleBufferPlaybackDelegate avPictureInPictureControllerContentSource =
  sendMessage avPictureInPictureControllerContentSource sampleBufferPlaybackDelegateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPictureInPictureControllerContentSource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPictureInPictureControllerContentSource)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPlayerLayer:@
initWithPlayerLayerSelector :: Selector '[Id AVPlayerLayer] (Id AVPictureInPictureControllerContentSource)
initWithPlayerLayerSelector = mkSelector "initWithPlayerLayer:"

-- | @Selector@ for @initWithSampleBufferDisplayLayer:playbackDelegate:@
initWithSampleBufferDisplayLayer_playbackDelegateSelector :: Selector '[Id AVSampleBufferDisplayLayer, RawId] (Id AVPictureInPictureControllerContentSource)
initWithSampleBufferDisplayLayer_playbackDelegateSelector = mkSelector "initWithSampleBufferDisplayLayer:playbackDelegate:"

-- | @Selector@ for @playerLayer@
playerLayerSelector :: Selector '[] (Id AVPlayerLayer)
playerLayerSelector = mkSelector "playerLayer"

-- | @Selector@ for @sampleBufferDisplayLayer@
sampleBufferDisplayLayerSelector :: Selector '[] (Id AVSampleBufferDisplayLayer)
sampleBufferDisplayLayerSelector = mkSelector "sampleBufferDisplayLayer"

-- | @Selector@ for @sampleBufferPlaybackDelegate@
sampleBufferPlaybackDelegateSelector :: Selector '[] RawId
sampleBufferPlaybackDelegateSelector = mkSelector "sampleBufferPlaybackDelegate"

