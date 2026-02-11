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
  , newSelector
  , initWithPlayerLayerSelector
  , initWithSampleBufferDisplayLayer_playbackDelegateSelector
  , playerLayerSelector
  , sampleBufferDisplayLayerSelector
  , sampleBufferPlaybackDelegateSelector


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

import ObjC.AVKit.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVPictureInPictureControllerContentSource)
init_ avPictureInPictureControllerContentSource  =
    sendMsg avPictureInPictureControllerContentSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPictureInPictureControllerContentSource)
new  =
  do
    cls' <- getRequiredClass "AVPictureInPictureControllerContentSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithPlayerLayer:
--
-- @playerLayer@ — The player layer to be shown in Picture in Picture.
--
-- Use this initializer for a content source with a player layer.
--
-- ObjC selector: @- initWithPlayerLayer:@
initWithPlayerLayer :: (IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource, IsAVPlayerLayer playerLayer) => avPictureInPictureControllerContentSource -> playerLayer -> IO (Id AVPictureInPictureControllerContentSource)
initWithPlayerLayer avPictureInPictureControllerContentSource  playerLayer =
  withObjCPtr playerLayer $ \raw_playerLayer ->
      sendMsg avPictureInPictureControllerContentSource (mkSelector "initWithPlayerLayer:") (retPtr retVoid) [argPtr (castPtr raw_playerLayer :: Ptr ())] >>= ownedObject . castPtr

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
initWithSampleBufferDisplayLayer_playbackDelegate avPictureInPictureControllerContentSource  sampleBufferDisplayLayer playbackDelegate =
  withObjCPtr sampleBufferDisplayLayer $ \raw_sampleBufferDisplayLayer ->
      sendMsg avPictureInPictureControllerContentSource (mkSelector "initWithSampleBufferDisplayLayer:playbackDelegate:") (retPtr retVoid) [argPtr (castPtr raw_sampleBufferDisplayLayer :: Ptr ()), argPtr (castPtr (unRawId playbackDelegate) :: Ptr ())] >>= ownedObject . castPtr

-- | playerLayer
--
-- The receiver's player layer.
--
-- ObjC selector: @- playerLayer@
playerLayer :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVPlayerLayer)
playerLayer avPictureInPictureControllerContentSource  =
    sendMsg avPictureInPictureControllerContentSource (mkSelector "playerLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleBufferDisplayLayer
--
-- The receiver's sample buffer display layer.
--
-- ObjC selector: @- sampleBufferDisplayLayer@
sampleBufferDisplayLayer :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO (Id AVSampleBufferDisplayLayer)
sampleBufferDisplayLayer avPictureInPictureControllerContentSource  =
    sendMsg avPictureInPictureControllerContentSource (mkSelector "sampleBufferDisplayLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleBufferPlaybackDelegate
--
-- The receiver's sample buffer playback delegate.
--
-- ObjC selector: @- sampleBufferPlaybackDelegate@
sampleBufferPlaybackDelegate :: IsAVPictureInPictureControllerContentSource avPictureInPictureControllerContentSource => avPictureInPictureControllerContentSource -> IO RawId
sampleBufferPlaybackDelegate avPictureInPictureControllerContentSource  =
    fmap (RawId . castPtr) $ sendMsg avPictureInPictureControllerContentSource (mkSelector "sampleBufferPlaybackDelegate") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPlayerLayer:@
initWithPlayerLayerSelector :: Selector
initWithPlayerLayerSelector = mkSelector "initWithPlayerLayer:"

-- | @Selector@ for @initWithSampleBufferDisplayLayer:playbackDelegate:@
initWithSampleBufferDisplayLayer_playbackDelegateSelector :: Selector
initWithSampleBufferDisplayLayer_playbackDelegateSelector = mkSelector "initWithSampleBufferDisplayLayer:playbackDelegate:"

-- | @Selector@ for @playerLayer@
playerLayerSelector :: Selector
playerLayerSelector = mkSelector "playerLayer"

-- | @Selector@ for @sampleBufferDisplayLayer@
sampleBufferDisplayLayerSelector :: Selector
sampleBufferDisplayLayerSelector = mkSelector "sampleBufferDisplayLayer"

-- | @Selector@ for @sampleBufferPlaybackDelegate@
sampleBufferPlaybackDelegateSelector :: Selector
sampleBufferPlaybackDelegateSelector = mkSelector "sampleBufferPlaybackDelegate"

