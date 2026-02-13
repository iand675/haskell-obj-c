{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPictureInPictureController
--
-- AVPictureInPictureController is a subclass of NSObject that can be used to present the contents of an AVPlayerLayer or AVPlayerView floating on top of applications.
--
-- Generated bindings for @AVPictureInPictureController@.
module ObjC.AVKit.AVPictureInPictureController
  ( AVPictureInPictureController
  , IsAVPictureInPictureController(..)
  , isPictureInPictureSupported
  , initWithContentSource
  , initWithPlayerLayer
  , startPictureInPicture
  , stopPictureInPicture
  , invalidatePlaybackState
  , pictureInPictureButtonStartImage
  , pictureInPictureButtonStopImage
  , contentSource
  , setContentSource
  , playerLayer
  , delegate
  , setDelegate
  , pictureInPicturePossible
  , pictureInPictureActive
  , pictureInPictureSuspended
  , canStopPictureInPicture
  , requiresLinearPlayback
  , setRequiresLinearPlayback
  , canStartPictureInPictureAutomaticallyFromInline
  , setCanStartPictureInPictureAutomaticallyFromInline
  , canStartPictureInPictureAutomaticallyFromInlineSelector
  , canStopPictureInPictureSelector
  , contentSourceSelector
  , delegateSelector
  , initWithContentSourceSelector
  , initWithPlayerLayerSelector
  , invalidatePlaybackStateSelector
  , isPictureInPictureSupportedSelector
  , pictureInPictureActiveSelector
  , pictureInPictureButtonStartImageSelector
  , pictureInPictureButtonStopImageSelector
  , pictureInPicturePossibleSelector
  , pictureInPictureSuspendedSelector
  , playerLayerSelector
  , requiresLinearPlaybackSelector
  , setCanStartPictureInPictureAutomaticallyFromInlineSelector
  , setContentSourceSelector
  , setDelegateSelector
  , setRequiresLinearPlaybackSelector
  , startPictureInPictureSelector
  , stopPictureInPictureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVKit.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | isPictureInPictureSupported
--
-- Whether or not Picture in Picture is supported on the current device.
--
-- When NO, all initializers will return nil.
--
-- ObjC selector: @+ isPictureInPictureSupported@
isPictureInPictureSupported :: IO Bool
isPictureInPictureSupported  =
  do
    cls' <- getRequiredClass "AVPictureInPictureController"
    sendClassMessage cls' isPictureInPictureSupportedSelector

-- | initWithContentSource:
--
-- @contentSource@ — The content source to be shown in Picture in Picture.
--
-- Use this initializer for content that may be a sample buffer display layer or a player layer.
--
-- ObjC selector: @- initWithContentSource:@
initWithContentSource :: (IsAVPictureInPictureController avPictureInPictureController, IsAVPictureInPictureControllerContentSource contentSource) => avPictureInPictureController -> contentSource -> IO (Id AVPictureInPictureController)
initWithContentSource avPictureInPictureController contentSource =
  sendOwnedMessage avPictureInPictureController initWithContentSourceSelector (toAVPictureInPictureControllerContentSource contentSource)

-- | initWithPlayerLayer:
--
-- @playerLayer@ — The player layer from which to source the media content for the Picture in Picture controller.
--
-- Initialize the picture in picture controller with a player layer.
--
-- ObjC selector: @- initWithPlayerLayer:@
initWithPlayerLayer :: (IsAVPictureInPictureController avPictureInPictureController, IsAVPlayerLayer playerLayer) => avPictureInPictureController -> playerLayer -> IO (Id AVPictureInPictureController)
initWithPlayerLayer avPictureInPictureController playerLayer =
  sendOwnedMessage avPictureInPictureController initWithPlayerLayerSelector (toAVPlayerLayer playerLayer)

-- | startPictureInPicture
--
-- Start Picture in Picture for the provided AVPlayerLayer if possible.
--
-- Receiver will call -pictureInPictureControllerWillStartPictureInPicture: if Picture in Picture is currently possible and -pictureInPictureControllerDidStartPictureInPicture: after a successful start. If starting Picture in Picture fails, -pictureInPictureControllerFailedToStartPictureInPicture:withError: is called on the delegate instead. Client can stop Picture in Picture by calling -stopPictureInPicture. In addition the user can stop Picture in Picture through user interaction. It is also possible that Picture in Picture is stopped by the Picture in Picture controller at any time. In all these cases receiver calls -pictureInPictureControllerWillStopPictureInPicture: on the delegate and -pictureInPictureControllerDidStopPictureInPicture:after the stop animation completed.
--
-- ObjC selector: @- startPictureInPicture@
startPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
startPictureInPicture avPictureInPictureController =
  sendMessage avPictureInPictureController startPictureInPictureSelector

-- | stopPictureInPicture
--
-- Stop the local Picture in Picture if currently active. On tvOS, this can also stop Picture in Picture sessions for other applications.
--
-- See startPictureInPicture for details.
--
-- ObjC selector: @- stopPictureInPicture@
stopPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
stopPictureInPicture avPictureInPictureController =
  sendMessage avPictureInPictureController stopPictureInPictureSelector

-- | invalidatePlaybackState
--
-- Informs Picture in Picture controller that it should request an updated playback state from its sampleBufferPlaybackDelegate.
--
-- This should always be called whenever playback is paused or unpaused, or the underlying content duration changes.
--
-- ObjC selector: @- invalidatePlaybackState@
invalidatePlaybackState :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
invalidatePlaybackState avPictureInPictureController =
  sendMessage avPictureInPictureController invalidatePlaybackStateSelector

-- | pictureInPictureButtonStartImage
--
-- System default Picture in Picture start template image for use in client's Picture in Picture button.
--
-- ObjC selector: @+ pictureInPictureButtonStartImage@
pictureInPictureButtonStartImage :: IO (Id NSImage)
pictureInPictureButtonStartImage  =
  do
    cls' <- getRequiredClass "AVPictureInPictureController"
    sendClassMessage cls' pictureInPictureButtonStartImageSelector

-- | pictureInPictureButtonStopImage
--
-- System default Picture in Picture stop template image for use in client's Picture in Picture button.
--
-- ObjC selector: @+ pictureInPictureButtonStopImage@
pictureInPictureButtonStopImage :: IO (Id NSImage)
pictureInPictureButtonStopImage  =
  do
    cls' <- getRequiredClass "AVPictureInPictureController"
    sendClassMessage cls' pictureInPictureButtonStopImageSelector

-- | contentSource
--
-- The receiver's content source. Can be changed while Picture in Picture is active, but the new content source must be ready for display (in the case of AVPlayerLayer, that means AVPlayerLayer.isReadyForDisplay must return YES), otherwise Picture in Picture will stop.
--
-- ObjC selector: @- contentSource@
contentSource :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO (Id AVPictureInPictureControllerContentSource)
contentSource avPictureInPictureController =
  sendMessage avPictureInPictureController contentSourceSelector

-- | contentSource
--
-- The receiver's content source. Can be changed while Picture in Picture is active, but the new content source must be ready for display (in the case of AVPlayerLayer, that means AVPlayerLayer.isReadyForDisplay must return YES), otherwise Picture in Picture will stop.
--
-- ObjC selector: @- setContentSource:@
setContentSource :: (IsAVPictureInPictureController avPictureInPictureController, IsAVPictureInPictureControllerContentSource value) => avPictureInPictureController -> value -> IO ()
setContentSource avPictureInPictureController value =
  sendMessage avPictureInPictureController setContentSourceSelector (toAVPictureInPictureControllerContentSource value)

-- | playerLayer
--
-- The receiver's player layer.
--
-- ObjC selector: @- playerLayer@
playerLayer :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO (Id AVPlayerLayer)
playerLayer avPictureInPictureController =
  sendMessage avPictureInPictureController playerLayerSelector

-- | delegate
--
-- The receiver's delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO RawId
delegate avPictureInPictureController =
  sendMessage avPictureInPictureController delegateSelector

-- | delegate
--
-- The receiver's delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> RawId -> IO ()
setDelegate avPictureInPictureController value =
  sendMessage avPictureInPictureController setDelegateSelector value

-- | pictureInPicturePossible
--
-- Whether or not Picture in Picture is currently possible.
--
-- ObjC selector: @- pictureInPicturePossible@
pictureInPicturePossible :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPicturePossible avPictureInPictureController =
  sendMessage avPictureInPictureController pictureInPicturePossibleSelector

-- | pictureInPictureActive
--
-- Whether or not Picture in Picture is currently active.
--
-- ObjC selector: @- pictureInPictureActive@
pictureInPictureActive :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPictureActive avPictureInPictureController =
  sendMessage avPictureInPictureController pictureInPictureActiveSelector

-- | pictureInPictureSuspended
--
-- Whether or not Picture in Picture is currently suspended.
--
-- ObjC selector: @- pictureInPictureSuspended@
pictureInPictureSuspended :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPictureSuspended avPictureInPictureController =
  sendMessage avPictureInPictureController pictureInPictureSuspendedSelector

-- | canStopPictureInPicture
--
-- Whether or not any Picture in Picture is active, and can be stopped.
--
-- When true, stopPictureInPicture will stop the active Picture in Picture session. Apps should re-inspect the system-provided picture in picture start button image when this property changes. Observable.
--
-- ObjC selector: @- canStopPictureInPicture@
canStopPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
canStopPictureInPicture avPictureInPictureController =
  sendMessage avPictureInPictureController canStopPictureInPictureSelector

-- | requiresLinearPlayback
--
-- Disables certain user operations (fast forward, forward skip, and scrubbing).
--
-- This can be used to temporarily enforce playback of mandatory content (such as legalese or advertisements).
--
-- ObjC selector: @- requiresLinearPlayback@
requiresLinearPlayback :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
requiresLinearPlayback avPictureInPictureController =
  sendMessage avPictureInPictureController requiresLinearPlaybackSelector

-- | requiresLinearPlayback
--
-- Disables certain user operations (fast forward, forward skip, and scrubbing).
--
-- This can be used to temporarily enforce playback of mandatory content (such as legalese or advertisements).
--
-- ObjC selector: @- setRequiresLinearPlayback:@
setRequiresLinearPlayback :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> Bool -> IO ()
setRequiresLinearPlayback avPictureInPictureController value =
  sendMessage avPictureInPictureController setRequiresLinearPlaybackSelector value

-- | canStartPictureInPictureAutomaticallyFromInline
--
-- Indicates whether Picture in Picture should be allowed to start automatically when transitioning to background when the receiver’s content is embedded inline. Default is NO.
--
-- This property must only be set to YES for content intended to be the user's primary focus.
--
-- ObjC selector: @- canStartPictureInPictureAutomaticallyFromInline@
canStartPictureInPictureAutomaticallyFromInline :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
canStartPictureInPictureAutomaticallyFromInline avPictureInPictureController =
  sendMessage avPictureInPictureController canStartPictureInPictureAutomaticallyFromInlineSelector

-- | canStartPictureInPictureAutomaticallyFromInline
--
-- Indicates whether Picture in Picture should be allowed to start automatically when transitioning to background when the receiver’s content is embedded inline. Default is NO.
--
-- This property must only be set to YES for content intended to be the user's primary focus.
--
-- ObjC selector: @- setCanStartPictureInPictureAutomaticallyFromInline:@
setCanStartPictureInPictureAutomaticallyFromInline :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> Bool -> IO ()
setCanStartPictureInPictureAutomaticallyFromInline avPictureInPictureController value =
  sendMessage avPictureInPictureController setCanStartPictureInPictureAutomaticallyFromInlineSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isPictureInPictureSupported@
isPictureInPictureSupportedSelector :: Selector '[] Bool
isPictureInPictureSupportedSelector = mkSelector "isPictureInPictureSupported"

-- | @Selector@ for @initWithContentSource:@
initWithContentSourceSelector :: Selector '[Id AVPictureInPictureControllerContentSource] (Id AVPictureInPictureController)
initWithContentSourceSelector = mkSelector "initWithContentSource:"

-- | @Selector@ for @initWithPlayerLayer:@
initWithPlayerLayerSelector :: Selector '[Id AVPlayerLayer] (Id AVPictureInPictureController)
initWithPlayerLayerSelector = mkSelector "initWithPlayerLayer:"

-- | @Selector@ for @startPictureInPicture@
startPictureInPictureSelector :: Selector '[] ()
startPictureInPictureSelector = mkSelector "startPictureInPicture"

-- | @Selector@ for @stopPictureInPicture@
stopPictureInPictureSelector :: Selector '[] ()
stopPictureInPictureSelector = mkSelector "stopPictureInPicture"

-- | @Selector@ for @invalidatePlaybackState@
invalidatePlaybackStateSelector :: Selector '[] ()
invalidatePlaybackStateSelector = mkSelector "invalidatePlaybackState"

-- | @Selector@ for @pictureInPictureButtonStartImage@
pictureInPictureButtonStartImageSelector :: Selector '[] (Id NSImage)
pictureInPictureButtonStartImageSelector = mkSelector "pictureInPictureButtonStartImage"

-- | @Selector@ for @pictureInPictureButtonStopImage@
pictureInPictureButtonStopImageSelector :: Selector '[] (Id NSImage)
pictureInPictureButtonStopImageSelector = mkSelector "pictureInPictureButtonStopImage"

-- | @Selector@ for @contentSource@
contentSourceSelector :: Selector '[] (Id AVPictureInPictureControllerContentSource)
contentSourceSelector = mkSelector "contentSource"

-- | @Selector@ for @setContentSource:@
setContentSourceSelector :: Selector '[Id AVPictureInPictureControllerContentSource] ()
setContentSourceSelector = mkSelector "setContentSource:"

-- | @Selector@ for @playerLayer@
playerLayerSelector :: Selector '[] (Id AVPlayerLayer)
playerLayerSelector = mkSelector "playerLayer"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @pictureInPicturePossible@
pictureInPicturePossibleSelector :: Selector '[] Bool
pictureInPicturePossibleSelector = mkSelector "pictureInPicturePossible"

-- | @Selector@ for @pictureInPictureActive@
pictureInPictureActiveSelector :: Selector '[] Bool
pictureInPictureActiveSelector = mkSelector "pictureInPictureActive"

-- | @Selector@ for @pictureInPictureSuspended@
pictureInPictureSuspendedSelector :: Selector '[] Bool
pictureInPictureSuspendedSelector = mkSelector "pictureInPictureSuspended"

-- | @Selector@ for @canStopPictureInPicture@
canStopPictureInPictureSelector :: Selector '[] Bool
canStopPictureInPictureSelector = mkSelector "canStopPictureInPicture"

-- | @Selector@ for @requiresLinearPlayback@
requiresLinearPlaybackSelector :: Selector '[] Bool
requiresLinearPlaybackSelector = mkSelector "requiresLinearPlayback"

-- | @Selector@ for @setRequiresLinearPlayback:@
setRequiresLinearPlaybackSelector :: Selector '[Bool] ()
setRequiresLinearPlaybackSelector = mkSelector "setRequiresLinearPlayback:"

-- | @Selector@ for @canStartPictureInPictureAutomaticallyFromInline@
canStartPictureInPictureAutomaticallyFromInlineSelector :: Selector '[] Bool
canStartPictureInPictureAutomaticallyFromInlineSelector = mkSelector "canStartPictureInPictureAutomaticallyFromInline"

-- | @Selector@ for @setCanStartPictureInPictureAutomaticallyFromInline:@
setCanStartPictureInPictureAutomaticallyFromInlineSelector :: Selector '[Bool] ()
setCanStartPictureInPictureAutomaticallyFromInlineSelector = mkSelector "setCanStartPictureInPictureAutomaticallyFromInline:"

