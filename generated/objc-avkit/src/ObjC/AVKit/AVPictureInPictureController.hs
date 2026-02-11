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
  , playerLayer
  , pictureInPicturePossible
  , pictureInPictureActive
  , pictureInPictureSuspended
  , canStopPictureInPicture
  , requiresLinearPlayback
  , setRequiresLinearPlayback
  , canStartPictureInPictureAutomaticallyFromInline
  , setCanStartPictureInPictureAutomaticallyFromInline
  , isPictureInPictureSupportedSelector
  , initWithContentSourceSelector
  , initWithPlayerLayerSelector
  , startPictureInPictureSelector
  , stopPictureInPictureSelector
  , invalidatePlaybackStateSelector
  , pictureInPictureButtonStartImageSelector
  , pictureInPictureButtonStopImageSelector
  , playerLayerSelector
  , pictureInPicturePossibleSelector
  , pictureInPictureActiveSelector
  , pictureInPictureSuspendedSelector
  , canStopPictureInPictureSelector
  , requiresLinearPlaybackSelector
  , setRequiresLinearPlaybackSelector
  , canStartPictureInPictureAutomaticallyFromInlineSelector
  , setCanStartPictureInPictureAutomaticallyFromInlineSelector


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
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPictureInPictureSupported") retCULong []

-- | initWithContentSource:
--
-- @contentSource@ — The content source to be shown in Picture in Picture.
--
-- Use this initializer for content that may be a sample buffer display layer or a player layer.
--
-- ObjC selector: @- initWithContentSource:@
initWithContentSource :: (IsAVPictureInPictureController avPictureInPictureController, IsAVPictureInPictureControllerContentSource contentSource) => avPictureInPictureController -> contentSource -> IO (Id AVPictureInPictureController)
initWithContentSource avPictureInPictureController  contentSource =
withObjCPtr contentSource $ \raw_contentSource ->
    sendMsg avPictureInPictureController (mkSelector "initWithContentSource:") (retPtr retVoid) [argPtr (castPtr raw_contentSource :: Ptr ())] >>= ownedObject . castPtr

-- | initWithPlayerLayer:
--
-- @playerLayer@ — The player layer from which to source the media content for the Picture in Picture controller.
--
-- Initialize the picture in picture controller with a player layer.
--
-- ObjC selector: @- initWithPlayerLayer:@
initWithPlayerLayer :: (IsAVPictureInPictureController avPictureInPictureController, IsAVPlayerLayer playerLayer) => avPictureInPictureController -> playerLayer -> IO (Id AVPictureInPictureController)
initWithPlayerLayer avPictureInPictureController  playerLayer =
withObjCPtr playerLayer $ \raw_playerLayer ->
    sendMsg avPictureInPictureController (mkSelector "initWithPlayerLayer:") (retPtr retVoid) [argPtr (castPtr raw_playerLayer :: Ptr ())] >>= ownedObject . castPtr

-- | startPictureInPicture
--
-- Start Picture in Picture for the provided AVPlayerLayer if possible.
--
-- Receiver will call -pictureInPictureControllerWillStartPictureInPicture: if Picture in Picture is currently possible and -pictureInPictureControllerDidStartPictureInPicture: after a successful start. If starting Picture in Picture fails, -pictureInPictureControllerFailedToStartPictureInPicture:withError: is called on the delegate instead. Client can stop Picture in Picture by calling -stopPictureInPicture. In addition the user can stop Picture in Picture through user interaction. It is also possible that Picture in Picture is stopped by the Picture in Picture controller at any time. In all these cases receiver calls -pictureInPictureControllerWillStopPictureInPicture: on the delegate and -pictureInPictureControllerDidStopPictureInPicture:after the stop animation completed.
--
-- ObjC selector: @- startPictureInPicture@
startPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
startPictureInPicture avPictureInPictureController  =
  sendMsg avPictureInPictureController (mkSelector "startPictureInPicture") retVoid []

-- | stopPictureInPicture
--
-- Stop the local Picture in Picture if currently active. On tvOS, this can also stop Picture in Picture sessions for other applications.
--
-- See startPictureInPicture for details.
--
-- ObjC selector: @- stopPictureInPicture@
stopPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
stopPictureInPicture avPictureInPictureController  =
  sendMsg avPictureInPictureController (mkSelector "stopPictureInPicture") retVoid []

-- | invalidatePlaybackState
--
-- Informs Picture in Picture controller that it should request an updated playback state from its sampleBufferPlaybackDelegate.
--
-- This should always be called whenever playback is paused or unpaused, or the underlying content duration changes.
--
-- ObjC selector: @- invalidatePlaybackState@
invalidatePlaybackState :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO ()
invalidatePlaybackState avPictureInPictureController  =
  sendMsg avPictureInPictureController (mkSelector "invalidatePlaybackState") retVoid []

-- | pictureInPictureButtonStartImage
--
-- System default Picture in Picture start template image for use in client's Picture in Picture button.
--
-- ObjC selector: @+ pictureInPictureButtonStartImage@
pictureInPictureButtonStartImage :: IO (Id NSImage)
pictureInPictureButtonStartImage  =
  do
    cls' <- getRequiredClass "AVPictureInPictureController"
    sendClassMsg cls' (mkSelector "pictureInPictureButtonStartImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pictureInPictureButtonStopImage
--
-- System default Picture in Picture stop template image for use in client's Picture in Picture button.
--
-- ObjC selector: @+ pictureInPictureButtonStopImage@
pictureInPictureButtonStopImage :: IO (Id NSImage)
pictureInPictureButtonStopImage  =
  do
    cls' <- getRequiredClass "AVPictureInPictureController"
    sendClassMsg cls' (mkSelector "pictureInPictureButtonStopImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | playerLayer
--
-- The receiver's player layer.
--
-- ObjC selector: @- playerLayer@
playerLayer :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO (Id AVPlayerLayer)
playerLayer avPictureInPictureController  =
  sendMsg avPictureInPictureController (mkSelector "playerLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pictureInPicturePossible
--
-- Whether or not Picture in Picture is currently possible.
--
-- ObjC selector: @- pictureInPicturePossible@
pictureInPicturePossible :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPicturePossible avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "pictureInPicturePossible") retCULong []

-- | pictureInPictureActive
--
-- Whether or not Picture in Picture is currently active.
--
-- ObjC selector: @- pictureInPictureActive@
pictureInPictureActive :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPictureActive avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "pictureInPictureActive") retCULong []

-- | pictureInPictureSuspended
--
-- Whether or not Picture in Picture is currently suspended.
--
-- ObjC selector: @- pictureInPictureSuspended@
pictureInPictureSuspended :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
pictureInPictureSuspended avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "pictureInPictureSuspended") retCULong []

-- | canStopPictureInPicture
--
-- Whether or not any Picture in Picture is active, and can be stopped.
--
-- When true, stopPictureInPicture will stop the active Picture in Picture session. Apps should re-inspect the system-provided picture in picture start button image when this property changes. Observable.
--
-- ObjC selector: @- canStopPictureInPicture@
canStopPictureInPicture :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
canStopPictureInPicture avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "canStopPictureInPicture") retCULong []

-- | requiresLinearPlayback
--
-- Disables certain user operations (fast forward, forward skip, and scrubbing).
--
-- This can be used to temporarily enforce playback of mandatory content (such as legalese or advertisements).
--
-- ObjC selector: @- requiresLinearPlayback@
requiresLinearPlayback :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
requiresLinearPlayback avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "requiresLinearPlayback") retCULong []

-- | requiresLinearPlayback
--
-- Disables certain user operations (fast forward, forward skip, and scrubbing).
--
-- This can be used to temporarily enforce playback of mandatory content (such as legalese or advertisements).
--
-- ObjC selector: @- setRequiresLinearPlayback:@
setRequiresLinearPlayback :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> Bool -> IO ()
setRequiresLinearPlayback avPictureInPictureController  value =
  sendMsg avPictureInPictureController (mkSelector "setRequiresLinearPlayback:") retVoid [argCULong (if value then 1 else 0)]

-- | canStartPictureInPictureAutomaticallyFromInline
--
-- Indicates whether Picture in Picture should be allowed to start automatically when transitioning to background when the receiver’s content is embedded inline. Default is NO.
--
-- This property must only be set to YES for content intended to be the user's primary focus.
--
-- ObjC selector: @- canStartPictureInPictureAutomaticallyFromInline@
canStartPictureInPictureAutomaticallyFromInline :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> IO Bool
canStartPictureInPictureAutomaticallyFromInline avPictureInPictureController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPictureInPictureController (mkSelector "canStartPictureInPictureAutomaticallyFromInline") retCULong []

-- | canStartPictureInPictureAutomaticallyFromInline
--
-- Indicates whether Picture in Picture should be allowed to start automatically when transitioning to background when the receiver’s content is embedded inline. Default is NO.
--
-- This property must only be set to YES for content intended to be the user's primary focus.
--
-- ObjC selector: @- setCanStartPictureInPictureAutomaticallyFromInline:@
setCanStartPictureInPictureAutomaticallyFromInline :: IsAVPictureInPictureController avPictureInPictureController => avPictureInPictureController -> Bool -> IO ()
setCanStartPictureInPictureAutomaticallyFromInline avPictureInPictureController  value =
  sendMsg avPictureInPictureController (mkSelector "setCanStartPictureInPictureAutomaticallyFromInline:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isPictureInPictureSupported@
isPictureInPictureSupportedSelector :: Selector
isPictureInPictureSupportedSelector = mkSelector "isPictureInPictureSupported"

-- | @Selector@ for @initWithContentSource:@
initWithContentSourceSelector :: Selector
initWithContentSourceSelector = mkSelector "initWithContentSource:"

-- | @Selector@ for @initWithPlayerLayer:@
initWithPlayerLayerSelector :: Selector
initWithPlayerLayerSelector = mkSelector "initWithPlayerLayer:"

-- | @Selector@ for @startPictureInPicture@
startPictureInPictureSelector :: Selector
startPictureInPictureSelector = mkSelector "startPictureInPicture"

-- | @Selector@ for @stopPictureInPicture@
stopPictureInPictureSelector :: Selector
stopPictureInPictureSelector = mkSelector "stopPictureInPicture"

-- | @Selector@ for @invalidatePlaybackState@
invalidatePlaybackStateSelector :: Selector
invalidatePlaybackStateSelector = mkSelector "invalidatePlaybackState"

-- | @Selector@ for @pictureInPictureButtonStartImage@
pictureInPictureButtonStartImageSelector :: Selector
pictureInPictureButtonStartImageSelector = mkSelector "pictureInPictureButtonStartImage"

-- | @Selector@ for @pictureInPictureButtonStopImage@
pictureInPictureButtonStopImageSelector :: Selector
pictureInPictureButtonStopImageSelector = mkSelector "pictureInPictureButtonStopImage"

-- | @Selector@ for @playerLayer@
playerLayerSelector :: Selector
playerLayerSelector = mkSelector "playerLayer"

-- | @Selector@ for @pictureInPicturePossible@
pictureInPicturePossibleSelector :: Selector
pictureInPicturePossibleSelector = mkSelector "pictureInPicturePossible"

-- | @Selector@ for @pictureInPictureActive@
pictureInPictureActiveSelector :: Selector
pictureInPictureActiveSelector = mkSelector "pictureInPictureActive"

-- | @Selector@ for @pictureInPictureSuspended@
pictureInPictureSuspendedSelector :: Selector
pictureInPictureSuspendedSelector = mkSelector "pictureInPictureSuspended"

-- | @Selector@ for @canStopPictureInPicture@
canStopPictureInPictureSelector :: Selector
canStopPictureInPictureSelector = mkSelector "canStopPictureInPicture"

-- | @Selector@ for @requiresLinearPlayback@
requiresLinearPlaybackSelector :: Selector
requiresLinearPlaybackSelector = mkSelector "requiresLinearPlayback"

-- | @Selector@ for @setRequiresLinearPlayback:@
setRequiresLinearPlaybackSelector :: Selector
setRequiresLinearPlaybackSelector = mkSelector "setRequiresLinearPlayback:"

-- | @Selector@ for @canStartPictureInPictureAutomaticallyFromInline@
canStartPictureInPictureAutomaticallyFromInlineSelector :: Selector
canStartPictureInPictureAutomaticallyFromInlineSelector = mkSelector "canStartPictureInPictureAutomaticallyFromInline"

-- | @Selector@ for @setCanStartPictureInPictureAutomaticallyFromInline:@
setCanStartPictureInPictureAutomaticallyFromInlineSelector :: Selector
setCanStartPictureInPictureAutomaticallyFromInlineSelector = mkSelector "setCanStartPictureInPictureAutomaticallyFromInline:"

