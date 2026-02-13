{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCapturePhotoOutputReadinessCoordinator
--
-- AVCapturePhotoOutputReadinessCoordinator notifies its delegate of changes in an AVCapturePhotoOutput's captureReadiness property and can be used to coordinate UI updates on the main queue with use of AVCapturePhotoOutput on a background queue.
--
-- AVCapturePhotoOutputReadinessCoordinator tracks its output's captureReadiness and incorporates additional requests registered via -startTrackingCaptureRequestUsingPhotoSettings:. This allows clients to synchronously update shutter button availability and appearance and on the main thread while calling -[AVCapturePhotoOutput capturePhotoWithSettings:delegate:] asynchronously on a background queue.
--
-- Generated bindings for @AVCapturePhotoOutputReadinessCoordinator@.
module ObjC.AVFoundation.AVCapturePhotoOutputReadinessCoordinator
  ( AVCapturePhotoOutputReadinessCoordinator
  , IsAVCapturePhotoOutputReadinessCoordinator(..)
  , init_
  , new
  , initWithPhotoOutput
  , startTrackingCaptureRequestUsingPhotoSettings
  , stopTrackingCaptureRequestUsingPhotoSettingsUniqueID
  , delegate
  , setDelegate
  , captureReadiness
  , captureReadinessSelector
  , delegateSelector
  , initSelector
  , initWithPhotoOutputSelector
  , newSelector
  , setDelegateSelector
  , startTrackingCaptureRequestUsingPhotoSettingsSelector
  , stopTrackingCaptureRequestUsingPhotoSettingsUniqueIDSelector

  -- * Enum types
  , AVCapturePhotoOutputCaptureReadiness(AVCapturePhotoOutputCaptureReadiness)
  , pattern AVCapturePhotoOutputCaptureReadinessSessionNotRunning
  , pattern AVCapturePhotoOutputCaptureReadinessReady
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyMomentarily
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyWaitingForCapture
  , pattern AVCapturePhotoOutputCaptureReadinessNotReadyWaitingForProcessing

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator => avCapturePhotoOutputReadinessCoordinator -> IO (Id AVCapturePhotoOutputReadinessCoordinator)
init_ avCapturePhotoOutputReadinessCoordinator =
  sendOwnedMessage avCapturePhotoOutputReadinessCoordinator initSelector

-- | @+ new@
new :: IO (Id AVCapturePhotoOutputReadinessCoordinator)
new  =
  do
    cls' <- getRequiredClass "AVCapturePhotoOutputReadinessCoordinator"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithPhotoOutput:@
initWithPhotoOutput :: (IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator, IsAVCapturePhotoOutput photoOutput) => avCapturePhotoOutputReadinessCoordinator -> photoOutput -> IO (Id AVCapturePhotoOutputReadinessCoordinator)
initWithPhotoOutput avCapturePhotoOutputReadinessCoordinator photoOutput =
  sendOwnedMessage avCapturePhotoOutputReadinessCoordinator initWithPhotoOutputSelector (toAVCapturePhotoOutput photoOutput)

-- | startTrackingCaptureRequestUsingPhotoSettings:
--
-- Track the capture request represented by the specified photo settings until it is enqueued to the photo output and update captureReadiness to include this request.
--
-- @settings@ — The AVCapturePhotoSettings which will be passed to -[AVCapturePhotoOutput capturePhotoWithSettings:delegate] for this capture request.
--
-- The captureReadiness property is updated to include the tracked request until the the photo output receives a settings object with the same or a newer uniqueID. It is recommended that the same photo settings be passed to -[AVCapturePhotoOutput capturePhotoWithSettings:delegate] to ensure the captureReadiness value is consistent once the capture begins. When called on the main queue the delegate callback is invoked synchronously before returning to ensure shutter availability is updated immediately and prevent queued touch events from initiating unwanted captures. The -startTrackingCaptureRequestUsingPhotoSettings: method can be called while in the SessionNotRunning state to allow the shutter button to be interactive while the session is being started on a background queue. An NSInvalidArgumentException is thrown if the photo settings are invalid.
--
-- ObjC selector: @- startTrackingCaptureRequestUsingPhotoSettings:@
startTrackingCaptureRequestUsingPhotoSettings :: (IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator, IsAVCapturePhotoSettings settings) => avCapturePhotoOutputReadinessCoordinator -> settings -> IO ()
startTrackingCaptureRequestUsingPhotoSettings avCapturePhotoOutputReadinessCoordinator settings =
  sendMessage avCapturePhotoOutputReadinessCoordinator startTrackingCaptureRequestUsingPhotoSettingsSelector (toAVCapturePhotoSettings settings)

-- | stopTrackingCaptureRequestUsingPhotoSettingsUniqueID:
--
-- Stop tracking the capture request represented by the specified photo settings uniqueID and update captureReadiness to no longer include this request.
--
-- @settingsUniqueID@ — The AVCapturePhotoSettings.uniqueID of the settings passed to -startTrackingCaptureRequestUsingPhotoSettings:.
--
-- Tracking automatically stops when -[AVCapturePhotoOutput capturePhotoWithSettings:delegate] is called with a photo settings objects with the same or a newer uniqueID, but in cases where an error or other condition prevents calling -capturePhotoWithSettings:delegate tracking should be explicitly stopped to ensure the captureReadiness value is up to date. When called on the main queue the delegate callback is invoked synchronously before returning to ensure shutter availability is updated immediately.
--
-- ObjC selector: @- stopTrackingCaptureRequestUsingPhotoSettingsUniqueID:@
stopTrackingCaptureRequestUsingPhotoSettingsUniqueID :: IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator => avCapturePhotoOutputReadinessCoordinator -> CLong -> IO ()
stopTrackingCaptureRequestUsingPhotoSettingsUniqueID avCapturePhotoOutputReadinessCoordinator settingsUniqueID =
  sendMessage avCapturePhotoOutputReadinessCoordinator stopTrackingCaptureRequestUsingPhotoSettingsUniqueIDSelector settingsUniqueID

-- | delegate
--
-- The receiver's delegate, called on the main queue.
--
-- The value of this property is an object conforming to the AVCapturePhotoOutputReadinessCoordinatorDelegate protocol that will receive a callback when the captureReadiness property changes. Callbacks are delivered on the main queue, allowing UI updates to be done directly in the callback. A callback with the initial value of captureReadiness is delivered when delegate is set.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator => avCapturePhotoOutputReadinessCoordinator -> IO RawId
delegate avCapturePhotoOutputReadinessCoordinator =
  sendMessage avCapturePhotoOutputReadinessCoordinator delegateSelector

-- | delegate
--
-- The receiver's delegate, called on the main queue.
--
-- The value of this property is an object conforming to the AVCapturePhotoOutputReadinessCoordinatorDelegate protocol that will receive a callback when the captureReadiness property changes. Callbacks are delivered on the main queue, allowing UI updates to be done directly in the callback. A callback with the initial value of captureReadiness is delivered when delegate is set.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator => avCapturePhotoOutputReadinessCoordinator -> RawId -> IO ()
setDelegate avCapturePhotoOutputReadinessCoordinator value =
  sendMessage avCapturePhotoOutputReadinessCoordinator setDelegateSelector value

-- | captureReadiness
--
-- A value specifying whether the coordinator's photo output is ready to respond to new capture requests in a timely manner.
--
-- The value incorporates the photo output's captureReadiness and any requests registered using -startTrackingCaptureRequestUsingPhotoSettings:. The value is updated before calling the -readinessCoordinator:captureReadinessDidChange: callback. See AVCapturePhotoOutput's captureReadiness documentation for a discussion of how to update shutter availability and appearance based on the captureReadiness value. This property is key-value observable and all change notifications are delivered on the main queue, allowing UI updates to be done directly in the callback.
--
-- ObjC selector: @- captureReadiness@
captureReadiness :: IsAVCapturePhotoOutputReadinessCoordinator avCapturePhotoOutputReadinessCoordinator => avCapturePhotoOutputReadinessCoordinator -> IO AVCapturePhotoOutputCaptureReadiness
captureReadiness avCapturePhotoOutputReadinessCoordinator =
  sendMessage avCapturePhotoOutputReadinessCoordinator captureReadinessSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCapturePhotoOutputReadinessCoordinator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCapturePhotoOutputReadinessCoordinator)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPhotoOutput:@
initWithPhotoOutputSelector :: Selector '[Id AVCapturePhotoOutput] (Id AVCapturePhotoOutputReadinessCoordinator)
initWithPhotoOutputSelector = mkSelector "initWithPhotoOutput:"

-- | @Selector@ for @startTrackingCaptureRequestUsingPhotoSettings:@
startTrackingCaptureRequestUsingPhotoSettingsSelector :: Selector '[Id AVCapturePhotoSettings] ()
startTrackingCaptureRequestUsingPhotoSettingsSelector = mkSelector "startTrackingCaptureRequestUsingPhotoSettings:"

-- | @Selector@ for @stopTrackingCaptureRequestUsingPhotoSettingsUniqueID:@
stopTrackingCaptureRequestUsingPhotoSettingsUniqueIDSelector :: Selector '[CLong] ()
stopTrackingCaptureRequestUsingPhotoSettingsUniqueIDSelector = mkSelector "stopTrackingCaptureRequestUsingPhotoSettingsUniqueID:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @captureReadiness@
captureReadinessSelector :: Selector '[] AVCapturePhotoOutputCaptureReadiness
captureReadinessSelector = mkSelector "captureReadiness"

