{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object associated with a capture device that monitors the scene and suggests an optimal framing.
--
-- A smart framing monitor observes its associated device for objects of interest entering and exiting the camera's field of view and recommends an optimal framing for good photographic composition. This framing recommendation consists of an aspect ratio and zoom factor. You may respond to the device's framing recommendation by calling ``AVCaptureDevice/setDynamicAspectRatio:completionHandler:`` and setting ``AVCaptureDevice/videoZoomFactor`` on the associated device in whatever order best matches your animation between old and new framings.
--
-- Generated bindings for @AVCaptureSmartFramingMonitor@.
module ObjC.AVFoundation.AVCaptureSmartFramingMonitor
  ( AVCaptureSmartFramingMonitor
  , IsAVCaptureSmartFramingMonitor(..)
  , init_
  , new
  , startMonitoringWithError
  , stopMonitoring
  , supportedFramings
  , enabledFramings
  , setEnabledFramings
  , recommendedFraming
  , monitoring
  , enabledFramingsSelector
  , initSelector
  , monitoringSelector
  , newSelector
  , recommendedFramingSelector
  , setEnabledFramingsSelector
  , startMonitoringWithErrorSelector
  , stopMonitoringSelector
  , supportedFramingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO (Id AVCaptureSmartFramingMonitor)
init_ avCaptureSmartFramingMonitor =
  sendOwnedMessage avCaptureSmartFramingMonitor initSelector

-- | @+ new@
new :: IO (Id AVCaptureSmartFramingMonitor)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSmartFramingMonitor"
    sendOwnedClassMessage cls' newSelector

-- | Begins monitoring the device's active scene and making framing recommendations.
--
-- - Parameter outError: A pointer to an ``NSError`` indicating why ``startMonitoringWithError:`` failed, or to a @nil@ ``NSError`` on success. - Returns: @true@ if successful, @false@ if monitoring could not be started.
--
-- The monitor's ``recommendedFraming`` is @nil@ when it is not actively running. Call this method to start monitoring. You may start monitoring before or after calling ``AVCaptureSession/startRunning``,  and you may stop active monitoring without stopping the capture session by calling ``stopMonitoring`` at any time, but you must set ``enabledFramings`` before running your capture session so that the monitor is prepared for your desired framing recommendations. While the monitor is running, you may set ``enabledFramings`` at any time to change the framing choices the monitor should consider in its recommendations.
--
-- ObjC selector: @- startMonitoringWithError:@
startMonitoringWithError :: (IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor, IsNSError outError) => avCaptureSmartFramingMonitor -> outError -> IO Bool
startMonitoringWithError avCaptureSmartFramingMonitor outError =
  sendMessage avCaptureSmartFramingMonitor startMonitoringWithErrorSelector (toNSError outError)

-- | Stops monitoring the device's active scene and making framing recommendations.
--
-- The monitor's ``recommendedFraming`` is @nil@ when it is not actively running. Call this method to stop actively monitoring the scene and making framing recommendations. You may start monitoring before or after calling ``AVCaptureSession/startRunning``, and may stop active monitoring without stopping the capture session by calling ``stopMonitoring`` at any time.
--
-- ObjC selector: @- stopMonitoring@
stopMonitoring :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO ()
stopMonitoring avCaptureSmartFramingMonitor =
  sendMessage avCaptureSmartFramingMonitor stopMonitoringSelector

-- | An array of framings supported by the monitor in its current configuration.
--
-- The monitor is capable of recommending any of the framings in this array. This property is key-value observable and may change as the target capture device's ``AVCaptureDevice/activeFormat`` property changes. This array contains the full set of framings supported by the monitor in the device's current configuration. You must tell the monitor which smart framings you are interested in having recommended to you by setting the ``enabledFramings`` property.
--
-- ObjC selector: @- supportedFramings@
supportedFramings :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO (Id NSArray)
supportedFramings avCaptureSmartFramingMonitor =
  sendMessage avCaptureSmartFramingMonitor supportedFramingsSelector

-- | An array of framings that the monitor is allowed to suggest.
--
-- The monitor is capable of recommending any of the framings in the ``supportedFramings`` array. This property contains the subset of ``supportedFramings`` you would like to have recommended to you. You may set this property at any time while running your ``AVCaptureSession``. This property's default value is the empty array.
--
-- ObjC selector: @- enabledFramings@
enabledFramings :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO (Id NSArray)
enabledFramings avCaptureSmartFramingMonitor =
  sendMessage avCaptureSmartFramingMonitor enabledFramingsSelector

-- | An array of framings that the monitor is allowed to suggest.
--
-- The monitor is capable of recommending any of the framings in the ``supportedFramings`` array. This property contains the subset of ``supportedFramings`` you would like to have recommended to you. You may set this property at any time while running your ``AVCaptureSession``. This property's default value is the empty array.
--
-- ObjC selector: @- setEnabledFramings:@
setEnabledFramings :: (IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor, IsNSArray value) => avCaptureSmartFramingMonitor -> value -> IO ()
setEnabledFramings avCaptureSmartFramingMonitor value =
  sendMessage avCaptureSmartFramingMonitor setEnabledFramingsSelector (toNSArray value)

-- | The latest recommended framing from the monitor.
--
-- While your ``AVCaptureSession`` is running, the monitor continuously observes its device's scene to recommend the best framing. This recommended framing is always one of the values in ``enabledFramings``. This property may return @nil@ if smart framing isn't supported for the device in its current configuration. Its default value is @nil@. This property is key-value observable, and when you observe a change, you may respond to the new recommendation by calling ``AVCaptureDevice/setDynamicAspectRatio:completionHandler:`` and setting ``AVCaptureDevice/videoZoomFactor`` on the associated device in whatever order best matches your animation between old and new framings.
--
-- ObjC selector: @- recommendedFraming@
recommendedFraming :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO (Id AVCaptureFraming)
recommendedFraming avCaptureSmartFramingMonitor =
  sendMessage avCaptureSmartFramingMonitor recommendedFramingSelector

-- | Yes when the receiver is actively monitoring.
--
-- See ``startMonitoringWithError:`` and ``stopMonitoring``.
--
-- ObjC selector: @- monitoring@
monitoring :: IsAVCaptureSmartFramingMonitor avCaptureSmartFramingMonitor => avCaptureSmartFramingMonitor -> IO Bool
monitoring avCaptureSmartFramingMonitor =
  sendMessage avCaptureSmartFramingMonitor monitoringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureSmartFramingMonitor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureSmartFramingMonitor)
newSelector = mkSelector "new"

-- | @Selector@ for @startMonitoringWithError:@
startMonitoringWithErrorSelector :: Selector '[Id NSError] Bool
startMonitoringWithErrorSelector = mkSelector "startMonitoringWithError:"

-- | @Selector@ for @stopMonitoring@
stopMonitoringSelector :: Selector '[] ()
stopMonitoringSelector = mkSelector "stopMonitoring"

-- | @Selector@ for @supportedFramings@
supportedFramingsSelector :: Selector '[] (Id NSArray)
supportedFramingsSelector = mkSelector "supportedFramings"

-- | @Selector@ for @enabledFramings@
enabledFramingsSelector :: Selector '[] (Id NSArray)
enabledFramingsSelector = mkSelector "enabledFramings"

-- | @Selector@ for @setEnabledFramings:@
setEnabledFramingsSelector :: Selector '[Id NSArray] ()
setEnabledFramingsSelector = mkSelector "setEnabledFramings:"

-- | @Selector@ for @recommendedFraming@
recommendedFramingSelector :: Selector '[] (Id AVCaptureFraming)
recommendedFramingSelector = mkSelector "recommendedFraming"

-- | @Selector@ for @monitoring@
monitoringSelector :: Selector '[] Bool
monitoringSelector = mkSelector "monitoring"

