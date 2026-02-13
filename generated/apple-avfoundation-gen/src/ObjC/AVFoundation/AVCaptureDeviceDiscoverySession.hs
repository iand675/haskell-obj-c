{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeviceDiscoverySession
--
-- The AVCaptureDeviceDiscoverySession allows clients to search for devices by certain criteria.
--
-- This class allows clients to discover devices by providing certain search criteria. The objective of this class is to help find devices by device type and optionally by media type or position and allow you to key-value observe changes to the returned devices list.
--
-- Generated bindings for @AVCaptureDeviceDiscoverySession@.
module ObjC.AVFoundation.AVCaptureDeviceDiscoverySession
  ( AVCaptureDeviceDiscoverySession
  , IsAVCaptureDeviceDiscoverySession(..)
  , init_
  , new
  , discoverySessionWithDeviceTypes_mediaType_position
  , devices
  , supportedMultiCamDeviceSets
  , devicesSelector
  , discoverySessionWithDeviceTypes_mediaType_positionSelector
  , initSelector
  , newSelector
  , supportedMultiCamDeviceSetsSelector

  -- * Enum types
  , AVCaptureDevicePosition(AVCaptureDevicePosition)
  , pattern AVCaptureDevicePositionUnspecified
  , pattern AVCaptureDevicePositionBack
  , pattern AVCaptureDevicePositionFront

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
init_ :: IsAVCaptureDeviceDiscoverySession avCaptureDeviceDiscoverySession => avCaptureDeviceDiscoverySession -> IO (Id AVCaptureDeviceDiscoverySession)
init_ avCaptureDeviceDiscoverySession =
  sendOwnedMessage avCaptureDeviceDiscoverySession initSelector

-- | @+ new@
new :: IO (Id AVCaptureDeviceDiscoverySession)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDeviceDiscoverySession"
    sendOwnedClassMessage cls' newSelector

-- | discoverySessionWithDeviceTypes:
--
-- Returns an AVCaptureDeviceDiscoverySession instance for the given device types, media type, and position.
--
-- @deviceTypes@ — An array specifying the device types to include in the list of discovered devices.
--
-- @mediaType@ — The media type, such as AVMediaTypeVideo, AVMediaTypeAudio, or AVMediaTypeMuxed, to include in the list of discovered devices. Pass nil to search for devices with any media type.
--
-- @position@ — The position to include in the list of discovered devices. Pass AVCaptureDevicePositionUnspecified to search for devices with any position.
--
-- Returns: The AVCaptureDeviceDiscoverySession from which the list of devices can be obtained.
--
-- The list of device types is mandatory. This is used to make sure that clients only get access to devices of types they expect. This prevents new device types from automatically being included in the list of devices.
--
-- ObjC selector: @+ discoverySessionWithDeviceTypes:mediaType:position:@
discoverySessionWithDeviceTypes_mediaType_position :: (IsNSArray deviceTypes, IsNSString mediaType) => deviceTypes -> mediaType -> AVCaptureDevicePosition -> IO (Id AVCaptureDeviceDiscoverySession)
discoverySessionWithDeviceTypes_mediaType_position deviceTypes mediaType position =
  do
    cls' <- getRequiredClass "AVCaptureDeviceDiscoverySession"
    sendClassMessage cls' discoverySessionWithDeviceTypes_mediaType_positionSelector (toNSArray deviceTypes) (toNSString mediaType) position

-- | devices
--
-- The list of devices that comply to the search criteria specified on the discovery session.
--
-- The returned array contains only devices that are available at the time the method is called. Applications can key-value observe this property to be notified when the list of available devices has changed. For apps linked against iOS 10, the devices returned are unsorted. For apps linked against iOS 11 or later, the devices are sorted by AVCaptureDeviceType, matching the order specified in the deviceTypes parameter of +[AVCaptureDeviceDiscoverySession discoverySessionWithDeviceTypes:mediaType:position:]. If a position of AVCaptureDevicePositionUnspecified is specified, the results are further ordered by position in the AVCaptureDevicePosition enum. Starting in Mac Catalyst 14.0, clients can key value observe the value of this property to be notified when the devices change.
--
-- ObjC selector: @- devices@
devices :: IsAVCaptureDeviceDiscoverySession avCaptureDeviceDiscoverySession => avCaptureDeviceDiscoverySession -> IO (Id NSArray)
devices avCaptureDeviceDiscoverySession =
  sendMessage avCaptureDeviceDiscoverySession devicesSelector

-- | supportedMultiCamDeviceSets
--
-- An array of sets of AVCaptureDevices that are allowed to be used simultaneously in an AVCaptureMultiCamSession.
--
-- When using an AVCaptureMultiCamSession, multiple cameras may be used as device inputs to the session, so long as they are included in one of the supportedMultiCamDeviceSets. Starting in Mac Catalyst 14.0, clients can key value observe the value of this property to be notified when the device sets change.
--
-- ObjC selector: @- supportedMultiCamDeviceSets@
supportedMultiCamDeviceSets :: IsAVCaptureDeviceDiscoverySession avCaptureDeviceDiscoverySession => avCaptureDeviceDiscoverySession -> IO (Id NSArray)
supportedMultiCamDeviceSets avCaptureDeviceDiscoverySession =
  sendMessage avCaptureDeviceDiscoverySession supportedMultiCamDeviceSetsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureDeviceDiscoverySession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureDeviceDiscoverySession)
newSelector = mkSelector "new"

-- | @Selector@ for @discoverySessionWithDeviceTypes:mediaType:position:@
discoverySessionWithDeviceTypes_mediaType_positionSelector :: Selector '[Id NSArray, Id NSString, AVCaptureDevicePosition] (Id AVCaptureDeviceDiscoverySession)
discoverySessionWithDeviceTypes_mediaType_positionSelector = mkSelector "discoverySessionWithDeviceTypes:mediaType:position:"

-- | @Selector@ for @devices@
devicesSelector :: Selector '[] (Id NSArray)
devicesSelector = mkSelector "devices"

-- | @Selector@ for @supportedMultiCamDeviceSets@
supportedMultiCamDeviceSetsSelector :: Selector '[] (Id NSArray)
supportedMultiCamDeviceSetsSelector = mkSelector "supportedMultiCamDeviceSets"

