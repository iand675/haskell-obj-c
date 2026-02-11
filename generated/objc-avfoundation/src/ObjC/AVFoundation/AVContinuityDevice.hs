{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVContinuityDevice
--
-- An AVContinuityDevice represents a physical iOS device that provides capture devices and audio session ports.
--
-- Each instance of AVContinuityDevice corresponds to a continuity device, such as an iPhone or iPad. Instances of AVContinuityDevice cannot be created directly.
--
-- Generated bindings for @AVContinuityDevice@.
module ObjC.AVFoundation.AVContinuityDevice
  ( AVContinuityDevice
  , IsAVContinuityDevice(..)
  , init_
  , new
  , connectionID
  , connected
  , videoDevices
  , initSelector
  , newSelector
  , connectionIDSelector
  , connectedSelector
  , videoDevicesSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVContinuityDevice avContinuityDevice => avContinuityDevice -> IO (Id AVContinuityDevice)
init_ avContinuityDevice  =
  sendMsg avContinuityDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVContinuityDevice)
new  =
  do
    cls' <- getRequiredClass "AVContinuityDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | connectionID
--
-- A connection ID of the continuity device.
--
-- This property can be used to uniquely identify a continuity device. Every available continuity device has a unique ID that regenerates across device connections and disconnections.
--
-- ObjC selector: @- connectionID@
connectionID :: IsAVContinuityDevice avContinuityDevice => avContinuityDevice -> IO (Id NSUUID)
connectionID avContinuityDevice  =
  sendMsg avContinuityDevice (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | connected
--
-- Indicates whether the continuity device is connected and available to the system.
--
-- The value of this property is a BOOL indicating whether the continuity device represented by the receiver is connected and available for use. Clients can key value observe the value of this property to be notified when a continuity device is no longer available. When the value of this property becomes NO for a given instance, it will not become YES again. If the same physical continuity device again becomes available to the system, it will be represented using a new instance of AVContinuityDevice.
--
-- ObjC selector: @- connected@
connected :: IsAVContinuityDevice avContinuityDevice => avContinuityDevice -> IO Bool
connected avContinuityDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avContinuityDevice (mkSelector "connected") retCULong []

-- | videoDevices
--
-- The video capture devices available from the continuity device.
--
-- ObjC selector: @- videoDevices@
videoDevices :: IsAVContinuityDevice avContinuityDevice => avContinuityDevice -> IO (Id NSArray)
videoDevices avContinuityDevice  =
  sendMsg avContinuityDevice (mkSelector "videoDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @connected@
connectedSelector :: Selector
connectedSelector = mkSelector "connected"

-- | @Selector@ for @videoDevices@
videoDevicesSelector :: Selector
videoDevicesSelector = mkSelector "videoDevices"

