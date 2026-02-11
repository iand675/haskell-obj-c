{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An external sync device connected to a host device that can be used to drive the timing of an internal component, such as a camera sensor.
--
-- Each instance of ``AVExternalSyncDevice`` corresponds to a physical external device that can drive an internal component, like a camera readout. You cannot create instances of ``AVExternalSyncDevice``. Instead, you obtain an array of all currently available external sync devices using ``AVExternalSyncDeviceDiscoverySession``.
--
-- Generated bindings for @AVExternalSyncDevice@.
module ObjC.AVFoundation.AVExternalSyncDevice
  ( AVExternalSyncDevice
  , IsAVExternalSyncDevice(..)
  , init_
  , new
  , status
  , clock
  , uuid
  , vendorID
  , productID
  , initSelector
  , newSelector
  , statusSelector
  , clockSelector
  , uuidSelector
  , vendorIDSelector
  , productIDSelector

  -- * Enum types
  , AVExternalSyncDeviceStatus(AVExternalSyncDeviceStatus)
  , pattern AVExternalSyncDeviceStatusUnavailable
  , pattern AVExternalSyncDeviceStatusReady
  , pattern AVExternalSyncDeviceStatusCalibrating
  , pattern AVExternalSyncDeviceStatusActiveSync
  , pattern AVExternalSyncDeviceStatusFreeRunSync

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Id AVExternalSyncDevice)
init_ avExternalSyncDevice  =
  sendMsg avExternalSyncDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVExternalSyncDevice)
new  =
  do
    cls' <- getRequiredClass "AVExternalSyncDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The status of the externally connected device.
--
-- Use this property to query the current connection status of the external sync device. This property is key-value observable.
--
-- ObjC selector: @- status@
status :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO AVExternalSyncDeviceStatus
status avExternalSyncDevice  =
  fmap (coerce :: CLong -> AVExternalSyncDeviceStatus) $ sendMsg avExternalSyncDevice (mkSelector "status") retCLong []

-- | A clock representing the source of time from the external sync device.
--
-- This property returns @NULL@ until the ``status`` reaches ``AVExternalSyncDeviceStatusActiveSync``.
--
-- ObjC selector: @- clock@
clock :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Ptr ())
clock avExternalSyncDevice  =
  fmap castPtr $ sendMsg avExternalSyncDevice (mkSelector "clock") (retPtr retVoid) []

-- | A unique identifier for an external sync device.
--
-- Use this property to select a specific external sync device.
--
-- ObjC selector: @- uuid@
uuid :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Id NSUUID)
uuid avExternalSyncDevice  =
  sendMsg avExternalSyncDevice (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The USB vendor identifier associated with the external sync device.
--
-- This @UInt32@ value is provided by the hardware vendor, and returns 0 if not available.
--
-- ObjC selector: @- vendorID@
vendorID :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO CUInt
vendorID avExternalSyncDevice  =
  sendMsg avExternalSyncDevice (mkSelector "vendorID") retCUInt []

-- | The USB product identifier associated with the external sync device.
--
-- This @UInt32@ value comes from the hardware vendor, and returns 0 if not available. Use this value in conjunction with the ``vendorID`` to determine a specific product.
--
-- ObjC selector: @- productID@
productID :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO CUInt
productID avExternalSyncDevice  =
  sendMsg avExternalSyncDevice (mkSelector "productID") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @clock@
clockSelector :: Selector
clockSelector = mkSelector "clock"

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

