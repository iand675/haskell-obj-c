{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , clockSelector
  , initSelector
  , newSelector
  , productIDSelector
  , statusSelector
  , uuidSelector
  , vendorIDSelector

  -- * Enum types
  , AVExternalSyncDeviceStatus(AVExternalSyncDeviceStatus)
  , pattern AVExternalSyncDeviceStatusUnavailable
  , pattern AVExternalSyncDeviceStatusReady
  , pattern AVExternalSyncDeviceStatusCalibrating
  , pattern AVExternalSyncDeviceStatusActiveSync
  , pattern AVExternalSyncDeviceStatusFreeRunSync

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
init_ :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Id AVExternalSyncDevice)
init_ avExternalSyncDevice =
  sendOwnedMessage avExternalSyncDevice initSelector

-- | @+ new@
new :: IO (Id AVExternalSyncDevice)
new  =
  do
    cls' <- getRequiredClass "AVExternalSyncDevice"
    sendOwnedClassMessage cls' newSelector

-- | The status of the externally connected device.
--
-- Use this property to query the current connection status of the external sync device. This property is key-value observable.
--
-- ObjC selector: @- status@
status :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO AVExternalSyncDeviceStatus
status avExternalSyncDevice =
  sendMessage avExternalSyncDevice statusSelector

-- | A clock representing the source of time from the external sync device.
--
-- This property returns @NULL@ until the ``status`` reaches ``AVExternalSyncDeviceStatusActiveSync``.
--
-- ObjC selector: @- clock@
clock :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Ptr ())
clock avExternalSyncDevice =
  sendMessage avExternalSyncDevice clockSelector

-- | A unique identifier for an external sync device.
--
-- Use this property to select a specific external sync device.
--
-- ObjC selector: @- uuid@
uuid :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO (Id NSUUID)
uuid avExternalSyncDevice =
  sendMessage avExternalSyncDevice uuidSelector

-- | The USB vendor identifier associated with the external sync device.
--
-- This @UInt32@ value is provided by the hardware vendor, and returns 0 if not available.
--
-- ObjC selector: @- vendorID@
vendorID :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO CUInt
vendorID avExternalSyncDevice =
  sendMessage avExternalSyncDevice vendorIDSelector

-- | The USB product identifier associated with the external sync device.
--
-- This @UInt32@ value comes from the hardware vendor, and returns 0 if not available. Use this value in conjunction with the ``vendorID`` to determine a specific product.
--
-- ObjC selector: @- productID@
productID :: IsAVExternalSyncDevice avExternalSyncDevice => avExternalSyncDevice -> IO CUInt
productID avExternalSyncDevice =
  sendMessage avExternalSyncDevice productIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVExternalSyncDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVExternalSyncDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVExternalSyncDeviceStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @clock@
clockSelector :: Selector '[] (Ptr ())
clockSelector = mkSelector "clock"

-- | @Selector@ for @uuid@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] CUInt
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] CUInt
productIDSelector = mkSelector "productID"

