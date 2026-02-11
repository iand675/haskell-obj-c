{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Traditional Memory Balloon Device
--
-- This is a primitive device for managing guest memory.    This device enables memory transfer between the host and the guest as specified by the Virtio specification, which allows the guest to adapt changes    in allowance of underlying physical memory.
--
-- To request a memory balloon device operation for the memory transfer, write the targeted memory size for the virtual machine to the targetVirtualMachineMemorySize property.    When the value written to targetVirtualMachineMemorySize is less than the current value, memory will be taken away from the guest and given to the host by the amount    determined by the difference between those two values. Similarly, when the value written to targetVirtualMachineMemorySize is greater than the current value, memory will be    given back to the guest by the amount determined by the difference between those two values.
--
-- Note that any changes to targetVirtualMachineMemorySize is a request to trigger a memory balloon operation. The actual changes in memory only happen after the guest operating    system handles the request, if at all.
--
-- The targetVirtualMachineMemorySize property is initialized to VZVirtualMachineConfiguration.memorySize. The acceptable values for the targetVirtualMachineMemorySize    property range from VZVirtualMachineConfiguration.minimumAllowedMemorySize to VZVirtualMachineConfiguration.memorySize, and must be a multiple of 1 megabyte    (1024 * 1024 bytes). If those constraints aren't satisfied, targetVirtualMachineMemorySize will be rounded down to the nearest multiple of 1 megabyte, clamped to    VZVirtualMachineConfiguration.minimumAllowedMemorySize and VZVirtualMachineConfiguration.memorySize respectively.
--
-- For optimal performance, it is strongly recommended to perform a memory compaction operation in the guest (e.g. echo 1 > /proc/sys/vm/compact_memory) before invoking the device.    This helps to minimize memory fragmentation in order for the memory allocation/deallocation process to be more effective.
--
-- This device is created through instantiating a VZVirtioTraditionalMemoryBalloonDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the    VZVirtualMachine.memoryBalloonDevices property.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- Generated bindings for @VZVirtioTraditionalMemoryBalloonDevice@.
module ObjC.Virtualization.VZVirtioTraditionalMemoryBalloonDevice
  ( VZVirtioTraditionalMemoryBalloonDevice
  , IsVZVirtioTraditionalMemoryBalloonDevice(..)
  , new
  , init_
  , targetVirtualMachineMemorySize
  , setTargetVirtualMachineMemorySize
  , newSelector
  , initSelector
  , targetVirtualMachineMemorySizeSelector
  , setTargetVirtualMachineMemorySizeSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZVirtioTraditionalMemoryBalloonDevice)
new  =
  do
    cls' <- getRequiredClass "VZVirtioTraditionalMemoryBalloonDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioTraditionalMemoryBalloonDevice vzVirtioTraditionalMemoryBalloonDevice => vzVirtioTraditionalMemoryBalloonDevice -> IO (Id VZVirtioTraditionalMemoryBalloonDevice)
init_ vzVirtioTraditionalMemoryBalloonDevice  =
  sendMsg vzVirtioTraditionalMemoryBalloonDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Target memory size for the virtual machine in bytes.
--
-- The targetVirtualMachineMemorySize must be a multiple of 1 megabyte (1024 * 1024 bytes) between VZVirtualMachineConfiguration.minimumAllowedMemorySize    and VZVirtualMachineConfiguration.memorySize. If those constraints aren't satisfied, targetVirtualMachineMemorySize will be rounded down to the nearest multiple of    1 megabyte, clamped to VZVirtualMachineConfiguration.minimumAllowedMemorySize and VZVirtualMachineConfiguration.memorySize respectively.
--
-- The targetVirtualMachineMemorySize represents the amount of physical memory to be made available to the guest.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedMemorySize
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @- targetVirtualMachineMemorySize@
targetVirtualMachineMemorySize :: IsVZVirtioTraditionalMemoryBalloonDevice vzVirtioTraditionalMemoryBalloonDevice => vzVirtioTraditionalMemoryBalloonDevice -> IO CULong
targetVirtualMachineMemorySize vzVirtioTraditionalMemoryBalloonDevice  =
  sendMsg vzVirtioTraditionalMemoryBalloonDevice (mkSelector "targetVirtualMachineMemorySize") retCULong []

-- | Target memory size for the virtual machine in bytes.
--
-- The targetVirtualMachineMemorySize must be a multiple of 1 megabyte (1024 * 1024 bytes) between VZVirtualMachineConfiguration.minimumAllowedMemorySize    and VZVirtualMachineConfiguration.memorySize. If those constraints aren't satisfied, targetVirtualMachineMemorySize will be rounded down to the nearest multiple of    1 megabyte, clamped to VZVirtualMachineConfiguration.minimumAllowedMemorySize and VZVirtualMachineConfiguration.memorySize respectively.
--
-- The targetVirtualMachineMemorySize represents the amount of physical memory to be made available to the guest.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedMemorySize
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @- setTargetVirtualMachineMemorySize:@
setTargetVirtualMachineMemorySize :: IsVZVirtioTraditionalMemoryBalloonDevice vzVirtioTraditionalMemoryBalloonDevice => vzVirtioTraditionalMemoryBalloonDevice -> CULong -> IO ()
setTargetVirtualMachineMemorySize vzVirtioTraditionalMemoryBalloonDevice  value =
  sendMsg vzVirtioTraditionalMemoryBalloonDevice (mkSelector "setTargetVirtualMachineMemorySize:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @targetVirtualMachineMemorySize@
targetVirtualMachineMemorySizeSelector :: Selector
targetVirtualMachineMemorySizeSelector = mkSelector "targetVirtualMachineMemorySize"

-- | @Selector@ for @setTargetVirtualMachineMemorySize:@
setTargetVirtualMachineMemorySizeSelector :: Selector
setTargetVirtualMachineMemorySizeSelector = mkSelector "setTargetVirtualMachineMemorySize:"

