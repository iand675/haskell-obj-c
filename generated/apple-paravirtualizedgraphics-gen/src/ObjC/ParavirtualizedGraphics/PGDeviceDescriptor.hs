{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PGDeviceDescriptor@.
module ObjC.ParavirtualizedGraphics.PGDeviceDescriptor
  ( PGDeviceDescriptor
  , IsPGDeviceDescriptor(..)
  , device
  , setDevice
  , mmioLength
  , setMmioLength
  , createTask
  , setCreateTask
  , destroyTask
  , setDestroyTask
  , mapMemory
  , setMapMemory
  , unmapMemory
  , setUnmapMemory
  , readMemory
  , setReadMemory
  , raiseInterrupt
  , setRaiseInterrupt
  , addTraceRange
  , setAddTraceRange
  , removeTraceRange
  , setRemoveTraceRange
  , displayPortCount
  , setDisplayPortCount
  , deviceSelector
  , setDeviceSelector
  , mmioLengthSelector
  , setMmioLengthSelector
  , createTaskSelector
  , setCreateTaskSelector
  , destroyTaskSelector
  , setDestroyTaskSelector
  , mapMemorySelector
  , setMapMemorySelector
  , unmapMemorySelector
  , setUnmapMemorySelector
  , readMemorySelector
  , setReadMemorySelector
  , raiseInterruptSelector
  , setRaiseInterruptSelector
  , addTraceRangeSelector
  , setAddTraceRangeSelector
  , removeTraceRangeSelector
  , setRemoveTraceRangeSelector
  , displayPortCountSelector
  , setDisplayPortCountSelector


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

import ObjC.ParavirtualizedGraphics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | device
--
-- The metal device to use to back the PGDevice
--
-- ObjC selector: @- device@
device :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO RawId
device pgDeviceDescriptor  =
    fmap (RawId . castPtr) $ sendMsg pgDeviceDescriptor (mkSelector "device") (retPtr retVoid) []

-- | device
--
-- The metal device to use to back the PGDevice
--
-- ObjC selector: @- setDevice:@
setDevice :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> RawId -> IO ()
setDevice pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setDevice:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | mmioLength
--
-- The length, of the memory that backs the APPLEGPU_BAR_MMIO
--
-- By default, the value of mmioLength will be the recommended default size for the MMIO memory.
--
-- ObjC selector: @- mmioLength@
mmioLength :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO CULong
mmioLength pgDeviceDescriptor  =
    sendMsg pgDeviceDescriptor (mkSelector "mmioLength") retCULong []

-- | mmioLength
--
-- The length, of the memory that backs the APPLEGPU_BAR_MMIO
--
-- By default, the value of mmioLength will be the recommended default size for the MMIO memory.
--
-- ObjC selector: @- setMmioLength:@
setMmioLength :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> CULong -> IO ()
setMmioLength pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setMmioLength:") retVoid [argCULong value]

-- | createTask
--
-- The block to invoke to create a task.
--
-- ObjC selector: @- createTask@
createTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
createTask pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "createTask") (retPtr retVoid) []

-- | createTask
--
-- The block to invoke to create a task.
--
-- ObjC selector: @- setCreateTask:@
setCreateTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setCreateTask pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setCreateTask:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | destroyTask
--
-- The block to invoke to destroy a task.
--
-- ObjC selector: @- destroyTask@
destroyTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
destroyTask pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "destroyTask") (retPtr retVoid) []

-- | destroyTask
--
-- The block to invoke to destroy a task.
--
-- ObjC selector: @- setDestroyTask:@
setDestroyTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setDestroyTask pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setDestroyTask:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | mapMemory
--
-- The block to invoke to map guest memory into a task.
--
-- ObjC selector: @- mapMemory@
mapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
mapMemory pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "mapMemory") (retPtr retVoid) []

-- | mapMemory
--
-- The block to invoke to map guest memory into a task.
--
-- ObjC selector: @- setMapMemory:@
setMapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setMapMemory pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setMapMemory:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | unmapMemory
--
-- The block to invoke to unmap guest memory from a task.
--
-- ObjC selector: @- unmapMemory@
unmapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
unmapMemory pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "unmapMemory") (retPtr retVoid) []

-- | unmapMemory
--
-- The block to invoke to unmap guest memory from a task.
--
-- ObjC selector: @- setUnmapMemory:@
setUnmapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setUnmapMemory pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setUnmapMemory:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | readMemory
--
-- The block to invoke to perform a read of guest memory
--
-- ObjC selector: @- readMemory@
readMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
readMemory pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "readMemory") (retPtr retVoid) []

-- | readMemory
--
-- The block to invoke to perform a read of guest memory
--
-- ObjC selector: @- setReadMemory:@
setReadMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setReadMemory pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setReadMemory:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | raiseInterrupt
--
-- The block to invoke to raise an interrupt to the guest.  May be raised from a dispatch queue must be thread safe.
--
-- ObjC selector: @- raiseInterrupt@
raiseInterrupt :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
raiseInterrupt pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "raiseInterrupt") (retPtr retVoid) []

-- | raiseInterrupt
--
-- The block to invoke to raise an interrupt to the guest.  May be raised from a dispatch queue must be thread safe.
--
-- ObjC selector: @- setRaiseInterrupt:@
setRaiseInterrupt :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setRaiseInterrupt pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setRaiseInterrupt:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | addTraceRange
--
-- The block to invoke to add a trace range.
--
-- If the client is unable to provide range tracing, it should not populate this property or removeTraceRange.
--
-- ObjC selector: @- addTraceRange@
addTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
addTraceRange pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "addTraceRange") (retPtr retVoid) []

-- | addTraceRange
--
-- The block to invoke to add a trace range.
--
-- If the client is unable to provide range tracing, it should not populate this property or removeTraceRange.
--
-- ObjC selector: @- setAddTraceRange:@
setAddTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setAddTraceRange pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setAddTraceRange:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | removeTraceRange
--
-- The block to invoke to remove a trace range.
--
-- This property must be populated if addTraceRange is populated.
--
-- ObjC selector: @- removeTraceRange@
removeTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
removeTraceRange pgDeviceDescriptor  =
    fmap castPtr $ sendMsg pgDeviceDescriptor (mkSelector "removeTraceRange") (retPtr retVoid) []

-- | removeTraceRange
--
-- The block to invoke to remove a trace range.
--
-- This property must be populated if addTraceRange is populated.
--
-- ObjC selector: @- setRemoveTraceRange:@
setRemoveTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setRemoveTraceRange pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setRemoveTraceRange:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | displayPortCount
--
-- The number of PGDisplay ports configured into the VM.
--
-- By default, the value of displayPortCount will be 1.  Valid values range from 1 to the value returned by PGMaxDisplayPortCount().
--
-- ObjC selector: @- displayPortCount@
displayPortCount :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO CUInt
displayPortCount pgDeviceDescriptor  =
    sendMsg pgDeviceDescriptor (mkSelector "displayPortCount") retCUInt []

-- | displayPortCount
--
-- The number of PGDisplay ports configured into the VM.
--
-- By default, the value of displayPortCount will be 1.  Valid values range from 1 to the value returned by PGMaxDisplayPortCount().
--
-- ObjC selector: @- setDisplayPortCount:@
setDisplayPortCount :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> CUInt -> IO ()
setDisplayPortCount pgDeviceDescriptor  value =
    sendMsg pgDeviceDescriptor (mkSelector "setDisplayPortCount:") retVoid [argCUInt value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @mmioLength@
mmioLengthSelector :: Selector
mmioLengthSelector = mkSelector "mmioLength"

-- | @Selector@ for @setMmioLength:@
setMmioLengthSelector :: Selector
setMmioLengthSelector = mkSelector "setMmioLength:"

-- | @Selector@ for @createTask@
createTaskSelector :: Selector
createTaskSelector = mkSelector "createTask"

-- | @Selector@ for @setCreateTask:@
setCreateTaskSelector :: Selector
setCreateTaskSelector = mkSelector "setCreateTask:"

-- | @Selector@ for @destroyTask@
destroyTaskSelector :: Selector
destroyTaskSelector = mkSelector "destroyTask"

-- | @Selector@ for @setDestroyTask:@
setDestroyTaskSelector :: Selector
setDestroyTaskSelector = mkSelector "setDestroyTask:"

-- | @Selector@ for @mapMemory@
mapMemorySelector :: Selector
mapMemorySelector = mkSelector "mapMemory"

-- | @Selector@ for @setMapMemory:@
setMapMemorySelector :: Selector
setMapMemorySelector = mkSelector "setMapMemory:"

-- | @Selector@ for @unmapMemory@
unmapMemorySelector :: Selector
unmapMemorySelector = mkSelector "unmapMemory"

-- | @Selector@ for @setUnmapMemory:@
setUnmapMemorySelector :: Selector
setUnmapMemorySelector = mkSelector "setUnmapMemory:"

-- | @Selector@ for @readMemory@
readMemorySelector :: Selector
readMemorySelector = mkSelector "readMemory"

-- | @Selector@ for @setReadMemory:@
setReadMemorySelector :: Selector
setReadMemorySelector = mkSelector "setReadMemory:"

-- | @Selector@ for @raiseInterrupt@
raiseInterruptSelector :: Selector
raiseInterruptSelector = mkSelector "raiseInterrupt"

-- | @Selector@ for @setRaiseInterrupt:@
setRaiseInterruptSelector :: Selector
setRaiseInterruptSelector = mkSelector "setRaiseInterrupt:"

-- | @Selector@ for @addTraceRange@
addTraceRangeSelector :: Selector
addTraceRangeSelector = mkSelector "addTraceRange"

-- | @Selector@ for @setAddTraceRange:@
setAddTraceRangeSelector :: Selector
setAddTraceRangeSelector = mkSelector "setAddTraceRange:"

-- | @Selector@ for @removeTraceRange@
removeTraceRangeSelector :: Selector
removeTraceRangeSelector = mkSelector "removeTraceRange"

-- | @Selector@ for @setRemoveTraceRange:@
setRemoveTraceRangeSelector :: Selector
setRemoveTraceRangeSelector = mkSelector "setRemoveTraceRange:"

-- | @Selector@ for @displayPortCount@
displayPortCountSelector :: Selector
displayPortCountSelector = mkSelector "displayPortCount"

-- | @Selector@ for @setDisplayPortCount:@
setDisplayPortCountSelector :: Selector
setDisplayPortCountSelector = mkSelector "setDisplayPortCount:"

