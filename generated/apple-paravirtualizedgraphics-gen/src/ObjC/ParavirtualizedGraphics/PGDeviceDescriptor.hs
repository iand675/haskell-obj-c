{-# LANGUAGE DataKinds #-}
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
  , addTraceRangeSelector
  , createTaskSelector
  , destroyTaskSelector
  , deviceSelector
  , displayPortCountSelector
  , mapMemorySelector
  , mmioLengthSelector
  , raiseInterruptSelector
  , readMemorySelector
  , removeTraceRangeSelector
  , setAddTraceRangeSelector
  , setCreateTaskSelector
  , setDestroyTaskSelector
  , setDeviceSelector
  , setDisplayPortCountSelector
  , setMapMemorySelector
  , setMmioLengthSelector
  , setRaiseInterruptSelector
  , setReadMemorySelector
  , setRemoveTraceRangeSelector
  , setUnmapMemorySelector
  , unmapMemorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
device pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor deviceSelector

-- | device
--
-- The metal device to use to back the PGDevice
--
-- ObjC selector: @- setDevice:@
setDevice :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> RawId -> IO ()
setDevice pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setDeviceSelector value

-- | mmioLength
--
-- The length, of the memory that backs the APPLEGPU_BAR_MMIO
--
-- By default, the value of mmioLength will be the recommended default size for the MMIO memory.
--
-- ObjC selector: @- mmioLength@
mmioLength :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO CULong
mmioLength pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor mmioLengthSelector

-- | mmioLength
--
-- The length, of the memory that backs the APPLEGPU_BAR_MMIO
--
-- By default, the value of mmioLength will be the recommended default size for the MMIO memory.
--
-- ObjC selector: @- setMmioLength:@
setMmioLength :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> CULong -> IO ()
setMmioLength pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setMmioLengthSelector value

-- | createTask
--
-- The block to invoke to create a task.
--
-- ObjC selector: @- createTask@
createTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
createTask pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor createTaskSelector

-- | createTask
--
-- The block to invoke to create a task.
--
-- ObjC selector: @- setCreateTask:@
setCreateTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setCreateTask pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setCreateTaskSelector value

-- | destroyTask
--
-- The block to invoke to destroy a task.
--
-- ObjC selector: @- destroyTask@
destroyTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
destroyTask pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor destroyTaskSelector

-- | destroyTask
--
-- The block to invoke to destroy a task.
--
-- ObjC selector: @- setDestroyTask:@
setDestroyTask :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setDestroyTask pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setDestroyTaskSelector value

-- | mapMemory
--
-- The block to invoke to map guest memory into a task.
--
-- ObjC selector: @- mapMemory@
mapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
mapMemory pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor mapMemorySelector

-- | mapMemory
--
-- The block to invoke to map guest memory into a task.
--
-- ObjC selector: @- setMapMemory:@
setMapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setMapMemory pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setMapMemorySelector value

-- | unmapMemory
--
-- The block to invoke to unmap guest memory from a task.
--
-- ObjC selector: @- unmapMemory@
unmapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
unmapMemory pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor unmapMemorySelector

-- | unmapMemory
--
-- The block to invoke to unmap guest memory from a task.
--
-- ObjC selector: @- setUnmapMemory:@
setUnmapMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setUnmapMemory pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setUnmapMemorySelector value

-- | readMemory
--
-- The block to invoke to perform a read of guest memory
--
-- ObjC selector: @- readMemory@
readMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
readMemory pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor readMemorySelector

-- | readMemory
--
-- The block to invoke to perform a read of guest memory
--
-- ObjC selector: @- setReadMemory:@
setReadMemory :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setReadMemory pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setReadMemorySelector value

-- | raiseInterrupt
--
-- The block to invoke to raise an interrupt to the guest.  May be raised from a dispatch queue must be thread safe.
--
-- ObjC selector: @- raiseInterrupt@
raiseInterrupt :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
raiseInterrupt pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor raiseInterruptSelector

-- | raiseInterrupt
--
-- The block to invoke to raise an interrupt to the guest.  May be raised from a dispatch queue must be thread safe.
--
-- ObjC selector: @- setRaiseInterrupt:@
setRaiseInterrupt :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setRaiseInterrupt pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setRaiseInterruptSelector value

-- | addTraceRange
--
-- The block to invoke to add a trace range.
--
-- If the client is unable to provide range tracing, it should not populate this property or removeTraceRange.
--
-- ObjC selector: @- addTraceRange@
addTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
addTraceRange pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor addTraceRangeSelector

-- | addTraceRange
--
-- The block to invoke to add a trace range.
--
-- If the client is unable to provide range tracing, it should not populate this property or removeTraceRange.
--
-- ObjC selector: @- setAddTraceRange:@
setAddTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setAddTraceRange pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setAddTraceRangeSelector value

-- | removeTraceRange
--
-- The block to invoke to remove a trace range.
--
-- This property must be populated if addTraceRange is populated.
--
-- ObjC selector: @- removeTraceRange@
removeTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO (Ptr ())
removeTraceRange pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor removeTraceRangeSelector

-- | removeTraceRange
--
-- The block to invoke to remove a trace range.
--
-- This property must be populated if addTraceRange is populated.
--
-- ObjC selector: @- setRemoveTraceRange:@
setRemoveTraceRange :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> Ptr () -> IO ()
setRemoveTraceRange pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setRemoveTraceRangeSelector value

-- | displayPortCount
--
-- The number of PGDisplay ports configured into the VM.
--
-- By default, the value of displayPortCount will be 1.  Valid values range from 1 to the value returned by PGMaxDisplayPortCount().
--
-- ObjC selector: @- displayPortCount@
displayPortCount :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> IO CUInt
displayPortCount pgDeviceDescriptor =
  sendMessage pgDeviceDescriptor displayPortCountSelector

-- | displayPortCount
--
-- The number of PGDisplay ports configured into the VM.
--
-- By default, the value of displayPortCount will be 1.  Valid values range from 1 to the value returned by PGMaxDisplayPortCount().
--
-- ObjC selector: @- setDisplayPortCount:@
setDisplayPortCount :: IsPGDeviceDescriptor pgDeviceDescriptor => pgDeviceDescriptor -> CUInt -> IO ()
setDisplayPortCount pgDeviceDescriptor value =
  sendMessage pgDeviceDescriptor setDisplayPortCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector '[RawId] ()
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @mmioLength@
mmioLengthSelector :: Selector '[] CULong
mmioLengthSelector = mkSelector "mmioLength"

-- | @Selector@ for @setMmioLength:@
setMmioLengthSelector :: Selector '[CULong] ()
setMmioLengthSelector = mkSelector "setMmioLength:"

-- | @Selector@ for @createTask@
createTaskSelector :: Selector '[] (Ptr ())
createTaskSelector = mkSelector "createTask"

-- | @Selector@ for @setCreateTask:@
setCreateTaskSelector :: Selector '[Ptr ()] ()
setCreateTaskSelector = mkSelector "setCreateTask:"

-- | @Selector@ for @destroyTask@
destroyTaskSelector :: Selector '[] (Ptr ())
destroyTaskSelector = mkSelector "destroyTask"

-- | @Selector@ for @setDestroyTask:@
setDestroyTaskSelector :: Selector '[Ptr ()] ()
setDestroyTaskSelector = mkSelector "setDestroyTask:"

-- | @Selector@ for @mapMemory@
mapMemorySelector :: Selector '[] (Ptr ())
mapMemorySelector = mkSelector "mapMemory"

-- | @Selector@ for @setMapMemory:@
setMapMemorySelector :: Selector '[Ptr ()] ()
setMapMemorySelector = mkSelector "setMapMemory:"

-- | @Selector@ for @unmapMemory@
unmapMemorySelector :: Selector '[] (Ptr ())
unmapMemorySelector = mkSelector "unmapMemory"

-- | @Selector@ for @setUnmapMemory:@
setUnmapMemorySelector :: Selector '[Ptr ()] ()
setUnmapMemorySelector = mkSelector "setUnmapMemory:"

-- | @Selector@ for @readMemory@
readMemorySelector :: Selector '[] (Ptr ())
readMemorySelector = mkSelector "readMemory"

-- | @Selector@ for @setReadMemory:@
setReadMemorySelector :: Selector '[Ptr ()] ()
setReadMemorySelector = mkSelector "setReadMemory:"

-- | @Selector@ for @raiseInterrupt@
raiseInterruptSelector :: Selector '[] (Ptr ())
raiseInterruptSelector = mkSelector "raiseInterrupt"

-- | @Selector@ for @setRaiseInterrupt:@
setRaiseInterruptSelector :: Selector '[Ptr ()] ()
setRaiseInterruptSelector = mkSelector "setRaiseInterrupt:"

-- | @Selector@ for @addTraceRange@
addTraceRangeSelector :: Selector '[] (Ptr ())
addTraceRangeSelector = mkSelector "addTraceRange"

-- | @Selector@ for @setAddTraceRange:@
setAddTraceRangeSelector :: Selector '[Ptr ()] ()
setAddTraceRangeSelector = mkSelector "setAddTraceRange:"

-- | @Selector@ for @removeTraceRange@
removeTraceRangeSelector :: Selector '[] (Ptr ())
removeTraceRangeSelector = mkSelector "removeTraceRange"

-- | @Selector@ for @setRemoveTraceRange:@
setRemoveTraceRangeSelector :: Selector '[Ptr ()] ()
setRemoveTraceRangeSelector = mkSelector "setRemoveTraceRange:"

-- | @Selector@ for @displayPortCount@
displayPortCountSelector :: Selector '[] CUInt
displayPortCountSelector = mkSelector "displayPortCount"

-- | @Selector@ for @setDisplayPortCount:@
setDisplayPortCountSelector :: Selector '[CUInt] ()
setDisplayPortCountSelector = mkSelector "setDisplayPortCount:"

