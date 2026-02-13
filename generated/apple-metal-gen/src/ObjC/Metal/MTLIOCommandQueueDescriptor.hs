{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLIOCommandQueueDescriptor
--
-- Represents a descriptor to create a MTLIOCommandQueue.
--
-- Generated bindings for @MTLIOCommandQueueDescriptor@.
module ObjC.Metal.MTLIOCommandQueueDescriptor
  ( MTLIOCommandQueueDescriptor
  , IsMTLIOCommandQueueDescriptor(..)
  , maxCommandBufferCount
  , setMaxCommandBufferCount
  , priority
  , setPriority
  , type_
  , setType
  , maxCommandsInFlight
  , setMaxCommandsInFlight
  , scratchBufferAllocator
  , setScratchBufferAllocator
  , maxCommandBufferCountSelector
  , maxCommandsInFlightSelector
  , prioritySelector
  , scratchBufferAllocatorSelector
  , setMaxCommandBufferCountSelector
  , setMaxCommandsInFlightSelector
  , setPrioritySelector
  , setScratchBufferAllocatorSelector
  , setTypeSelector
  , typeSelector

  -- * Enum types
  , MTLIOCommandQueueType(MTLIOCommandQueueType)
  , pattern MTLIOCommandQueueTypeConcurrent
  , pattern MTLIOCommandQueueTypeSerial
  , MTLIOPriority(MTLIOPriority)
  , pattern MTLIOPriorityHigh
  , pattern MTLIOPriorityNormal
  , pattern MTLIOPriorityLow

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | maxCommandBufferCount
--
-- The maximum number of commandBuffers that can be in flight at a given time for the queue.
--
-- ObjC selector: @- maxCommandBufferCount@
maxCommandBufferCount :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO CULong
maxCommandBufferCount mtlioCommandQueueDescriptor =
  sendMessage mtlioCommandQueueDescriptor maxCommandBufferCountSelector

-- | maxCommandBufferCount
--
-- The maximum number of commandBuffers that can be in flight at a given time for the queue.
--
-- ObjC selector: @- setMaxCommandBufferCount:@
setMaxCommandBufferCount :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandBufferCount mtlioCommandQueueDescriptor value =
  sendMessage mtlioCommandQueueDescriptor setMaxCommandBufferCountSelector value

-- | priority
--
-- The priority of the commands executed by this queue.
--
-- ObjC selector: @- priority@
priority :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO MTLIOPriority
priority mtlioCommandQueueDescriptor =
  sendMessage mtlioCommandQueueDescriptor prioritySelector

-- | priority
--
-- The priority of the commands executed by this queue.
--
-- ObjC selector: @- setPriority:@
setPriority :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> MTLIOPriority -> IO ()
setPriority mtlioCommandQueueDescriptor value =
  sendMessage mtlioCommandQueueDescriptor setPrioritySelector value

-- | type
--
-- The type (serial or concurrent) of the queue.
--
-- ObjC selector: @- type@
type_ :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO MTLIOCommandQueueType
type_ mtlioCommandQueueDescriptor =
  sendMessage mtlioCommandQueueDescriptor typeSelector

-- | type
--
-- The type (serial or concurrent) of the queue.
--
-- ObjC selector: @- setType:@
setType :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> MTLIOCommandQueueType -> IO ()
setType mtlioCommandQueueDescriptor value =
  sendMessage mtlioCommandQueueDescriptor setTypeSelector value

-- | maxCommandsInFlight
--
-- The maximum number of IO commands that can be in flight at a given time for the queue.
--
-- A zero value defaults to the system dependent maximum value, a smaller number can be provided to bound the utilization of the storage device.
--
-- ObjC selector: @- maxCommandsInFlight@
maxCommandsInFlight :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO CULong
maxCommandsInFlight mtlioCommandQueueDescriptor =
  sendMessage mtlioCommandQueueDescriptor maxCommandsInFlightSelector

-- | maxCommandsInFlight
--
-- The maximum number of IO commands that can be in flight at a given time for the queue.
--
-- A zero value defaults to the system dependent maximum value, a smaller number can be provided to bound the utilization of the storage device.
--
-- ObjC selector: @- setMaxCommandsInFlight:@
setMaxCommandsInFlight :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandsInFlight mtlioCommandQueueDescriptor value =
  sendMessage mtlioCommandQueueDescriptor setMaxCommandsInFlightSelector value

-- | scratchBufferAllocator
--
-- An optional property that allows setting a custom allocator for scratch buffers by the queue.
--
-- An application can manage scratch buffers manually by implemeting a class  conforming to the MTLIOScratchBufferAllocator protocol and creating an instance that is passed in here.
--
-- ObjC selector: @- scratchBufferAllocator@
scratchBufferAllocator :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO RawId
scratchBufferAllocator mtlioCommandQueueDescriptor =
  sendMessage mtlioCommandQueueDescriptor scratchBufferAllocatorSelector

-- | scratchBufferAllocator
--
-- An optional property that allows setting a custom allocator for scratch buffers by the queue.
--
-- An application can manage scratch buffers manually by implemeting a class  conforming to the MTLIOScratchBufferAllocator protocol and creating an instance that is passed in here.
--
-- ObjC selector: @- setScratchBufferAllocator:@
setScratchBufferAllocator :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> RawId -> IO ()
setScratchBufferAllocator mtlioCommandQueueDescriptor value =
  sendMessage mtlioCommandQueueDescriptor setScratchBufferAllocatorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCommandBufferCount@
maxCommandBufferCountSelector :: Selector '[] CULong
maxCommandBufferCountSelector = mkSelector "maxCommandBufferCount"

-- | @Selector@ for @setMaxCommandBufferCount:@
setMaxCommandBufferCountSelector :: Selector '[CULong] ()
setMaxCommandBufferCountSelector = mkSelector "setMaxCommandBufferCount:"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] MTLIOPriority
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[MTLIOPriority] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTLIOCommandQueueType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[MTLIOCommandQueueType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @maxCommandsInFlight@
maxCommandsInFlightSelector :: Selector '[] CULong
maxCommandsInFlightSelector = mkSelector "maxCommandsInFlight"

-- | @Selector@ for @setMaxCommandsInFlight:@
setMaxCommandsInFlightSelector :: Selector '[CULong] ()
setMaxCommandsInFlightSelector = mkSelector "setMaxCommandsInFlight:"

-- | @Selector@ for @scratchBufferAllocator@
scratchBufferAllocatorSelector :: Selector '[] RawId
scratchBufferAllocatorSelector = mkSelector "scratchBufferAllocator"

-- | @Selector@ for @setScratchBufferAllocator:@
setScratchBufferAllocatorSelector :: Selector '[RawId] ()
setScratchBufferAllocatorSelector = mkSelector "setScratchBufferAllocator:"

