{-# LANGUAGE PatternSynonyms #-}
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
  , maxCommandBufferCountSelector
  , setMaxCommandBufferCountSelector
  , prioritySelector
  , setPrioritySelector
  , typeSelector
  , setTypeSelector
  , maxCommandsInFlightSelector
  , setMaxCommandsInFlightSelector

  -- * Enum types
  , MTLIOCommandQueueType(MTLIOCommandQueueType)
  , pattern MTLIOCommandQueueTypeConcurrent
  , pattern MTLIOCommandQueueTypeSerial
  , MTLIOPriority(MTLIOPriority)
  , pattern MTLIOPriorityHigh
  , pattern MTLIOPriorityNormal
  , pattern MTLIOPriorityLow

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | maxCommandBufferCount
--
-- The maximum number of commandBuffers that can be in flight at a given time for the queue.
--
-- ObjC selector: @- maxCommandBufferCount@
maxCommandBufferCount :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO CULong
maxCommandBufferCount mtlioCommandQueueDescriptor  =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "maxCommandBufferCount") retCULong []

-- | maxCommandBufferCount
--
-- The maximum number of commandBuffers that can be in flight at a given time for the queue.
--
-- ObjC selector: @- setMaxCommandBufferCount:@
setMaxCommandBufferCount :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandBufferCount mtlioCommandQueueDescriptor  value =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "setMaxCommandBufferCount:") retVoid [argCULong (fromIntegral value)]

-- | priority
--
-- The priority of the commands executed by this queue.
--
-- ObjC selector: @- priority@
priority :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO MTLIOPriority
priority mtlioCommandQueueDescriptor  =
  fmap (coerce :: CLong -> MTLIOPriority) $ sendMsg mtlioCommandQueueDescriptor (mkSelector "priority") retCLong []

-- | priority
--
-- The priority of the commands executed by this queue.
--
-- ObjC selector: @- setPriority:@
setPriority :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> MTLIOPriority -> IO ()
setPriority mtlioCommandQueueDescriptor  value =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "setPriority:") retVoid [argCLong (coerce value)]

-- | type
--
-- The type (serial or concurrent) of the queue.
--
-- ObjC selector: @- type@
type_ :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO MTLIOCommandQueueType
type_ mtlioCommandQueueDescriptor  =
  fmap (coerce :: CLong -> MTLIOCommandQueueType) $ sendMsg mtlioCommandQueueDescriptor (mkSelector "type") retCLong []

-- | type
--
-- The type (serial or concurrent) of the queue.
--
-- ObjC selector: @- setType:@
setType :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> MTLIOCommandQueueType -> IO ()
setType mtlioCommandQueueDescriptor  value =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | maxCommandsInFlight
--
-- The maximum number of IO commands that can be in flight at a given time for the queue.
--
-- A zero value defaults to the system dependent maximum value, a smaller number can be provided to bound the utilization of the storage device.
--
-- ObjC selector: @- maxCommandsInFlight@
maxCommandsInFlight :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> IO CULong
maxCommandsInFlight mtlioCommandQueueDescriptor  =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "maxCommandsInFlight") retCULong []

-- | maxCommandsInFlight
--
-- The maximum number of IO commands that can be in flight at a given time for the queue.
--
-- A zero value defaults to the system dependent maximum value, a smaller number can be provided to bound the utilization of the storage device.
--
-- ObjC selector: @- setMaxCommandsInFlight:@
setMaxCommandsInFlight :: IsMTLIOCommandQueueDescriptor mtlioCommandQueueDescriptor => mtlioCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandsInFlight mtlioCommandQueueDescriptor  value =
  sendMsg mtlioCommandQueueDescriptor (mkSelector "setMaxCommandsInFlight:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCommandBufferCount@
maxCommandBufferCountSelector :: Selector
maxCommandBufferCountSelector = mkSelector "maxCommandBufferCount"

-- | @Selector@ for @setMaxCommandBufferCount:@
setMaxCommandBufferCountSelector :: Selector
setMaxCommandBufferCountSelector = mkSelector "setMaxCommandBufferCount:"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @maxCommandsInFlight@
maxCommandsInFlightSelector :: Selector
maxCommandsInFlightSelector = mkSelector "maxCommandsInFlight"

-- | @Selector@ for @setMaxCommandsInFlight:@
setMaxCommandsInFlightSelector :: Selector
setMaxCommandsInFlightSelector = mkSelector "setMaxCommandsInFlight:"

