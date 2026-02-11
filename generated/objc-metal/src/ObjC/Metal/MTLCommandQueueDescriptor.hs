{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLCommandQueueDescriptor@.
module ObjC.Metal.MTLCommandQueueDescriptor
  ( MTLCommandQueueDescriptor
  , IsMTLCommandQueueDescriptor(..)
  , maxCommandBufferCount
  , setMaxCommandBufferCount
  , maxCommandBufferCountSelector
  , setMaxCommandBufferCountSelector


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
import ObjC.Foundation.Internal.Classes

-- | maxCommandBufferCount
--
-- \@ Specify upper bound on uncompleted command buffers that may be enqueued on this queue
--
-- ObjC selector: @- maxCommandBufferCount@
maxCommandBufferCount :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> IO CULong
maxCommandBufferCount mtlCommandQueueDescriptor  =
  sendMsg mtlCommandQueueDescriptor (mkSelector "maxCommandBufferCount") retCULong []

-- | maxCommandBufferCount
--
-- \@ Specify upper bound on uncompleted command buffers that may be enqueued on this queue
--
-- ObjC selector: @- setMaxCommandBufferCount:@
setMaxCommandBufferCount :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandBufferCount mtlCommandQueueDescriptor  value =
  sendMsg mtlCommandQueueDescriptor (mkSelector "setMaxCommandBufferCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCommandBufferCount@
maxCommandBufferCountSelector :: Selector
maxCommandBufferCountSelector = mkSelector "maxCommandBufferCount"

-- | @Selector@ for @setMaxCommandBufferCount:@
setMaxCommandBufferCountSelector :: Selector
setMaxCommandBufferCountSelector = mkSelector "setMaxCommandBufferCount:"

