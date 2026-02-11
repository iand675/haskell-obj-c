{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLCommandQueueDescriptor@.
module ObjC.Metal.MTLCommandQueueDescriptor
  ( MTLCommandQueueDescriptor
  , IsMTLCommandQueueDescriptor(..)
  , maxCommandBufferCount
  , setMaxCommandBufferCount
  , logState
  , setLogState
  , maxCommandBufferCountSelector
  , setMaxCommandBufferCountSelector
  , logStateSelector
  , setLogStateSelector


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
    sendMsg mtlCommandQueueDescriptor (mkSelector "setMaxCommandBufferCount:") retVoid [argCULong value]

-- | logState
--
-- \@ Specify the MTLLogState to enable shader logging
--
-- ObjC selector: @- logState@
logState :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> IO RawId
logState mtlCommandQueueDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlCommandQueueDescriptor (mkSelector "logState") (retPtr retVoid) []

-- | logState
--
-- \@ Specify the MTLLogState to enable shader logging
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> RawId -> IO ()
setLogState mtlCommandQueueDescriptor  value =
    sendMsg mtlCommandQueueDescriptor (mkSelector "setLogState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCommandBufferCount@
maxCommandBufferCountSelector :: Selector
maxCommandBufferCountSelector = mkSelector "maxCommandBufferCount"

-- | @Selector@ for @setMaxCommandBufferCount:@
setMaxCommandBufferCountSelector :: Selector
setMaxCommandBufferCountSelector = mkSelector "setMaxCommandBufferCount:"

-- | @Selector@ for @logState@
logStateSelector :: Selector
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector
setLogStateSelector = mkSelector "setLogState:"

