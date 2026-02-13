{-# LANGUAGE DataKinds #-}
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
  , logStateSelector
  , maxCommandBufferCountSelector
  , setLogStateSelector
  , setMaxCommandBufferCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
maxCommandBufferCount mtlCommandQueueDescriptor =
  sendMessage mtlCommandQueueDescriptor maxCommandBufferCountSelector

-- | maxCommandBufferCount
--
-- \@ Specify upper bound on uncompleted command buffers that may be enqueued on this queue
--
-- ObjC selector: @- setMaxCommandBufferCount:@
setMaxCommandBufferCount :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> CULong -> IO ()
setMaxCommandBufferCount mtlCommandQueueDescriptor value =
  sendMessage mtlCommandQueueDescriptor setMaxCommandBufferCountSelector value

-- | logState
--
-- \@ Specify the MTLLogState to enable shader logging
--
-- ObjC selector: @- logState@
logState :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> IO RawId
logState mtlCommandQueueDescriptor =
  sendMessage mtlCommandQueueDescriptor logStateSelector

-- | logState
--
-- \@ Specify the MTLLogState to enable shader logging
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTLCommandQueueDescriptor mtlCommandQueueDescriptor => mtlCommandQueueDescriptor -> RawId -> IO ()
setLogState mtlCommandQueueDescriptor value =
  sendMessage mtlCommandQueueDescriptor setLogStateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCommandBufferCount@
maxCommandBufferCountSelector :: Selector '[] CULong
maxCommandBufferCountSelector = mkSelector "maxCommandBufferCount"

-- | @Selector@ for @setMaxCommandBufferCount:@
setMaxCommandBufferCountSelector :: Selector '[CULong] ()
setMaxCommandBufferCountSelector = mkSelector "setMaxCommandBufferCount:"

-- | @Selector@ for @logState@
logStateSelector :: Selector '[] RawId
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector '[RawId] ()
setLogStateSelector = mkSelector "setLogState:"

