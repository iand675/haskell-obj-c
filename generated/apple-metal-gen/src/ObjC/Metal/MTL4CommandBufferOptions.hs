{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Options to configure a command buffer before encoding work into it.
--
-- Generated bindings for @MTL4CommandBufferOptions@.
module ObjC.Metal.MTL4CommandBufferOptions
  ( MTL4CommandBufferOptions
  , IsMTL4CommandBufferOptions(..)
  , logState
  , setLogState
  , logStateSelector
  , setLogStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Contains information related to shader logging.
--
-- To enable shader logging, call ``MTL4CommandBuffer/beginCommandBufferWithAllocator:options:`` with an instance of ``MTL4CommandBufferOptions`` that contains a non-@nil@ ``MTLLogState`` instance in this property.
--
-- Shader functions log messages until the command buffer ends.
--
-- ObjC selector: @- logState@
logState :: IsMTL4CommandBufferOptions mtL4CommandBufferOptions => mtL4CommandBufferOptions -> IO RawId
logState mtL4CommandBufferOptions =
  sendMessage mtL4CommandBufferOptions logStateSelector

-- | Contains information related to shader logging.
--
-- To enable shader logging, call ``MTL4CommandBuffer/beginCommandBufferWithAllocator:options:`` with an instance of ``MTL4CommandBufferOptions`` that contains a non-@nil@ ``MTLLogState`` instance in this property.
--
-- Shader functions log messages until the command buffer ends.
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTL4CommandBufferOptions mtL4CommandBufferOptions => mtL4CommandBufferOptions -> RawId -> IO ()
setLogState mtL4CommandBufferOptions value =
  sendMessage mtL4CommandBufferOptions setLogStateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @logState@
logStateSelector :: Selector '[] RawId
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector '[RawId] ()
setLogStateSelector = mkSelector "setLogState:"

