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

-- | Contains information related to shader logging.
--
-- To enable shader logging, call ``MTL4CommandBuffer/beginCommandBufferWithAllocator:options:`` with an instance of ``MTL4CommandBufferOptions`` that contains a non-@nil@ ``MTLLogState`` instance in this property.
--
-- Shader functions log messages until the command buffer ends.
--
-- ObjC selector: @- logState@
logState :: IsMTL4CommandBufferOptions mtL4CommandBufferOptions => mtL4CommandBufferOptions -> IO RawId
logState mtL4CommandBufferOptions  =
    fmap (RawId . castPtr) $ sendMsg mtL4CommandBufferOptions (mkSelector "logState") (retPtr retVoid) []

-- | Contains information related to shader logging.
--
-- To enable shader logging, call ``MTL4CommandBuffer/beginCommandBufferWithAllocator:options:`` with an instance of ``MTL4CommandBufferOptions`` that contains a non-@nil@ ``MTLLogState`` instance in this property.
--
-- Shader functions log messages until the command buffer ends.
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTL4CommandBufferOptions mtL4CommandBufferOptions => mtL4CommandBufferOptions -> RawId -> IO ()
setLogState mtL4CommandBufferOptions  value =
    sendMsg mtL4CommandBufferOptions (mkSelector "setLogState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @logState@
logStateSelector :: Selector
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector
setLogStateSelector = mkSelector "setLogState:"

