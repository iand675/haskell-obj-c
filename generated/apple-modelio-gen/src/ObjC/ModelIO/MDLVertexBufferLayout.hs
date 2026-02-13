{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLVertexBufferLayout
--
-- Describes a vertex buffer's layout
--
-- Generated bindings for @MDLVertexBufferLayout@.
module ObjC.ModelIO.MDLVertexBufferLayout
  ( MDLVertexBufferLayout
  , IsMDLVertexBufferLayout(..)
  , initWithStride
  , stride
  , setStride
  , initWithStrideSelector
  , setStrideSelector
  , strideSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithStride:@
initWithStride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> CULong -> IO (Id MDLVertexBufferLayout)
initWithStride mdlVertexBufferLayout stride =
  sendOwnedMessage mdlVertexBufferLayout initWithStrideSelector stride

-- | stride
--
-- stride in bytes of each vertex element in the buffer.
--
-- - If you store multiple attributes interleaved in the vertex               buffer, the stride will be the sum of sizes of each attribute (and any padding).             - If you store multiple attributes non-interleaved (back to back),               the stride will be the size of an attribute (and all attributes are               required to have the same size).
--
-- ObjC selector: @- stride@
stride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> IO CULong
stride mdlVertexBufferLayout =
  sendMessage mdlVertexBufferLayout strideSelector

-- | stride
--
-- stride in bytes of each vertex element in the buffer.
--
-- - If you store multiple attributes interleaved in the vertex               buffer, the stride will be the sum of sizes of each attribute (and any padding).             - If you store multiple attributes non-interleaved (back to back),               the stride will be the size of an attribute (and all attributes are               required to have the same size).
--
-- ObjC selector: @- setStride:@
setStride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> CULong -> IO ()
setStride mdlVertexBufferLayout value =
  sendMessage mdlVertexBufferLayout setStrideSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStride:@
initWithStrideSelector :: Selector '[CULong] (Id MDLVertexBufferLayout)
initWithStrideSelector = mkSelector "initWithStride:"

-- | @Selector@ for @stride@
strideSelector :: Selector '[] CULong
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector '[CULong] ()
setStrideSelector = mkSelector "setStride:"

