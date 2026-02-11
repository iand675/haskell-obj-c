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
  , strideSelector
  , setStrideSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithStride:@
initWithStride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> CULong -> IO (Id MDLVertexBufferLayout)
initWithStride mdlVertexBufferLayout  stride =
  sendMsg mdlVertexBufferLayout (mkSelector "initWithStride:") (retPtr retVoid) [argCULong (fromIntegral stride)] >>= ownedObject . castPtr

-- | stride
--
-- stride in bytes of each vertex element in the buffer.
--
-- - If you store multiple attributes interleaved in the vertex               buffer, the stride will be the sum of sizes of each attribute (and any padding).             - If you store multiple attributes non-interleaved (back to back),               the stride will be the size of an attribute (and all attributes are               required to have the same size).
--
-- ObjC selector: @- stride@
stride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> IO CULong
stride mdlVertexBufferLayout  =
  sendMsg mdlVertexBufferLayout (mkSelector "stride") retCULong []

-- | stride
--
-- stride in bytes of each vertex element in the buffer.
--
-- - If you store multiple attributes interleaved in the vertex               buffer, the stride will be the sum of sizes of each attribute (and any padding).             - If you store multiple attributes non-interleaved (back to back),               the stride will be the size of an attribute (and all attributes are               required to have the same size).
--
-- ObjC selector: @- setStride:@
setStride :: IsMDLVertexBufferLayout mdlVertexBufferLayout => mdlVertexBufferLayout -> CULong -> IO ()
setStride mdlVertexBufferLayout  value =
  sendMsg mdlVertexBufferLayout (mkSelector "setStride:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStride:@
initWithStrideSelector :: Selector
initWithStrideSelector = mkSelector "initWithStride:"

-- | @Selector@ for @stride@
strideSelector :: Selector
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector
setStrideSelector = mkSelector "setStride:"

