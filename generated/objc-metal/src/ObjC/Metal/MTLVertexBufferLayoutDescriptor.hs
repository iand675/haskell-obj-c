{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVertexBufferLayoutDescriptor@.
module ObjC.Metal.MTLVertexBufferLayoutDescriptor
  ( MTLVertexBufferLayoutDescriptor
  , IsMTLVertexBufferLayoutDescriptor(..)
  , stride
  , setStride
  , stepFunction
  , setStepFunction
  , stepRate
  , setStepRate
  , strideSelector
  , setStrideSelector
  , stepFunctionSelector
  , setStepFunctionSelector
  , stepRateSelector
  , setStepRateSelector

  -- * Enum types
  , MTLVertexStepFunction(MTLVertexStepFunction)
  , pattern MTLVertexStepFunctionConstant
  , pattern MTLVertexStepFunctionPerVertex
  , pattern MTLVertexStepFunctionPerInstance
  , pattern MTLVertexStepFunctionPerPatch
  , pattern MTLVertexStepFunctionPerPatchControlPoint

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

-- | @- stride@
stride :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO CULong
stride mtlVertexBufferLayoutDescriptor  =
  sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "stride") retCULong []

-- | @- setStride:@
setStride :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> CULong -> IO ()
setStride mtlVertexBufferLayoutDescriptor  value =
  sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "setStride:") retVoid [argCULong (fromIntegral value)]

-- | @- stepFunction@
stepFunction :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO MTLVertexStepFunction
stepFunction mtlVertexBufferLayoutDescriptor  =
  fmap (coerce :: CULong -> MTLVertexStepFunction) $ sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "stepFunction") retCULong []

-- | @- setStepFunction:@
setStepFunction :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> MTLVertexStepFunction -> IO ()
setStepFunction mtlVertexBufferLayoutDescriptor  value =
  sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "setStepFunction:") retVoid [argCULong (coerce value)]

-- | @- stepRate@
stepRate :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO CULong
stepRate mtlVertexBufferLayoutDescriptor  =
  sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "stepRate") retCULong []

-- | @- setStepRate:@
setStepRate :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> CULong -> IO ()
setStepRate mtlVertexBufferLayoutDescriptor  value =
  sendMsg mtlVertexBufferLayoutDescriptor (mkSelector "setStepRate:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stride@
strideSelector :: Selector
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector
setStrideSelector = mkSelector "setStride:"

-- | @Selector@ for @stepFunction@
stepFunctionSelector :: Selector
stepFunctionSelector = mkSelector "stepFunction"

-- | @Selector@ for @setStepFunction:@
setStepFunctionSelector :: Selector
setStepFunctionSelector = mkSelector "setStepFunction:"

-- | @Selector@ for @stepRate@
stepRateSelector :: Selector
stepRateSelector = mkSelector "stepRate"

-- | @Selector@ for @setStepRate:@
setStepRateSelector :: Selector
setStepRateSelector = mkSelector "setStepRate:"

