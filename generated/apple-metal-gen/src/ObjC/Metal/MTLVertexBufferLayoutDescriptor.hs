{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setStepFunctionSelector
  , setStepRateSelector
  , setStrideSelector
  , stepFunctionSelector
  , stepRateSelector
  , strideSelector

  -- * Enum types
  , MTLVertexStepFunction(MTLVertexStepFunction)
  , pattern MTLVertexStepFunctionConstant
  , pattern MTLVertexStepFunctionPerVertex
  , pattern MTLVertexStepFunctionPerInstance
  , pattern MTLVertexStepFunctionPerPatch
  , pattern MTLVertexStepFunctionPerPatchControlPoint

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

-- | @- stride@
stride :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO CULong
stride mtlVertexBufferLayoutDescriptor =
  sendMessage mtlVertexBufferLayoutDescriptor strideSelector

-- | @- setStride:@
setStride :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> CULong -> IO ()
setStride mtlVertexBufferLayoutDescriptor value =
  sendMessage mtlVertexBufferLayoutDescriptor setStrideSelector value

-- | @- stepFunction@
stepFunction :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO MTLVertexStepFunction
stepFunction mtlVertexBufferLayoutDescriptor =
  sendMessage mtlVertexBufferLayoutDescriptor stepFunctionSelector

-- | @- setStepFunction:@
setStepFunction :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> MTLVertexStepFunction -> IO ()
setStepFunction mtlVertexBufferLayoutDescriptor value =
  sendMessage mtlVertexBufferLayoutDescriptor setStepFunctionSelector value

-- | @- stepRate@
stepRate :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> IO CULong
stepRate mtlVertexBufferLayoutDescriptor =
  sendMessage mtlVertexBufferLayoutDescriptor stepRateSelector

-- | @- setStepRate:@
setStepRate :: IsMTLVertexBufferLayoutDescriptor mtlVertexBufferLayoutDescriptor => mtlVertexBufferLayoutDescriptor -> CULong -> IO ()
setStepRate mtlVertexBufferLayoutDescriptor value =
  sendMessage mtlVertexBufferLayoutDescriptor setStepRateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stride@
strideSelector :: Selector '[] CULong
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector '[CULong] ()
setStrideSelector = mkSelector "setStride:"

-- | @Selector@ for @stepFunction@
stepFunctionSelector :: Selector '[] MTLVertexStepFunction
stepFunctionSelector = mkSelector "stepFunction"

-- | @Selector@ for @setStepFunction:@
setStepFunctionSelector :: Selector '[MTLVertexStepFunction] ()
setStepFunctionSelector = mkSelector "setStepFunction:"

-- | @Selector@ for @stepRate@
stepRateSelector :: Selector '[] CULong
stepRateSelector = mkSelector "stepRate"

-- | @Selector@ for @setStepRate:@
setStepRateSelector :: Selector '[CULong] ()
setStepRateSelector = mkSelector "setStepRate:"

