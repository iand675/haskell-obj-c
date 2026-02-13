{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLBufferLayoutDescriptor@.
module ObjC.Metal.MTLBufferLayoutDescriptor
  ( MTLBufferLayoutDescriptor
  , IsMTLBufferLayoutDescriptor(..)
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
  , MTLStepFunction(MTLStepFunction)
  , pattern MTLStepFunctionConstant
  , pattern MTLStepFunctionPerVertex
  , pattern MTLStepFunctionPerInstance
  , pattern MTLStepFunctionPerPatch
  , pattern MTLStepFunctionPerPatchControlPoint
  , pattern MTLStepFunctionThreadPositionInGridX
  , pattern MTLStepFunctionThreadPositionInGridY
  , pattern MTLStepFunctionThreadPositionInGridXIndexed
  , pattern MTLStepFunctionThreadPositionInGridYIndexed

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
stride :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO CULong
stride mtlBufferLayoutDescriptor =
  sendMessage mtlBufferLayoutDescriptor strideSelector

-- | @- setStride:@
setStride :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> CULong -> IO ()
setStride mtlBufferLayoutDescriptor value =
  sendMessage mtlBufferLayoutDescriptor setStrideSelector value

-- | @- stepFunction@
stepFunction :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO MTLStepFunction
stepFunction mtlBufferLayoutDescriptor =
  sendMessage mtlBufferLayoutDescriptor stepFunctionSelector

-- | @- setStepFunction:@
setStepFunction :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> MTLStepFunction -> IO ()
setStepFunction mtlBufferLayoutDescriptor value =
  sendMessage mtlBufferLayoutDescriptor setStepFunctionSelector value

-- | @- stepRate@
stepRate :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO CULong
stepRate mtlBufferLayoutDescriptor =
  sendMessage mtlBufferLayoutDescriptor stepRateSelector

-- | @- setStepRate:@
setStepRate :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> CULong -> IO ()
setStepRate mtlBufferLayoutDescriptor value =
  sendMessage mtlBufferLayoutDescriptor setStepRateSelector value

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
stepFunctionSelector :: Selector '[] MTLStepFunction
stepFunctionSelector = mkSelector "stepFunction"

-- | @Selector@ for @setStepFunction:@
setStepFunctionSelector :: Selector '[MTLStepFunction] ()
setStepFunctionSelector = mkSelector "setStepFunction:"

-- | @Selector@ for @stepRate@
stepRateSelector :: Selector '[] CULong
stepRateSelector = mkSelector "stepRate"

-- | @Selector@ for @setStepRate:@
setStepRateSelector :: Selector '[CULong] ()
setStepRateSelector = mkSelector "setStepRate:"

