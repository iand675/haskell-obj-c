{-# LANGUAGE PatternSynonyms #-}
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
  , strideSelector
  , setStrideSelector
  , stepFunctionSelector
  , setStepFunctionSelector
  , stepRateSelector
  , setStepRateSelector

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
stride :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO CULong
stride mtlBufferLayoutDescriptor  =
  sendMsg mtlBufferLayoutDescriptor (mkSelector "stride") retCULong []

-- | @- setStride:@
setStride :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> CULong -> IO ()
setStride mtlBufferLayoutDescriptor  value =
  sendMsg mtlBufferLayoutDescriptor (mkSelector "setStride:") retVoid [argCULong (fromIntegral value)]

-- | @- stepFunction@
stepFunction :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO MTLStepFunction
stepFunction mtlBufferLayoutDescriptor  =
  fmap (coerce :: CULong -> MTLStepFunction) $ sendMsg mtlBufferLayoutDescriptor (mkSelector "stepFunction") retCULong []

-- | @- setStepFunction:@
setStepFunction :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> MTLStepFunction -> IO ()
setStepFunction mtlBufferLayoutDescriptor  value =
  sendMsg mtlBufferLayoutDescriptor (mkSelector "setStepFunction:") retVoid [argCULong (coerce value)]

-- | @- stepRate@
stepRate :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> IO CULong
stepRate mtlBufferLayoutDescriptor  =
  sendMsg mtlBufferLayoutDescriptor (mkSelector "stepRate") retCULong []

-- | @- setStepRate:@
setStepRate :: IsMTLBufferLayoutDescriptor mtlBufferLayoutDescriptor => mtlBufferLayoutDescriptor -> CULong -> IO ()
setStepRate mtlBufferLayoutDescriptor  value =
  sendMsg mtlBufferLayoutDescriptor (mkSelector "setStepRate:") retVoid [argCULong (fromIntegral value)]

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

