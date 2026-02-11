{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLStencilDescriptor@.
module ObjC.Metal.MTLStencilDescriptor
  ( MTLStencilDescriptor
  , IsMTLStencilDescriptor(..)
  , stencilCompareFunction
  , setStencilCompareFunction
  , stencilFailureOperation
  , setStencilFailureOperation
  , depthFailureOperation
  , setDepthFailureOperation
  , depthStencilPassOperation
  , setDepthStencilPassOperation
  , readMask
  , setReadMask
  , writeMask
  , setWriteMask
  , stencilCompareFunctionSelector
  , setStencilCompareFunctionSelector
  , stencilFailureOperationSelector
  , setStencilFailureOperationSelector
  , depthFailureOperationSelector
  , setDepthFailureOperationSelector
  , depthStencilPassOperationSelector
  , setDepthStencilPassOperationSelector
  , readMaskSelector
  , setReadMaskSelector
  , writeMaskSelector
  , setWriteMaskSelector

  -- * Enum types
  , MTLCompareFunction(MTLCompareFunction)
  , pattern MTLCompareFunctionNever
  , pattern MTLCompareFunctionLess
  , pattern MTLCompareFunctionEqual
  , pattern MTLCompareFunctionLessEqual
  , pattern MTLCompareFunctionGreater
  , pattern MTLCompareFunctionNotEqual
  , pattern MTLCompareFunctionGreaterEqual
  , pattern MTLCompareFunctionAlways
  , MTLStencilOperation(MTLStencilOperation)
  , pattern MTLStencilOperationKeep
  , pattern MTLStencilOperationZero
  , pattern MTLStencilOperationReplace
  , pattern MTLStencilOperationIncrementClamp
  , pattern MTLStencilOperationDecrementClamp
  , pattern MTLStencilOperationInvert
  , pattern MTLStencilOperationIncrementWrap
  , pattern MTLStencilOperationDecrementWrap

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

-- | @- stencilCompareFunction@
stencilCompareFunction :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLCompareFunction
stencilCompareFunction mtlStencilDescriptor  =
  fmap (coerce :: CULong -> MTLCompareFunction) $ sendMsg mtlStencilDescriptor (mkSelector "stencilCompareFunction") retCULong []

-- | @- setStencilCompareFunction:@
setStencilCompareFunction :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLCompareFunction -> IO ()
setStencilCompareFunction mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setStencilCompareFunction:") retVoid [argCULong (coerce value)]

-- | Stencil is tested first.  stencilFailureOperation declares how the stencil buffer is updated when the stencil test fails.
--
-- ObjC selector: @- stencilFailureOperation@
stencilFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
stencilFailureOperation mtlStencilDescriptor  =
  fmap (coerce :: CULong -> MTLStencilOperation) $ sendMsg mtlStencilDescriptor (mkSelector "stencilFailureOperation") retCULong []

-- | Stencil is tested first.  stencilFailureOperation declares how the stencil buffer is updated when the stencil test fails.
--
-- ObjC selector: @- setStencilFailureOperation:@
setStencilFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setStencilFailureOperation mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setStencilFailureOperation:") retVoid [argCULong (coerce value)]

-- | If stencil passes, depth is tested next.  Declare what happens when the depth test fails.
--
-- ObjC selector: @- depthFailureOperation@
depthFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
depthFailureOperation mtlStencilDescriptor  =
  fmap (coerce :: CULong -> MTLStencilOperation) $ sendMsg mtlStencilDescriptor (mkSelector "depthFailureOperation") retCULong []

-- | If stencil passes, depth is tested next.  Declare what happens when the depth test fails.
--
-- ObjC selector: @- setDepthFailureOperation:@
setDepthFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setDepthFailureOperation mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setDepthFailureOperation:") retVoid [argCULong (coerce value)]

-- | If both the stencil and depth tests pass, declare how the stencil buffer is updated.
--
-- ObjC selector: @- depthStencilPassOperation@
depthStencilPassOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
depthStencilPassOperation mtlStencilDescriptor  =
  fmap (coerce :: CULong -> MTLStencilOperation) $ sendMsg mtlStencilDescriptor (mkSelector "depthStencilPassOperation") retCULong []

-- | If both the stencil and depth tests pass, declare how the stencil buffer is updated.
--
-- ObjC selector: @- setDepthStencilPassOperation:@
setDepthStencilPassOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setDepthStencilPassOperation mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setDepthStencilPassOperation:") retVoid [argCULong (coerce value)]

-- | @- readMask@
readMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO CUInt
readMask mtlStencilDescriptor  =
  sendMsg mtlStencilDescriptor (mkSelector "readMask") retCUInt []

-- | @- setReadMask:@
setReadMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> CUInt -> IO ()
setReadMask mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setReadMask:") retVoid [argCUInt (fromIntegral value)]

-- | @- writeMask@
writeMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO CUInt
writeMask mtlStencilDescriptor  =
  sendMsg mtlStencilDescriptor (mkSelector "writeMask") retCUInt []

-- | @- setWriteMask:@
setWriteMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> CUInt -> IO ()
setWriteMask mtlStencilDescriptor  value =
  sendMsg mtlStencilDescriptor (mkSelector "setWriteMask:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stencilCompareFunction@
stencilCompareFunctionSelector :: Selector
stencilCompareFunctionSelector = mkSelector "stencilCompareFunction"

-- | @Selector@ for @setStencilCompareFunction:@
setStencilCompareFunctionSelector :: Selector
setStencilCompareFunctionSelector = mkSelector "setStencilCompareFunction:"

-- | @Selector@ for @stencilFailureOperation@
stencilFailureOperationSelector :: Selector
stencilFailureOperationSelector = mkSelector "stencilFailureOperation"

-- | @Selector@ for @setStencilFailureOperation:@
setStencilFailureOperationSelector :: Selector
setStencilFailureOperationSelector = mkSelector "setStencilFailureOperation:"

-- | @Selector@ for @depthFailureOperation@
depthFailureOperationSelector :: Selector
depthFailureOperationSelector = mkSelector "depthFailureOperation"

-- | @Selector@ for @setDepthFailureOperation:@
setDepthFailureOperationSelector :: Selector
setDepthFailureOperationSelector = mkSelector "setDepthFailureOperation:"

-- | @Selector@ for @depthStencilPassOperation@
depthStencilPassOperationSelector :: Selector
depthStencilPassOperationSelector = mkSelector "depthStencilPassOperation"

-- | @Selector@ for @setDepthStencilPassOperation:@
setDepthStencilPassOperationSelector :: Selector
setDepthStencilPassOperationSelector = mkSelector "setDepthStencilPassOperation:"

-- | @Selector@ for @readMask@
readMaskSelector :: Selector
readMaskSelector = mkSelector "readMask"

-- | @Selector@ for @setReadMask:@
setReadMaskSelector :: Selector
setReadMaskSelector = mkSelector "setReadMask:"

-- | @Selector@ for @writeMask@
writeMaskSelector :: Selector
writeMaskSelector = mkSelector "writeMask"

-- | @Selector@ for @setWriteMask:@
setWriteMaskSelector :: Selector
setWriteMaskSelector = mkSelector "setWriteMask:"

