{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , depthFailureOperationSelector
  , depthStencilPassOperationSelector
  , readMaskSelector
  , setDepthFailureOperationSelector
  , setDepthStencilPassOperationSelector
  , setReadMaskSelector
  , setStencilCompareFunctionSelector
  , setStencilFailureOperationSelector
  , setWriteMaskSelector
  , stencilCompareFunctionSelector
  , stencilFailureOperationSelector
  , writeMaskSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- stencilCompareFunction@
stencilCompareFunction :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLCompareFunction
stencilCompareFunction mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor stencilCompareFunctionSelector

-- | @- setStencilCompareFunction:@
setStencilCompareFunction :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLCompareFunction -> IO ()
setStencilCompareFunction mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setStencilCompareFunctionSelector value

-- | Stencil is tested first.  stencilFailureOperation declares how the stencil buffer is updated when the stencil test fails.
--
-- ObjC selector: @- stencilFailureOperation@
stencilFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
stencilFailureOperation mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor stencilFailureOperationSelector

-- | Stencil is tested first.  stencilFailureOperation declares how the stencil buffer is updated when the stencil test fails.
--
-- ObjC selector: @- setStencilFailureOperation:@
setStencilFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setStencilFailureOperation mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setStencilFailureOperationSelector value

-- | If stencil passes, depth is tested next.  Declare what happens when the depth test fails.
--
-- ObjC selector: @- depthFailureOperation@
depthFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
depthFailureOperation mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor depthFailureOperationSelector

-- | If stencil passes, depth is tested next.  Declare what happens when the depth test fails.
--
-- ObjC selector: @- setDepthFailureOperation:@
setDepthFailureOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setDepthFailureOperation mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setDepthFailureOperationSelector value

-- | If both the stencil and depth tests pass, declare how the stencil buffer is updated.
--
-- ObjC selector: @- depthStencilPassOperation@
depthStencilPassOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO MTLStencilOperation
depthStencilPassOperation mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor depthStencilPassOperationSelector

-- | If both the stencil and depth tests pass, declare how the stencil buffer is updated.
--
-- ObjC selector: @- setDepthStencilPassOperation:@
setDepthStencilPassOperation :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> MTLStencilOperation -> IO ()
setDepthStencilPassOperation mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setDepthStencilPassOperationSelector value

-- | @- readMask@
readMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO CUInt
readMask mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor readMaskSelector

-- | @- setReadMask:@
setReadMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> CUInt -> IO ()
setReadMask mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setReadMaskSelector value

-- | @- writeMask@
writeMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> IO CUInt
writeMask mtlStencilDescriptor =
  sendMessage mtlStencilDescriptor writeMaskSelector

-- | @- setWriteMask:@
setWriteMask :: IsMTLStencilDescriptor mtlStencilDescriptor => mtlStencilDescriptor -> CUInt -> IO ()
setWriteMask mtlStencilDescriptor value =
  sendMessage mtlStencilDescriptor setWriteMaskSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stencilCompareFunction@
stencilCompareFunctionSelector :: Selector '[] MTLCompareFunction
stencilCompareFunctionSelector = mkSelector "stencilCompareFunction"

-- | @Selector@ for @setStencilCompareFunction:@
setStencilCompareFunctionSelector :: Selector '[MTLCompareFunction] ()
setStencilCompareFunctionSelector = mkSelector "setStencilCompareFunction:"

-- | @Selector@ for @stencilFailureOperation@
stencilFailureOperationSelector :: Selector '[] MTLStencilOperation
stencilFailureOperationSelector = mkSelector "stencilFailureOperation"

-- | @Selector@ for @setStencilFailureOperation:@
setStencilFailureOperationSelector :: Selector '[MTLStencilOperation] ()
setStencilFailureOperationSelector = mkSelector "setStencilFailureOperation:"

-- | @Selector@ for @depthFailureOperation@
depthFailureOperationSelector :: Selector '[] MTLStencilOperation
depthFailureOperationSelector = mkSelector "depthFailureOperation"

-- | @Selector@ for @setDepthFailureOperation:@
setDepthFailureOperationSelector :: Selector '[MTLStencilOperation] ()
setDepthFailureOperationSelector = mkSelector "setDepthFailureOperation:"

-- | @Selector@ for @depthStencilPassOperation@
depthStencilPassOperationSelector :: Selector '[] MTLStencilOperation
depthStencilPassOperationSelector = mkSelector "depthStencilPassOperation"

-- | @Selector@ for @setDepthStencilPassOperation:@
setDepthStencilPassOperationSelector :: Selector '[MTLStencilOperation] ()
setDepthStencilPassOperationSelector = mkSelector "setDepthStencilPassOperation:"

-- | @Selector@ for @readMask@
readMaskSelector :: Selector '[] CUInt
readMaskSelector = mkSelector "readMask"

-- | @Selector@ for @setReadMask:@
setReadMaskSelector :: Selector '[CUInt] ()
setReadMaskSelector = mkSelector "setReadMask:"

-- | @Selector@ for @writeMask@
writeMaskSelector :: Selector '[] CUInt
writeMaskSelector = mkSelector "writeMask"

-- | @Selector@ for @setWriteMask:@
setWriteMaskSelector :: Selector '[CUInt] ()
setWriteMaskSelector = mkSelector "setWriteMask:"

