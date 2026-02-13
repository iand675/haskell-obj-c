{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLDepthStencilDescriptor@.
module ObjC.Metal.MTLDepthStencilDescriptor
  ( MTLDepthStencilDescriptor
  , IsMTLDepthStencilDescriptor(..)
  , depthCompareFunction
  , setDepthCompareFunction
  , depthWriteEnabled
  , setDepthWriteEnabled
  , frontFaceStencil
  , setFrontFaceStencil
  , backFaceStencil
  , setBackFaceStencil
  , label
  , setLabel
  , backFaceStencilSelector
  , depthCompareFunctionSelector
  , depthWriteEnabledSelector
  , frontFaceStencilSelector
  , labelSelector
  , setBackFaceStencilSelector
  , setDepthCompareFunctionSelector
  , setDepthWriteEnabledSelector
  , setFrontFaceStencilSelector
  , setLabelSelector

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

-- | @- depthCompareFunction@
depthCompareFunction :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO MTLCompareFunction
depthCompareFunction mtlDepthStencilDescriptor =
  sendMessage mtlDepthStencilDescriptor depthCompareFunctionSelector

-- | @- setDepthCompareFunction:@
setDepthCompareFunction :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> MTLCompareFunction -> IO ()
setDepthCompareFunction mtlDepthStencilDescriptor value =
  sendMessage mtlDepthStencilDescriptor setDepthCompareFunctionSelector value

-- | @- depthWriteEnabled@
depthWriteEnabled :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO Bool
depthWriteEnabled mtlDepthStencilDescriptor =
  sendMessage mtlDepthStencilDescriptor depthWriteEnabledSelector

-- | @- setDepthWriteEnabled:@
setDepthWriteEnabled :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> Bool -> IO ()
setDepthWriteEnabled mtlDepthStencilDescriptor value =
  sendMessage mtlDepthStencilDescriptor setDepthWriteEnabledSelector value

-- | @- frontFaceStencil@
frontFaceStencil :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id MTLStencilDescriptor)
frontFaceStencil mtlDepthStencilDescriptor =
  sendMessage mtlDepthStencilDescriptor frontFaceStencilSelector

-- | @- setFrontFaceStencil:@
setFrontFaceStencil :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsMTLStencilDescriptor value) => mtlDepthStencilDescriptor -> value -> IO ()
setFrontFaceStencil mtlDepthStencilDescriptor value =
  sendMessage mtlDepthStencilDescriptor setFrontFaceStencilSelector (toMTLStencilDescriptor value)

-- | @- backFaceStencil@
backFaceStencil :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id MTLStencilDescriptor)
backFaceStencil mtlDepthStencilDescriptor =
  sendMessage mtlDepthStencilDescriptor backFaceStencilSelector

-- | @- setBackFaceStencil:@
setBackFaceStencil :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsMTLStencilDescriptor value) => mtlDepthStencilDescriptor -> value -> IO ()
setBackFaceStencil mtlDepthStencilDescriptor value =
  sendMessage mtlDepthStencilDescriptor setBackFaceStencilSelector (toMTLStencilDescriptor value)

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- label@
label :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id NSString)
label mtlDepthStencilDescriptor =
  sendMessage mtlDepthStencilDescriptor labelSelector

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsNSString value) => mtlDepthStencilDescriptor -> value -> IO ()
setLabel mtlDepthStencilDescriptor value =
  sendMessage mtlDepthStencilDescriptor setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @depthCompareFunction@
depthCompareFunctionSelector :: Selector '[] MTLCompareFunction
depthCompareFunctionSelector = mkSelector "depthCompareFunction"

-- | @Selector@ for @setDepthCompareFunction:@
setDepthCompareFunctionSelector :: Selector '[MTLCompareFunction] ()
setDepthCompareFunctionSelector = mkSelector "setDepthCompareFunction:"

-- | @Selector@ for @depthWriteEnabled@
depthWriteEnabledSelector :: Selector '[] Bool
depthWriteEnabledSelector = mkSelector "depthWriteEnabled"

-- | @Selector@ for @setDepthWriteEnabled:@
setDepthWriteEnabledSelector :: Selector '[Bool] ()
setDepthWriteEnabledSelector = mkSelector "setDepthWriteEnabled:"

-- | @Selector@ for @frontFaceStencil@
frontFaceStencilSelector :: Selector '[] (Id MTLStencilDescriptor)
frontFaceStencilSelector = mkSelector "frontFaceStencil"

-- | @Selector@ for @setFrontFaceStencil:@
setFrontFaceStencilSelector :: Selector '[Id MTLStencilDescriptor] ()
setFrontFaceStencilSelector = mkSelector "setFrontFaceStencil:"

-- | @Selector@ for @backFaceStencil@
backFaceStencilSelector :: Selector '[] (Id MTLStencilDescriptor)
backFaceStencilSelector = mkSelector "backFaceStencil"

-- | @Selector@ for @setBackFaceStencil:@
setBackFaceStencilSelector :: Selector '[Id MTLStencilDescriptor] ()
setBackFaceStencilSelector = mkSelector "setBackFaceStencil:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

