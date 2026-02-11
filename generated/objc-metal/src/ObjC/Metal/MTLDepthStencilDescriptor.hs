{-# LANGUAGE PatternSynonyms #-}
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
  , depthCompareFunctionSelector
  , setDepthCompareFunctionSelector
  , depthWriteEnabledSelector
  , setDepthWriteEnabledSelector
  , frontFaceStencilSelector
  , setFrontFaceStencilSelector
  , backFaceStencilSelector
  , setBackFaceStencilSelector
  , labelSelector
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

-- | @- depthCompareFunction@
depthCompareFunction :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO MTLCompareFunction
depthCompareFunction mtlDepthStencilDescriptor  =
  fmap (coerce :: CULong -> MTLCompareFunction) $ sendMsg mtlDepthStencilDescriptor (mkSelector "depthCompareFunction") retCULong []

-- | @- setDepthCompareFunction:@
setDepthCompareFunction :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> MTLCompareFunction -> IO ()
setDepthCompareFunction mtlDepthStencilDescriptor  value =
  sendMsg mtlDepthStencilDescriptor (mkSelector "setDepthCompareFunction:") retVoid [argCULong (coerce value)]

-- | @- depthWriteEnabled@
depthWriteEnabled :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO Bool
depthWriteEnabled mtlDepthStencilDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlDepthStencilDescriptor (mkSelector "depthWriteEnabled") retCULong []

-- | @- setDepthWriteEnabled:@
setDepthWriteEnabled :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> Bool -> IO ()
setDepthWriteEnabled mtlDepthStencilDescriptor  value =
  sendMsg mtlDepthStencilDescriptor (mkSelector "setDepthWriteEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- frontFaceStencil@
frontFaceStencil :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id MTLStencilDescriptor)
frontFaceStencil mtlDepthStencilDescriptor  =
  sendMsg mtlDepthStencilDescriptor (mkSelector "frontFaceStencil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrontFaceStencil:@
setFrontFaceStencil :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsMTLStencilDescriptor value) => mtlDepthStencilDescriptor -> value -> IO ()
setFrontFaceStencil mtlDepthStencilDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlDepthStencilDescriptor (mkSelector "setFrontFaceStencil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backFaceStencil@
backFaceStencil :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id MTLStencilDescriptor)
backFaceStencil mtlDepthStencilDescriptor  =
  sendMsg mtlDepthStencilDescriptor (mkSelector "backFaceStencil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackFaceStencil:@
setBackFaceStencil :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsMTLStencilDescriptor value) => mtlDepthStencilDescriptor -> value -> IO ()
setBackFaceStencil mtlDepthStencilDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlDepthStencilDescriptor (mkSelector "setBackFaceStencil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- label@
label :: IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor => mtlDepthStencilDescriptor -> IO (Id NSString)
label mtlDepthStencilDescriptor  =
  sendMsg mtlDepthStencilDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLDepthStencilDescriptor mtlDepthStencilDescriptor, IsNSString value) => mtlDepthStencilDescriptor -> value -> IO ()
setLabel mtlDepthStencilDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlDepthStencilDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @depthCompareFunction@
depthCompareFunctionSelector :: Selector
depthCompareFunctionSelector = mkSelector "depthCompareFunction"

-- | @Selector@ for @setDepthCompareFunction:@
setDepthCompareFunctionSelector :: Selector
setDepthCompareFunctionSelector = mkSelector "setDepthCompareFunction:"

-- | @Selector@ for @depthWriteEnabled@
depthWriteEnabledSelector :: Selector
depthWriteEnabledSelector = mkSelector "depthWriteEnabled"

-- | @Selector@ for @setDepthWriteEnabled:@
setDepthWriteEnabledSelector :: Selector
setDepthWriteEnabledSelector = mkSelector "setDepthWriteEnabled:"

-- | @Selector@ for @frontFaceStencil@
frontFaceStencilSelector :: Selector
frontFaceStencilSelector = mkSelector "frontFaceStencil"

-- | @Selector@ for @setFrontFaceStencil:@
setFrontFaceStencilSelector :: Selector
setFrontFaceStencilSelector = mkSelector "setFrontFaceStencil:"

-- | @Selector@ for @backFaceStencil@
backFaceStencilSelector :: Selector
backFaceStencilSelector = mkSelector "backFaceStencil"

-- | @Selector@ for @setBackFaceStencil:@
setBackFaceStencilSelector :: Selector
setBackFaceStencilSelector = mkSelector "setBackFaceStencil:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

