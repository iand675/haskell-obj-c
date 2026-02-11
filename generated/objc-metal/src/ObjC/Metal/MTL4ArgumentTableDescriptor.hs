{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups parameters for the creation of a Metal argument table.
--
-- Argument tables provide resource bindings to your Metal pipeline states.
--
-- Generated bindings for @MTL4ArgumentTableDescriptor@.
module ObjC.Metal.MTL4ArgumentTableDescriptor
  ( MTL4ArgumentTableDescriptor
  , IsMTL4ArgumentTableDescriptor(..)
  , maxBufferBindCount
  , setMaxBufferBindCount
  , maxTextureBindCount
  , setMaxTextureBindCount
  , maxSamplerStateBindCount
  , setMaxSamplerStateBindCount
  , initializeBindings
  , setInitializeBindings
  , supportAttributeStrides
  , setSupportAttributeStrides
  , label
  , setLabel
  , maxBufferBindCountSelector
  , setMaxBufferBindCountSelector
  , maxTextureBindCountSelector
  , setMaxTextureBindCountSelector
  , maxSamplerStateBindCountSelector
  , setMaxSamplerStateBindCountSelector
  , initializeBindingsSelector
  , setInitializeBindingsSelector
  , supportAttributeStridesSelector
  , setSupportAttributeStridesSelector
  , labelSelector
  , setLabelSelector


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
import ObjC.Foundation.Internal.Classes

-- | Determines the number of buffer-binding slots for the argument table.
--
-- The maximum value of this parameter is 31.
--
-- ObjC selector: @- maxBufferBindCount@
maxBufferBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO CULong
maxBufferBindCount mtL4ArgumentTableDescriptor  =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "maxBufferBindCount") retCULong []

-- | Determines the number of buffer-binding slots for the argument table.
--
-- The maximum value of this parameter is 31.
--
-- ObjC selector: @- setMaxBufferBindCount:@
setMaxBufferBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxBufferBindCount mtL4ArgumentTableDescriptor  value =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "setMaxBufferBindCount:") retVoid [argCULong (fromIntegral value)]

-- | Determines the number of texture-binding slots for the argument table.
--
-- The maximum value of this parameter is 128.
--
-- ObjC selector: @- maxTextureBindCount@
maxTextureBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO CULong
maxTextureBindCount mtL4ArgumentTableDescriptor  =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "maxTextureBindCount") retCULong []

-- | Determines the number of texture-binding slots for the argument table.
--
-- The maximum value of this parameter is 128.
--
-- ObjC selector: @- setMaxTextureBindCount:@
setMaxTextureBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxTextureBindCount mtL4ArgumentTableDescriptor  value =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "setMaxTextureBindCount:") retVoid [argCULong (fromIntegral value)]

-- | Determines the number of sampler state-binding slots for the argument table.
--
-- The maximum value of this parameter is 16.
--
-- ObjC selector: @- maxSamplerStateBindCount@
maxSamplerStateBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO CULong
maxSamplerStateBindCount mtL4ArgumentTableDescriptor  =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "maxSamplerStateBindCount") retCULong []

-- | Determines the number of sampler state-binding slots for the argument table.
--
-- The maximum value of this parameter is 16.
--
-- ObjC selector: @- setMaxSamplerStateBindCount:@
setMaxSamplerStateBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxSamplerStateBindCount mtL4ArgumentTableDescriptor  value =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "setMaxSamplerStateBindCount:") retVoid [argCULong (fromIntegral value)]

-- | Configures whether Metal initializes the bindings to nil values upon creation of argument table.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- initializeBindings@
initializeBindings :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO Bool
initializeBindings mtL4ArgumentTableDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4ArgumentTableDescriptor (mkSelector "initializeBindings") retCULong []

-- | Configures whether Metal initializes the bindings to nil values upon creation of argument table.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setInitializeBindings:@
setInitializeBindings :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> Bool -> IO ()
setInitializeBindings mtL4ArgumentTableDescriptor  value =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "setInitializeBindings:") retVoid [argCULong (if value then 1 else 0)]

-- | Controls whether Metal should reserve memory for attribute strides in the argument table.
--
-- Set this value to true if you intend to provide dynamic attribute strides when binding vertex array buffers to the argument table by calling ``MTL4ArgumentTable/setAddress:attributeStride:atIndex:``
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- supportAttributeStrides@
supportAttributeStrides :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO Bool
supportAttributeStrides mtL4ArgumentTableDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4ArgumentTableDescriptor (mkSelector "supportAttributeStrides") retCULong []

-- | Controls whether Metal should reserve memory for attribute strides in the argument table.
--
-- Set this value to true if you intend to provide dynamic attribute strides when binding vertex array buffers to the argument table by calling ``MTL4ArgumentTable/setAddress:attributeStride:atIndex:``
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setSupportAttributeStrides:@
setSupportAttributeStrides :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> Bool -> IO ()
setSupportAttributeStrides mtL4ArgumentTableDescriptor  value =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "setSupportAttributeStrides:") retVoid [argCULong (if value then 1 else 0)]

-- | Assigns an optional label with the argument table for debug purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO (Id NSString)
label mtL4ArgumentTableDescriptor  =
  sendMsg mtL4ArgumentTableDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional label with the argument table for debug purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor, IsNSString value) => mtL4ArgumentTableDescriptor -> value -> IO ()
setLabel mtL4ArgumentTableDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4ArgumentTableDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxBufferBindCount@
maxBufferBindCountSelector :: Selector
maxBufferBindCountSelector = mkSelector "maxBufferBindCount"

-- | @Selector@ for @setMaxBufferBindCount:@
setMaxBufferBindCountSelector :: Selector
setMaxBufferBindCountSelector = mkSelector "setMaxBufferBindCount:"

-- | @Selector@ for @maxTextureBindCount@
maxTextureBindCountSelector :: Selector
maxTextureBindCountSelector = mkSelector "maxTextureBindCount"

-- | @Selector@ for @setMaxTextureBindCount:@
setMaxTextureBindCountSelector :: Selector
setMaxTextureBindCountSelector = mkSelector "setMaxTextureBindCount:"

-- | @Selector@ for @maxSamplerStateBindCount@
maxSamplerStateBindCountSelector :: Selector
maxSamplerStateBindCountSelector = mkSelector "maxSamplerStateBindCount"

-- | @Selector@ for @setMaxSamplerStateBindCount:@
setMaxSamplerStateBindCountSelector :: Selector
setMaxSamplerStateBindCountSelector = mkSelector "setMaxSamplerStateBindCount:"

-- | @Selector@ for @initializeBindings@
initializeBindingsSelector :: Selector
initializeBindingsSelector = mkSelector "initializeBindings"

-- | @Selector@ for @setInitializeBindings:@
setInitializeBindingsSelector :: Selector
setInitializeBindingsSelector = mkSelector "setInitializeBindings:"

-- | @Selector@ for @supportAttributeStrides@
supportAttributeStridesSelector :: Selector
supportAttributeStridesSelector = mkSelector "supportAttributeStrides"

-- | @Selector@ for @setSupportAttributeStrides:@
setSupportAttributeStridesSelector :: Selector
setSupportAttributeStridesSelector = mkSelector "setSupportAttributeStrides:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

