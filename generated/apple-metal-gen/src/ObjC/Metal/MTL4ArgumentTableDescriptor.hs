{-# LANGUAGE DataKinds #-}
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
  , initializeBindingsSelector
  , labelSelector
  , maxBufferBindCountSelector
  , maxSamplerStateBindCountSelector
  , maxTextureBindCountSelector
  , setInitializeBindingsSelector
  , setLabelSelector
  , setMaxBufferBindCountSelector
  , setMaxSamplerStateBindCountSelector
  , setMaxTextureBindCountSelector
  , setSupportAttributeStridesSelector
  , supportAttributeStridesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
maxBufferBindCount mtL4ArgumentTableDescriptor =
  sendMessage mtL4ArgumentTableDescriptor maxBufferBindCountSelector

-- | Determines the number of buffer-binding slots for the argument table.
--
-- The maximum value of this parameter is 31.
--
-- ObjC selector: @- setMaxBufferBindCount:@
setMaxBufferBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxBufferBindCount mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setMaxBufferBindCountSelector value

-- | Determines the number of texture-binding slots for the argument table.
--
-- The maximum value of this parameter is 128.
--
-- ObjC selector: @- maxTextureBindCount@
maxTextureBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO CULong
maxTextureBindCount mtL4ArgumentTableDescriptor =
  sendMessage mtL4ArgumentTableDescriptor maxTextureBindCountSelector

-- | Determines the number of texture-binding slots for the argument table.
--
-- The maximum value of this parameter is 128.
--
-- ObjC selector: @- setMaxTextureBindCount:@
setMaxTextureBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxTextureBindCount mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setMaxTextureBindCountSelector value

-- | Determines the number of sampler state-binding slots for the argument table.
--
-- The maximum value of this parameter is 16.
--
-- ObjC selector: @- maxSamplerStateBindCount@
maxSamplerStateBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO CULong
maxSamplerStateBindCount mtL4ArgumentTableDescriptor =
  sendMessage mtL4ArgumentTableDescriptor maxSamplerStateBindCountSelector

-- | Determines the number of sampler state-binding slots for the argument table.
--
-- The maximum value of this parameter is 16.
--
-- ObjC selector: @- setMaxSamplerStateBindCount:@
setMaxSamplerStateBindCount :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> CULong -> IO ()
setMaxSamplerStateBindCount mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setMaxSamplerStateBindCountSelector value

-- | Configures whether Metal initializes the bindings to nil values upon creation of argument table.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- initializeBindings@
initializeBindings :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO Bool
initializeBindings mtL4ArgumentTableDescriptor =
  sendOwnedMessage mtL4ArgumentTableDescriptor initializeBindingsSelector

-- | Configures whether Metal initializes the bindings to nil values upon creation of argument table.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setInitializeBindings:@
setInitializeBindings :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> Bool -> IO ()
setInitializeBindings mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setInitializeBindingsSelector value

-- | Controls whether Metal should reserve memory for attribute strides in the argument table.
--
-- Set this value to true if you intend to provide dynamic attribute strides when binding vertex array buffers to the argument table by calling ``MTL4ArgumentTable/setAddress:attributeStride:atIndex:``
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- supportAttributeStrides@
supportAttributeStrides :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO Bool
supportAttributeStrides mtL4ArgumentTableDescriptor =
  sendMessage mtL4ArgumentTableDescriptor supportAttributeStridesSelector

-- | Controls whether Metal should reserve memory for attribute strides in the argument table.
--
-- Set this value to true if you intend to provide dynamic attribute strides when binding vertex array buffers to the argument table by calling ``MTL4ArgumentTable/setAddress:attributeStride:atIndex:``
--
-- The default value of this property is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setSupportAttributeStrides:@
setSupportAttributeStrides :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> Bool -> IO ()
setSupportAttributeStrides mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setSupportAttributeStridesSelector value

-- | Assigns an optional label with the argument table for debug purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor => mtL4ArgumentTableDescriptor -> IO (Id NSString)
label mtL4ArgumentTableDescriptor =
  sendMessage mtL4ArgumentTableDescriptor labelSelector

-- | Assigns an optional label with the argument table for debug purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4ArgumentTableDescriptor mtL4ArgumentTableDescriptor, IsNSString value) => mtL4ArgumentTableDescriptor -> value -> IO ()
setLabel mtL4ArgumentTableDescriptor value =
  sendMessage mtL4ArgumentTableDescriptor setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxBufferBindCount@
maxBufferBindCountSelector :: Selector '[] CULong
maxBufferBindCountSelector = mkSelector "maxBufferBindCount"

-- | @Selector@ for @setMaxBufferBindCount:@
setMaxBufferBindCountSelector :: Selector '[CULong] ()
setMaxBufferBindCountSelector = mkSelector "setMaxBufferBindCount:"

-- | @Selector@ for @maxTextureBindCount@
maxTextureBindCountSelector :: Selector '[] CULong
maxTextureBindCountSelector = mkSelector "maxTextureBindCount"

-- | @Selector@ for @setMaxTextureBindCount:@
setMaxTextureBindCountSelector :: Selector '[CULong] ()
setMaxTextureBindCountSelector = mkSelector "setMaxTextureBindCount:"

-- | @Selector@ for @maxSamplerStateBindCount@
maxSamplerStateBindCountSelector :: Selector '[] CULong
maxSamplerStateBindCountSelector = mkSelector "maxSamplerStateBindCount"

-- | @Selector@ for @setMaxSamplerStateBindCount:@
setMaxSamplerStateBindCountSelector :: Selector '[CULong] ()
setMaxSamplerStateBindCountSelector = mkSelector "setMaxSamplerStateBindCount:"

-- | @Selector@ for @initializeBindings@
initializeBindingsSelector :: Selector '[] Bool
initializeBindingsSelector = mkSelector "initializeBindings"

-- | @Selector@ for @setInitializeBindings:@
setInitializeBindingsSelector :: Selector '[Bool] ()
setInitializeBindingsSelector = mkSelector "setInitializeBindings:"

-- | @Selector@ for @supportAttributeStrides@
supportAttributeStridesSelector :: Selector '[] Bool
supportAttributeStridesSelector = mkSelector "supportAttributeStrides"

-- | @Selector@ for @setSupportAttributeStrides:@
setSupportAttributeStridesSelector :: Selector '[Bool] ()
setSupportAttributeStridesSelector = mkSelector "setSupportAttributeStrides:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

