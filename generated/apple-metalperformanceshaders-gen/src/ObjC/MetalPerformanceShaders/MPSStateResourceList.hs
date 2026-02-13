{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSStateResourceList@.
module ObjC.MetalPerformanceShaders.MPSStateResourceList
  ( MPSStateResourceList
  , IsMPSStateResourceList(..)
  , resourceList
  , resourceListWithTextureDescriptors
  , resourceListWithBufferSizes
  , init_
  , appendTexture
  , appendBuffer
  , appendBufferSelector
  , appendTextureSelector
  , initSelector
  , resourceListSelector
  , resourceListWithBufferSizesSelector
  , resourceListWithTextureDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- | Init an empty autoreleased resource list
--
-- ObjC selector: @+ resourceList@
resourceList :: IO (Id MPSStateResourceList)
resourceList  =
  do
    cls' <- getRequiredClass "MPSStateResourceList"
    sendClassMessage cls' resourceListSelector

-- | Init a resource list with a nil terminated list of MTLTextureDescriptors
--
-- ObjC selector: @+ resourceListWithTextureDescriptors:@
resourceListWithTextureDescriptors :: IsMTLTextureDescriptor d => d -> IO (Id MPSStateResourceList)
resourceListWithTextureDescriptors d =
  do
    cls' <- getRequiredClass "MPSStateResourceList"
    sendClassMessage cls' resourceListWithTextureDescriptorsSelector (toMTLTextureDescriptor d)

-- | Init a resource list with a 0 terminated list of Buffer Sizes
--
-- ObjC selector: @+ resourceListWithBufferSizes:@
resourceListWithBufferSizes :: CULong -> IO (Id MPSStateResourceList)
resourceListWithBufferSizes firstSize =
  do
    cls' <- getRequiredClass "MPSStateResourceList"
    sendClassMessage cls' resourceListWithBufferSizesSelector firstSize

-- | Init an empty list
--
-- ObjC selector: @- init@
init_ :: IsMPSStateResourceList mpsStateResourceList => mpsStateResourceList -> IO (Id MPSStateResourceList)
init_ mpsStateResourceList =
  sendOwnedMessage mpsStateResourceList initSelector

-- | append a texture to the resource list
--
-- ObjC selector: @- appendTexture:@
appendTexture :: (IsMPSStateResourceList mpsStateResourceList, IsMTLTextureDescriptor descriptor) => mpsStateResourceList -> descriptor -> IO ()
appendTexture mpsStateResourceList descriptor =
  sendMessage mpsStateResourceList appendTextureSelector (toMTLTextureDescriptor descriptor)

-- | append a buffer to the resource list
--
-- ObjC selector: @- appendBuffer:@
appendBuffer :: IsMPSStateResourceList mpsStateResourceList => mpsStateResourceList -> CULong -> IO ()
appendBuffer mpsStateResourceList size =
  sendMessage mpsStateResourceList appendBufferSelector size

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceList@
resourceListSelector :: Selector '[] (Id MPSStateResourceList)
resourceListSelector = mkSelector "resourceList"

-- | @Selector@ for @resourceListWithTextureDescriptors:@
resourceListWithTextureDescriptorsSelector :: Selector '[Id MTLTextureDescriptor] (Id MPSStateResourceList)
resourceListWithTextureDescriptorsSelector = mkSelector "resourceListWithTextureDescriptors:"

-- | @Selector@ for @resourceListWithBufferSizes:@
resourceListWithBufferSizesSelector :: Selector '[CULong] (Id MPSStateResourceList)
resourceListWithBufferSizesSelector = mkSelector "resourceListWithBufferSizes:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSStateResourceList)
initSelector = mkSelector "init"

-- | @Selector@ for @appendTexture:@
appendTextureSelector :: Selector '[Id MTLTextureDescriptor] ()
appendTextureSelector = mkSelector "appendTexture:"

-- | @Selector@ for @appendBuffer:@
appendBufferSelector :: Selector '[CULong] ()
appendBufferSelector = mkSelector "appendBuffer:"

