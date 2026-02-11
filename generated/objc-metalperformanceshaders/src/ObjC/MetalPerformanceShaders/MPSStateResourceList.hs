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
  , resourceListSelector
  , resourceListWithTextureDescriptorsSelector
  , resourceListWithBufferSizesSelector
  , initSelector
  , appendTextureSelector
  , appendBufferSelector


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
    sendClassMsg cls' (mkSelector "resourceList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Init a resource list with a nil terminated list of MTLTextureDescriptors
--
-- ObjC selector: @+ resourceListWithTextureDescriptors:@
resourceListWithTextureDescriptors :: IsMTLTextureDescriptor d => d -> IO (Id MPSStateResourceList)
resourceListWithTextureDescriptors d =
  do
    cls' <- getRequiredClass "MPSStateResourceList"
    withObjCPtr d $ \raw_d ->
      sendClassMsg cls' (mkSelector "resourceListWithTextureDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_d :: Ptr ())] >>= retainedObject . castPtr

-- | Init a resource list with a 0 terminated list of Buffer Sizes
--
-- ObjC selector: @+ resourceListWithBufferSizes:@
resourceListWithBufferSizes :: CULong -> IO (Id MPSStateResourceList)
resourceListWithBufferSizes firstSize =
  do
    cls' <- getRequiredClass "MPSStateResourceList"
    sendClassMsg cls' (mkSelector "resourceListWithBufferSizes:") (retPtr retVoid) [argCULong (fromIntegral firstSize)] >>= retainedObject . castPtr

-- | Init an empty list
--
-- ObjC selector: @- init@
init_ :: IsMPSStateResourceList mpsStateResourceList => mpsStateResourceList -> IO (Id MPSStateResourceList)
init_ mpsStateResourceList  =
  sendMsg mpsStateResourceList (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | append a texture to the resource list
--
-- ObjC selector: @- appendTexture:@
appendTexture :: (IsMPSStateResourceList mpsStateResourceList, IsMTLTextureDescriptor descriptor) => mpsStateResourceList -> descriptor -> IO ()
appendTexture mpsStateResourceList  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsStateResourceList (mkSelector "appendTexture:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ())]

-- | append a buffer to the resource list
--
-- ObjC selector: @- appendBuffer:@
appendBuffer :: IsMPSStateResourceList mpsStateResourceList => mpsStateResourceList -> CULong -> IO ()
appendBuffer mpsStateResourceList  size =
  sendMsg mpsStateResourceList (mkSelector "appendBuffer:") retVoid [argCULong (fromIntegral size)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceList@
resourceListSelector :: Selector
resourceListSelector = mkSelector "resourceList"

-- | @Selector@ for @resourceListWithTextureDescriptors:@
resourceListWithTextureDescriptorsSelector :: Selector
resourceListWithTextureDescriptorsSelector = mkSelector "resourceListWithTextureDescriptors:"

-- | @Selector@ for @resourceListWithBufferSizes:@
resourceListWithBufferSizesSelector :: Selector
resourceListWithBufferSizesSelector = mkSelector "resourceListWithBufferSizes:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @appendTexture:@
appendTextureSelector :: Selector
appendTextureSelector = mkSelector "appendTexture:"

-- | @Selector@ for @appendBuffer:@
appendBufferSelector :: Selector
appendBufferSelector = mkSelector "appendBuffer:"

