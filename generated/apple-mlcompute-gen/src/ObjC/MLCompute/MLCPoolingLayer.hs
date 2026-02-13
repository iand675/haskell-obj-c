{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCPoolingLayer
--
-- A pooling layer
--
-- Generated bindings for @MLCPoolingLayer@.
module ObjC.MLCompute.MLCPoolingLayer
  ( MLCPoolingLayer
  , IsMLCPoolingLayer(..)
  , layerWithDescriptor
  , descriptor
  , descriptorSelector
  , layerWithDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a pooling layer
--
-- @descriptor@ — The pooling descriptor
--
-- Returns: A new pooling layer
--
-- ObjC selector: @+ layerWithDescriptor:@
layerWithDescriptor :: IsMLCPoolingDescriptor descriptor => descriptor -> IO (Id MLCPoolingLayer)
layerWithDescriptor descriptor =
  do
    cls' <- getRequiredClass "MLCPoolingLayer"
    sendClassMessage cls' layerWithDescriptorSelector (toMLCPoolingDescriptor descriptor)

-- | descriptor
--
-- The pooling descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCPoolingLayer mlcPoolingLayer => mlcPoolingLayer -> IO (Id MLCPoolingDescriptor)
descriptor mlcPoolingLayer =
  sendMessage mlcPoolingLayer descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector '[Id MLCPoolingDescriptor] (Id MLCPoolingLayer)
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCPoolingDescriptor)
descriptorSelector = mkSelector "descriptor"

