{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCMatMulLayer
--
-- A batched matrix multiplication layer
--
-- Generated bindings for @MLCMatMulLayer@.
module ObjC.MLCompute.MLCMatMulLayer
  ( MLCMatMulLayer
  , IsMLCMatMulLayer(..)
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

-- | Create a matrix multiply layer
--
-- @descriptor@ — A matrix multiply descriptor
--
-- Returns: A new layer for matrix multiplication.
--
-- ObjC selector: @+ layerWithDescriptor:@
layerWithDescriptor :: IsMLCMatMulDescriptor descriptor => descriptor -> IO (Id MLCMatMulLayer)
layerWithDescriptor descriptor =
  do
    cls' <- getRequiredClass "MLCMatMulLayer"
    sendClassMessage cls' layerWithDescriptorSelector (toMLCMatMulDescriptor descriptor)

-- | descriptor
--
-- The matrix multiplication descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCMatMulLayer mlcMatMulLayer => mlcMatMulLayer -> IO (Id MLCMatMulDescriptor)
descriptor mlcMatMulLayer =
  sendMessage mlcMatMulLayer descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector '[Id MLCMatMulDescriptor] (Id MLCMatMulLayer)
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCMatMulDescriptor)
descriptorSelector = mkSelector "descriptor"

