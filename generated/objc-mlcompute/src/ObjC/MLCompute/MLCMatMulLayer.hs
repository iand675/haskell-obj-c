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
  , layerWithDescriptorSelector
  , descriptorSelector


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
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "layerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The matrix multiplication descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCMatMulLayer mlcMatMulLayer => mlcMatMulLayer -> IO (Id MLCMatMulDescriptor)
descriptor mlcMatMulLayer  =
  sendMsg mlcMatMulLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

