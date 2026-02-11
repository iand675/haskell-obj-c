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
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "layerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The pooling descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCPoolingLayer mlcPoolingLayer => mlcPoolingLayer -> IO (Id MLCPoolingDescriptor)
descriptor mlcPoolingLayer  =
  sendMsg mlcPoolingLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

