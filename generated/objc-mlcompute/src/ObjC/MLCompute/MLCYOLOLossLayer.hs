{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCYOLOLossLayer
--
-- A YOLO loss layer
--
-- Generated bindings for @MLCYOLOLossLayer@.
module ObjC.MLCompute.MLCYOLOLossLayer
  ( MLCYOLOLossLayer
  , IsMLCYOLOLossLayer(..)
  , layerWithDescriptor
  , yoloLossDescriptor
  , layerWithDescriptorSelector
  , yoloLossDescriptorSelector


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

-- | Create a YOLO loss layer
--
-- @lossDescriptor@ — The loss descriptor
--
-- Returns: A new YOLO loss layer.
--
-- ObjC selector: @+ layerWithDescriptor:@
layerWithDescriptor :: IsMLCYOLOLossDescriptor lossDescriptor => lossDescriptor -> IO (Id MLCYOLOLossLayer)
layerWithDescriptor lossDescriptor =
  do
    cls' <- getRequiredClass "MLCYOLOLossLayer"
    withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
      sendClassMsg cls' (mkSelector "layerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | yoloLossDescriptor
--
-- The YOLO loss descriptor
--
-- ObjC selector: @- yoloLossDescriptor@
yoloLossDescriptor :: IsMLCYOLOLossLayer mlcyoloLossLayer => mlcyoloLossLayer -> IO (Id MLCYOLOLossDescriptor)
yoloLossDescriptor mlcyoloLossLayer  =
  sendMsg mlcyoloLossLayer (mkSelector "yoloLossDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @yoloLossDescriptor@
yoloLossDescriptorSelector :: Selector
yoloLossDescriptorSelector = mkSelector "yoloLossDescriptor"

