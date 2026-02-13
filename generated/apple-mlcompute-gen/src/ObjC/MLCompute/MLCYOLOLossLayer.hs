{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' layerWithDescriptorSelector (toMLCYOLOLossDescriptor lossDescriptor)

-- | yoloLossDescriptor
--
-- The YOLO loss descriptor
--
-- ObjC selector: @- yoloLossDescriptor@
yoloLossDescriptor :: IsMLCYOLOLossLayer mlcyoloLossLayer => mlcyoloLossLayer -> IO (Id MLCYOLOLossDescriptor)
yoloLossDescriptor mlcyoloLossLayer =
  sendMessage mlcyoloLossLayer yoloLossDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector '[Id MLCYOLOLossDescriptor] (Id MLCYOLOLossLayer)
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @yoloLossDescriptor@
yoloLossDescriptorSelector :: Selector '[] (Id MLCYOLOLossDescriptor)
yoloLossDescriptorSelector = mkSelector "yoloLossDescriptor"

