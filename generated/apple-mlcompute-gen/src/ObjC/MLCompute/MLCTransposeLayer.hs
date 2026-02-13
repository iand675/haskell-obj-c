{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTransposeLayer
--
-- A transpose layer
--
-- Generated bindings for @MLCTransposeLayer@.
module ObjC.MLCompute.MLCTransposeLayer
  ( MLCTransposeLayer
  , IsMLCTransposeLayer(..)
  , layerWithDimensions
  , dimensions
  , dimensionsSelector
  , layerWithDimensionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a transpose layer
--
-- @dimensions@ — NSArray<NSNumber *> representing the desired ordering of dimensions                The dimensions array specifies the input axis source for each output axis, such that the                K'th element in the dimensions array specifies the input axis source for the K'th axis in the                output.  The batch dimension which is typically axis 0 cannot be transposed.
--
-- Returns: A new transpose layer.
--
-- ObjC selector: @+ layerWithDimensions:@
layerWithDimensions :: IsNSArray dimensions => dimensions -> IO (Id MLCTransposeLayer)
layerWithDimensions dimensions =
  do
    cls' <- getRequiredClass "MLCTransposeLayer"
    sendClassMessage cls' layerWithDimensionsSelector (toNSArray dimensions)

-- | dimensions
--
-- Permutes the dimensions according to 'dimensions'.
--
-- The returned tensor's dimension i will correspond to dimensions[i].
--
-- ObjC selector: @- dimensions@
dimensions :: IsMLCTransposeLayer mlcTransposeLayer => mlcTransposeLayer -> IO (Id NSArray)
dimensions mlcTransposeLayer =
  sendMessage mlcTransposeLayer dimensionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDimensions:@
layerWithDimensionsSelector :: Selector '[Id NSArray] (Id MLCTransposeLayer)
layerWithDimensionsSelector = mkSelector "layerWithDimensions:"

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector '[] (Id NSArray)
dimensionsSelector = mkSelector "dimensions"

