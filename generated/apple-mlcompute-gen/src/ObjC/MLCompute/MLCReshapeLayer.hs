{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCReshapeLayer
--
-- A reshape layer.
--
-- Generated bindings for @MLCReshapeLayer@.
module ObjC.MLCompute.MLCReshapeLayer
  ( MLCReshapeLayer
  , IsMLCReshapeLayer(..)
  , layerWithShape
  , shape
  , layerWithShapeSelector
  , shapeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a reshape layer with the shape you specify.
--
-- @shape@ — An array that contains the sizes of each dimension.
--
-- Returns: A new reshape layer.
--
-- ObjC selector: @+ layerWithShape:@
layerWithShape :: IsNSArray shape => shape -> IO (Id MLCReshapeLayer)
layerWithShape shape =
  do
    cls' <- getRequiredClass "MLCReshapeLayer"
    sendClassMessage cls' layerWithShapeSelector (toNSArray shape)

-- | shape
--
-- The target shape.
--
-- ObjC selector: @- shape@
shape :: IsMLCReshapeLayer mlcReshapeLayer => mlcReshapeLayer -> IO (Id NSArray)
shape mlcReshapeLayer =
  sendMessage mlcReshapeLayer shapeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithShape:@
layerWithShapeSelector :: Selector '[Id NSArray] (Id MLCReshapeLayer)
layerWithShapeSelector = mkSelector "layerWithShape:"

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] (Id NSArray)
shapeSelector = mkSelector "shape"

