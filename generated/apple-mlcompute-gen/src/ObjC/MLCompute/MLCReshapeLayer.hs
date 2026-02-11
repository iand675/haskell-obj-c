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
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "layerWithShape:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ())] >>= retainedObject . castPtr

-- | shape
--
-- The target shape.
--
-- ObjC selector: @- shape@
shape :: IsMLCReshapeLayer mlcReshapeLayer => mlcReshapeLayer -> IO (Id NSArray)
shape mlcReshapeLayer  =
    sendMsg mlcReshapeLayer (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithShape:@
layerWithShapeSelector :: Selector
layerWithShapeSelector = mkSelector "layerWithShape:"

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

