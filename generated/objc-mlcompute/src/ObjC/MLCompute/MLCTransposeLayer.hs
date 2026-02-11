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
  , layerWithDimensionsSelector


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
    withObjCPtr dimensions $ \raw_dimensions ->
      sendClassMsg cls' (mkSelector "layerWithDimensions:") (retPtr retVoid) [argPtr (castPtr raw_dimensions :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDimensions:@
layerWithDimensionsSelector :: Selector
layerWithDimensionsSelector = mkSelector "layerWithDimensions:"

