{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCGatherLayer
--
-- A gather layer
--
-- Generated bindings for @MLCGatherLayer@.
module ObjC.MLCompute.MLCGatherLayer
  ( MLCGatherLayer
  , IsMLCGatherLayer(..)
  , layerWithDimension
  , dimension
  , layerWithDimensionSelector
  , dimensionSelector


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

-- | Create a gather layer
--
-- @dimension@ — The dimension along which to index
--
-- Returns: A new gather layer
--
-- ObjC selector: @+ layerWithDimension:@
layerWithDimension :: CULong -> IO (Id MLCGatherLayer)
layerWithDimension dimension =
  do
    cls' <- getRequiredClass "MLCGatherLayer"
    sendClassMsg cls' (mkSelector "layerWithDimension:") (retPtr retVoid) [argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

-- | dimension
--
-- The dimension along which to index
--
-- ObjC selector: @- dimension@
dimension :: IsMLCGatherLayer mlcGatherLayer => mlcGatherLayer -> IO CULong
dimension mlcGatherLayer  =
  sendMsg mlcGatherLayer (mkSelector "dimension") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDimension:@
layerWithDimensionSelector :: Selector
layerWithDimensionSelector = mkSelector "layerWithDimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

