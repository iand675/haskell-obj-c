{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCUpsampleLayer
--
-- An upsample layer
--
-- Generated bindings for @MLCUpsampleLayer@.
module ObjC.MLCompute.MLCUpsampleLayer
  ( MLCUpsampleLayer
  , IsMLCUpsampleLayer(..)
  , layerWithShape
  , layerWithShape_sampleMode_alignsCorners
  , shape
  , sampleMode
  , alignsCorners
  , alignsCornersSelector
  , layerWithShapeSelector
  , layerWithShape_sampleMode_alignsCornersSelector
  , sampleModeSelector
  , shapeSelector

  -- * Enum types
  , MLCSampleMode(MLCSampleMode)
  , pattern MLCSampleModeNearest
  , pattern MLCSampleModeLinear

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create an upsample layer
--
-- @shape@ — A NSArray<NSNumber *> representing the dimensions of the result tensor
--
-- Returns: A new upsample layer.
--
-- ObjC selector: @+ layerWithShape:@
layerWithShape :: IsNSArray shape => shape -> IO (Id MLCUpsampleLayer)
layerWithShape shape =
  do
    cls' <- getRequiredClass "MLCUpsampleLayer"
    sendClassMessage cls' layerWithShapeSelector (toNSArray shape)

-- | Create an upsample layer
--
-- @shape@ — A NSArray<NSNumber *> representing the dimensions of the result tensor
--
-- @sampleMode@ — The upsampling algorithm to use.  Default is nearest.
--
-- @alignsCorners@ — Whether the corner pixels of the input and output tensors are aligned or not.
--
-- Returns: A new upsample layer.
--
-- ObjC selector: @+ layerWithShape:sampleMode:alignsCorners:@
layerWithShape_sampleMode_alignsCorners :: IsNSArray shape => shape -> MLCSampleMode -> Bool -> IO (Id MLCUpsampleLayer)
layerWithShape_sampleMode_alignsCorners shape sampleMode alignsCorners =
  do
    cls' <- getRequiredClass "MLCUpsampleLayer"
    sendClassMessage cls' layerWithShape_sampleMode_alignsCornersSelector (toNSArray shape) sampleMode alignsCorners

-- | shape
--
-- A NSArray<NSNumber *> representing just the width if number of entries in shape array is 1 or                the height followed by width of result tensor if the number of entries in shape array is 2.
--
-- ObjC selector: @- shape@
shape :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO (Id NSArray)
shape mlcUpsampleLayer =
  sendMessage mlcUpsampleLayer shapeSelector

-- | sampleMode
--
-- The sampling mode to use when performing the upsample.
--
-- ObjC selector: @- sampleMode@
sampleMode :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO MLCSampleMode
sampleMode mlcUpsampleLayer =
  sendMessage mlcUpsampleLayer sampleModeSelector

-- | alignsCorners
--
-- A boolean that specifies whether the corner pixels of the source and result tensors are aligned.
--
-- If True, the corner pixels of the source and result tensors are aligned, and thus preserving the values at those pixels.                This only has effect when mode is 'bilinear'. Default is NO.
--
-- ObjC selector: @- alignsCorners@
alignsCorners :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO Bool
alignsCorners mlcUpsampleLayer =
  sendMessage mlcUpsampleLayer alignsCornersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithShape:@
layerWithShapeSelector :: Selector '[Id NSArray] (Id MLCUpsampleLayer)
layerWithShapeSelector = mkSelector "layerWithShape:"

-- | @Selector@ for @layerWithShape:sampleMode:alignsCorners:@
layerWithShape_sampleMode_alignsCornersSelector :: Selector '[Id NSArray, MLCSampleMode, Bool] (Id MLCUpsampleLayer)
layerWithShape_sampleMode_alignsCornersSelector = mkSelector "layerWithShape:sampleMode:alignsCorners:"

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] (Id NSArray)
shapeSelector = mkSelector "shape"

-- | @Selector@ for @sampleMode@
sampleModeSelector :: Selector '[] MLCSampleMode
sampleModeSelector = mkSelector "sampleMode"

-- | @Selector@ for @alignsCorners@
alignsCornersSelector :: Selector '[] Bool
alignsCornersSelector = mkSelector "alignsCorners"

