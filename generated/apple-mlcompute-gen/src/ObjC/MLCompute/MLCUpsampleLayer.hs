{-# LANGUAGE PatternSynonyms #-}
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
  , layerWithShapeSelector
  , layerWithShape_sampleMode_alignsCornersSelector
  , shapeSelector
  , sampleModeSelector
  , alignsCornersSelector

  -- * Enum types
  , MLCSampleMode(MLCSampleMode)
  , pattern MLCSampleModeNearest
  , pattern MLCSampleModeLinear

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
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "layerWithShape:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "layerWithShape:sampleMode:alignsCorners:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCInt (coerce sampleMode), argCULong (if alignsCorners then 1 else 0)] >>= retainedObject . castPtr

-- | shape
--
-- A NSArray<NSNumber *> representing just the width if number of entries in shape array is 1 or                the height followed by width of result tensor if the number of entries in shape array is 2.
--
-- ObjC selector: @- shape@
shape :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO (Id NSArray)
shape mlcUpsampleLayer  =
    sendMsg mlcUpsampleLayer (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleMode
--
-- The sampling mode to use when performing the upsample.
--
-- ObjC selector: @- sampleMode@
sampleMode :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO MLCSampleMode
sampleMode mlcUpsampleLayer  =
    fmap (coerce :: CInt -> MLCSampleMode) $ sendMsg mlcUpsampleLayer (mkSelector "sampleMode") retCInt []

-- | alignsCorners
--
-- A boolean that specifies whether the corner pixels of the source and result tensors are aligned.
--
-- If True, the corner pixels of the source and result tensors are aligned, and thus preserving the values at those pixels.                This only has effect when mode is 'bilinear'. Default is NO.
--
-- ObjC selector: @- alignsCorners@
alignsCorners :: IsMLCUpsampleLayer mlcUpsampleLayer => mlcUpsampleLayer -> IO Bool
alignsCorners mlcUpsampleLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcUpsampleLayer (mkSelector "alignsCorners") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithShape:@
layerWithShapeSelector :: Selector
layerWithShapeSelector = mkSelector "layerWithShape:"

-- | @Selector@ for @layerWithShape:sampleMode:alignsCorners:@
layerWithShape_sampleMode_alignsCornersSelector :: Selector
layerWithShape_sampleMode_alignsCornersSelector = mkSelector "layerWithShape:sampleMode:alignsCorners:"

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

-- | @Selector@ for @sampleMode@
sampleModeSelector :: Selector
sampleModeSelector = mkSelector "sampleMode"

-- | @Selector@ for @alignsCorners@
alignsCornersSelector :: Selector
alignsCornersSelector = mkSelector "alignsCorners"

