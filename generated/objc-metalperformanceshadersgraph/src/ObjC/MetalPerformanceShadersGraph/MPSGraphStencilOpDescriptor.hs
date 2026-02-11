{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a stencil operation.
--
-- Use this descriptor with the following ``MPSGraph`` method: - ``MPSGraph/stencilWithSourceTensor:weightsTensor:descriptor:name:``
--
-- Generated bindings for @MPSGraphStencilOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphStencilOpDescriptor
  ( MPSGraphStencilOpDescriptor
  , IsMPSGraphStencilOpDescriptor(..)
  , descriptorWithPaddingStyle
  , reductionMode
  , setReductionMode
  , boundaryMode
  , setBoundaryMode
  , paddingStyle
  , setPaddingStyle
  , paddingConstant
  , setPaddingConstant
  , descriptorWithPaddingStyleSelector
  , reductionModeSelector
  , setReductionModeSelector
  , boundaryModeSelector
  , setBoundaryModeSelector
  , paddingStyleSelector
  , setPaddingStyleSelector
  , paddingConstantSelector
  , setPaddingConstantSelector

  -- * Enum types
  , MPSGraphPaddingMode(MPSGraphPaddingMode)
  , pattern MPSGraphPaddingModeConstant
  , pattern MPSGraphPaddingModeReflect
  , pattern MPSGraphPaddingModeSymmetric
  , pattern MPSGraphPaddingModeClampToEdge
  , pattern MPSGraphPaddingModeZero
  , pattern MPSGraphPaddingModePeriodic
  , pattern MPSGraphPaddingModeAntiPeriodic
  , MPSGraphPaddingStyle(MPSGraphPaddingStyle)
  , pattern MPSGraphPaddingStyleExplicit
  , pattern MPSGraphPaddingStyleTF_VALID
  , pattern MPSGraphPaddingStyleTF_SAME
  , pattern MPSGraphPaddingStyleExplicitOffset
  , pattern MPSGraphPaddingStyleONNX_SAME_LOWER
  , MPSGraphReductionMode(MPSGraphReductionMode)
  , pattern MPSGraphReductionModeMin
  , pattern MPSGraphReductionModeMax
  , pattern MPSGraphReductionModeSum
  , pattern MPSGraphReductionModeProduct
  , pattern MPSGraphReductionModeArgumentMin
  , pattern MPSGraphReductionModeArgumentMax

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - paddingStyle: See @paddingStyle@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithPaddingStyle:@
descriptorWithPaddingStyle :: MPSGraphPaddingStyle -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithPaddingStyle paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithPaddingStyle:") (retPtr retVoid) [argCULong (coerce paddingStyle)] >>= retainedObject . castPtr

-- | The reduction mode to use within the stencil window.
--
-- Default value: @MPSGraphReductionModeSum@.
--
-- ObjC selector: @- reductionMode@
reductionMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphReductionMode
reductionMode mpsGraphStencilOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphReductionMode) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "reductionMode") retCULong []

-- | The reduction mode to use within the stencil window.
--
-- Default value: @MPSGraphReductionModeSum@.
--
-- ObjC selector: @- setReductionMode:@
setReductionMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphReductionMode -> IO ()
setReductionMode mpsGraphStencilOpDescriptor  value =
  sendMsg mpsGraphStencilOpDescriptor (mkSelector "setReductionMode:") retVoid [argCULong (coerce value)]

-- | The property that determines which values to use for padding the input tensor.
--
-- Default value: @MPSGraphPaddingModeZero@.
--
-- ObjC selector: @- boundaryMode@
boundaryMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphPaddingMode
boundaryMode mpsGraphStencilOpDescriptor  =
  fmap (coerce :: CLong -> MPSGraphPaddingMode) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "boundaryMode") retCLong []

-- | The property that determines which values to use for padding the input tensor.
--
-- Default value: @MPSGraphPaddingModeZero@.
--
-- ObjC selector: @- setBoundaryMode:@
setBoundaryMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphPaddingMode -> IO ()
setBoundaryMode mpsGraphStencilOpDescriptor  value =
  sendMsg mpsGraphStencilOpDescriptor (mkSelector "setBoundaryMode:") retVoid [argCLong (coerce value)]

-- | The property that defines what kind of padding to apply to the stencil operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphStencilOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | The property that defines what kind of padding to apply to the stencil operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphStencilOpDescriptor  value =
  sendMsg mpsGraphStencilOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | The padding value for @boundaryMode = MPSGraphPaddingModeConstant@.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingConstant@
paddingConstant :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO CFloat
paddingConstant mpsGraphStencilOpDescriptor  =
  sendMsg mpsGraphStencilOpDescriptor (mkSelector "paddingConstant") retCFloat []

-- | The padding value for @boundaryMode = MPSGraphPaddingModeConstant@.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingConstant:@
setPaddingConstant :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> CFloat -> IO ()
setPaddingConstant mpsGraphStencilOpDescriptor  value =
  sendMsg mpsGraphStencilOpDescriptor (mkSelector "setPaddingConstant:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithPaddingStyle:@
descriptorWithPaddingStyleSelector :: Selector
descriptorWithPaddingStyleSelector = mkSelector "descriptorWithPaddingStyle:"

-- | @Selector@ for @reductionMode@
reductionModeSelector :: Selector
reductionModeSelector = mkSelector "reductionMode"

-- | @Selector@ for @setReductionMode:@
setReductionModeSelector :: Selector
setReductionModeSelector = mkSelector "setReductionMode:"

-- | @Selector@ for @boundaryMode@
boundaryModeSelector :: Selector
boundaryModeSelector = mkSelector "boundaryMode"

-- | @Selector@ for @setBoundaryMode:@
setBoundaryModeSelector :: Selector
setBoundaryModeSelector = mkSelector "setBoundaryMode:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @paddingConstant@
paddingConstantSelector :: Selector
paddingConstantSelector = mkSelector "paddingConstant"

-- | @Selector@ for @setPaddingConstant:@
setPaddingConstantSelector :: Selector
setPaddingConstantSelector = mkSelector "setPaddingConstant:"

