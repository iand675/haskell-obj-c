{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstant
  , descriptorWithOffsets_explicitPadding
  , descriptorWithExplicitPadding
  , descriptorWithPaddingStyle
  , reductionMode
  , setReductionMode
  , offsets
  , setOffsets
  , strides
  , setStrides
  , dilationRates
  , setDilationRates
  , explicitPadding
  , setExplicitPadding
  , boundaryMode
  , setBoundaryMode
  , paddingStyle
  , setPaddingStyle
  , paddingConstant
  , setPaddingConstant
  , boundaryModeSelector
  , descriptorWithExplicitPaddingSelector
  , descriptorWithOffsets_explicitPaddingSelector
  , descriptorWithPaddingStyleSelector
  , descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector
  , dilationRatesSelector
  , explicitPaddingSelector
  , offsetsSelector
  , paddingConstantSelector
  , paddingStyleSelector
  , reductionModeSelector
  , setBoundaryModeSelector
  , setDilationRatesSelector
  , setExplicitPaddingSelector
  , setOffsetsSelector
  , setPaddingConstantSelector
  , setPaddingStyleSelector
  , setReductionModeSelector
  , setStridesSelector
  , stridesSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a stencil operation descriptor with given values.
--
-- - Parameters:   - reductionMode: See @reductionMode@ property.   - offsets: See @offsets@ property.   - strides: See @strides@ property.   - dilationRates: See @dilationRates@ property.   - explicitPadding: See @explicitPadding@ property.   - boundaryMode: See @boundaryMode@ property.   - paddingStyle: See @paddingStyle@ property.   - paddingConstant: See @paddingConstant@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:@
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstant :: MPSGraphReductionMode -> RawId -> RawId -> RawId -> RawId -> MPSGraphPaddingMode -> MPSGraphPaddingStyle -> CFloat -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstant reductionMode offsets strides dilationRates explicitPadding boundaryMode paddingStyle paddingConstant =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMessage cls' descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector reductionMode offsets strides dilationRates explicitPadding boundaryMode paddingStyle paddingConstant

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - offsets: See @offsets@ property.   - explicitPadding: See @explicitPadding@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithOffsets:explicitPadding:@
descriptorWithOffsets_explicitPadding :: RawId -> RawId -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithOffsets_explicitPadding offsets explicitPadding =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMessage cls' descriptorWithOffsets_explicitPaddingSelector offsets explicitPadding

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - explicitPadding: See @explicitPadding@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithExplicitPadding:@
descriptorWithExplicitPadding :: RawId -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithExplicitPadding explicitPadding =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMessage cls' descriptorWithExplicitPaddingSelector explicitPadding

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - paddingStyle: See @paddingStyle@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithPaddingStyle:@
descriptorWithPaddingStyle :: MPSGraphPaddingStyle -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithPaddingStyle paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMessage cls' descriptorWithPaddingStyleSelector paddingStyle

-- | The reduction mode to use within the stencil window.
--
-- Default value: @MPSGraphReductionModeSum@.
--
-- ObjC selector: @- reductionMode@
reductionMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphReductionMode
reductionMode mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor reductionModeSelector

-- | The reduction mode to use within the stencil window.
--
-- Default value: @MPSGraphReductionModeSum@.
--
-- ObjC selector: @- setReductionMode:@
setReductionMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphReductionMode -> IO ()
setReductionMode mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setReductionModeSelector value

-- | An array of length four that determines from which offset to start reading the input tensor.
--
-- Only used when @paddingStyle@ is @MPSGraphPaddingStyleExplicitOffset@. For example zero offset means that the first stencil window will align its top-left corner (in 4 dimensions) to the top-left corner of the input tensor. Default value: `\@[ \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- offsets@
offsets :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
offsets mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor offsetsSelector

-- | An array of length four that determines from which offset to start reading the input tensor.
--
-- Only used when @paddingStyle@ is @MPSGraphPaddingStyleExplicitOffset@. For example zero offset means that the first stencil window will align its top-left corner (in 4 dimensions) to the top-left corner of the input tensor. Default value: `\@[ \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setOffsets:@
setOffsets :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setOffsets mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setOffsetsSelector value

-- | The property that defines strides for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- strides@
strides :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
strides mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor stridesSelector

-- | The property that defines strides for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setStrides:@
setStrides :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setStrides mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setStridesSelector value

-- | The property that defines dilation rates for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- dilationRates@
dilationRates :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
dilationRates mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor dilationRatesSelector

-- | The property that defines dilation rates for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setDilationRates:@
setDilationRates :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setDilationRates mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setDilationRatesSelector value

-- | The property that defines padding values for spatial dimensions.
--
-- Must be eight numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- explicitPadding@
explicitPadding :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
explicitPadding mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor explicitPaddingSelector

-- | The property that defines padding values for spatial dimensions.
--
-- Must be eight numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setExplicitPadding:@
setExplicitPadding :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setExplicitPadding mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setExplicitPaddingSelector value

-- | The property that determines which values to use for padding the input tensor.
--
-- Default value: @MPSGraphPaddingModeZero@.
--
-- ObjC selector: @- boundaryMode@
boundaryMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphPaddingMode
boundaryMode mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor boundaryModeSelector

-- | The property that determines which values to use for padding the input tensor.
--
-- Default value: @MPSGraphPaddingModeZero@.
--
-- ObjC selector: @- setBoundaryMode:@
setBoundaryMode :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphPaddingMode -> IO ()
setBoundaryMode mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setBoundaryModeSelector value

-- | The property that defines what kind of padding to apply to the stencil operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor paddingStyleSelector

-- | The property that defines what kind of padding to apply to the stencil operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setPaddingStyleSelector value

-- | The padding value for @boundaryMode = MPSGraphPaddingModeConstant@.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingConstant@
paddingConstant :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO CFloat
paddingConstant mpsGraphStencilOpDescriptor =
  sendMessage mpsGraphStencilOpDescriptor paddingConstantSelector

-- | The padding value for @boundaryMode = MPSGraphPaddingModeConstant@.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingConstant:@
setPaddingConstant :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> CFloat -> IO ()
setPaddingConstant mpsGraphStencilOpDescriptor value =
  sendMessage mpsGraphStencilOpDescriptor setPaddingConstantSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:@
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector :: Selector '[MPSGraphReductionMode, RawId, RawId, RawId, RawId, MPSGraphPaddingMode, MPSGraphPaddingStyle, CFloat] (Id MPSGraphStencilOpDescriptor)
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector = mkSelector "descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:"

-- | @Selector@ for @descriptorWithOffsets:explicitPadding:@
descriptorWithOffsets_explicitPaddingSelector :: Selector '[RawId, RawId] (Id MPSGraphStencilOpDescriptor)
descriptorWithOffsets_explicitPaddingSelector = mkSelector "descriptorWithOffsets:explicitPadding:"

-- | @Selector@ for @descriptorWithExplicitPadding:@
descriptorWithExplicitPaddingSelector :: Selector '[RawId] (Id MPSGraphStencilOpDescriptor)
descriptorWithExplicitPaddingSelector = mkSelector "descriptorWithExplicitPadding:"

-- | @Selector@ for @descriptorWithPaddingStyle:@
descriptorWithPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] (Id MPSGraphStencilOpDescriptor)
descriptorWithPaddingStyleSelector = mkSelector "descriptorWithPaddingStyle:"

-- | @Selector@ for @reductionMode@
reductionModeSelector :: Selector '[] MPSGraphReductionMode
reductionModeSelector = mkSelector "reductionMode"

-- | @Selector@ for @setReductionMode:@
setReductionModeSelector :: Selector '[MPSGraphReductionMode] ()
setReductionModeSelector = mkSelector "setReductionMode:"

-- | @Selector@ for @offsets@
offsetsSelector :: Selector '[] RawId
offsetsSelector = mkSelector "offsets"

-- | @Selector@ for @setOffsets:@
setOffsetsSelector :: Selector '[RawId] ()
setOffsetsSelector = mkSelector "setOffsets:"

-- | @Selector@ for @strides@
stridesSelector :: Selector '[] RawId
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector '[RawId] ()
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dilationRates@
dilationRatesSelector :: Selector '[] RawId
dilationRatesSelector = mkSelector "dilationRates"

-- | @Selector@ for @setDilationRates:@
setDilationRatesSelector :: Selector '[RawId] ()
setDilationRatesSelector = mkSelector "setDilationRates:"

-- | @Selector@ for @explicitPadding@
explicitPaddingSelector :: Selector '[] RawId
explicitPaddingSelector = mkSelector "explicitPadding"

-- | @Selector@ for @setExplicitPadding:@
setExplicitPaddingSelector :: Selector '[RawId] ()
setExplicitPaddingSelector = mkSelector "setExplicitPadding:"

-- | @Selector@ for @boundaryMode@
boundaryModeSelector :: Selector '[] MPSGraphPaddingMode
boundaryModeSelector = mkSelector "boundaryMode"

-- | @Selector@ for @setBoundaryMode:@
setBoundaryModeSelector :: Selector '[MPSGraphPaddingMode] ()
setBoundaryModeSelector = mkSelector "setBoundaryMode:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector '[] MPSGraphPaddingStyle
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] ()
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @paddingConstant@
paddingConstantSelector :: Selector '[] CFloat
paddingConstantSelector = mkSelector "paddingConstant"

-- | @Selector@ for @setPaddingConstant:@
setPaddingConstantSelector :: Selector '[CFloat] ()
setPaddingConstantSelector = mkSelector "setPaddingConstant:"

