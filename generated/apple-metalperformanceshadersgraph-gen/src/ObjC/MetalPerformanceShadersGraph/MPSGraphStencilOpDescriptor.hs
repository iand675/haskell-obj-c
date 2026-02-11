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
  , descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector
  , descriptorWithOffsets_explicitPaddingSelector
  , descriptorWithExplicitPaddingSelector
  , descriptorWithPaddingStyleSelector
  , reductionModeSelector
  , setReductionModeSelector
  , offsetsSelector
  , setOffsetsSelector
  , stridesSelector
  , setStridesSelector
  , dilationRatesSelector
  , setDilationRatesSelector
  , explicitPaddingSelector
  , setExplicitPaddingSelector
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

-- | Creates a stencil operation descriptor with given values.
--
-- - Parameters:   - reductionMode: See @reductionMode@ property.   - offsets: See @offsets@ property.   - strides: See @strides@ property.   - dilationRates: See @dilationRates@ property.   - explicitPadding: See @explicitPadding@ property.   - boundaryMode: See @boundaryMode@ property.   - paddingStyle: See @paddingStyle@ property.   - paddingConstant: See @paddingConstant@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:@
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstant :: MPSGraphReductionMode -> RawId -> RawId -> RawId -> RawId -> MPSGraphPaddingMode -> MPSGraphPaddingStyle -> CFloat -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstant reductionMode offsets strides dilationRates explicitPadding boundaryMode paddingStyle paddingConstant =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:") (retPtr retVoid) [argCULong (coerce reductionMode), argPtr (castPtr (unRawId offsets) :: Ptr ()), argPtr (castPtr (unRawId strides) :: Ptr ()), argPtr (castPtr (unRawId dilationRates) :: Ptr ()), argPtr (castPtr (unRawId explicitPadding) :: Ptr ()), argCLong (coerce boundaryMode), argCULong (coerce paddingStyle), argCFloat paddingConstant] >>= retainedObject . castPtr

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - offsets: See @offsets@ property.   - explicitPadding: See @explicitPadding@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithOffsets:explicitPadding:@
descriptorWithOffsets_explicitPadding :: RawId -> RawId -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithOffsets_explicitPadding offsets explicitPadding =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithOffsets:explicitPadding:") (retPtr retVoid) [argPtr (castPtr (unRawId offsets) :: Ptr ()), argPtr (castPtr (unRawId explicitPadding) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a stencil operation descriptor with default values.
--
-- - Parameters:   - explicitPadding: See @explicitPadding@ property. - Returns: A valid MPSGraphStencilOpDescriptor object
--
-- ObjC selector: @+ descriptorWithExplicitPadding:@
descriptorWithExplicitPadding :: RawId -> IO (Id MPSGraphStencilOpDescriptor)
descriptorWithExplicitPadding explicitPadding =
  do
    cls' <- getRequiredClass "MPSGraphStencilOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithExplicitPadding:") (retPtr retVoid) [argPtr (castPtr (unRawId explicitPadding) :: Ptr ())] >>= retainedObject . castPtr

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

-- | An array of length four that determines from which offset to start reading the input tensor.
--
-- Only used when @paddingStyle@ is @MPSGraphPaddingStyleExplicitOffset@. For example zero offset means that the first stencil window will align its top-left corner (in 4 dimensions) to the top-left corner of the input tensor. Default value: `\@[ \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- offsets@
offsets :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
offsets mpsGraphStencilOpDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "offsets") (retPtr retVoid) []

-- | An array of length four that determines from which offset to start reading the input tensor.
--
-- Only used when @paddingStyle@ is @MPSGraphPaddingStyleExplicitOffset@. For example zero offset means that the first stencil window will align its top-left corner (in 4 dimensions) to the top-left corner of the input tensor. Default value: `\@[ \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setOffsets:@
setOffsets :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setOffsets mpsGraphStencilOpDescriptor  value =
    sendMsg mpsGraphStencilOpDescriptor (mkSelector "setOffsets:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The property that defines strides for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- strides@
strides :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
strides mpsGraphStencilOpDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "strides") (retPtr retVoid) []

-- | The property that defines strides for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setStrides:@
setStrides :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setStrides mpsGraphStencilOpDescriptor  value =
    sendMsg mpsGraphStencilOpDescriptor (mkSelector "setStrides:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The property that defines dilation rates for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- dilationRates@
dilationRates :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
dilationRates mpsGraphStencilOpDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "dilationRates") (retPtr retVoid) []

-- | The property that defines dilation rates for spatial dimensions.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setDilationRates:@
setDilationRates :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setDilationRates mpsGraphStencilOpDescriptor  value =
    sendMsg mpsGraphStencilOpDescriptor (mkSelector "setDilationRates:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The property that defines padding values for spatial dimensions.
--
-- Must be eight numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- explicitPadding@
explicitPadding :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> IO RawId
explicitPadding mpsGraphStencilOpDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphStencilOpDescriptor (mkSelector "explicitPadding") (retPtr retVoid) []

-- | The property that defines padding values for spatial dimensions.
--
-- Must be eight numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setExplicitPadding:@
setExplicitPadding :: IsMPSGraphStencilOpDescriptor mpsGraphStencilOpDescriptor => mpsGraphStencilOpDescriptor -> RawId -> IO ()
setExplicitPadding mpsGraphStencilOpDescriptor  value =
    sendMsg mpsGraphStencilOpDescriptor (mkSelector "setExplicitPadding:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

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
    sendMsg mpsGraphStencilOpDescriptor (mkSelector "setPaddingConstant:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:@
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector :: Selector
descriptorWithReductionMode_offsets_strides_dilationRates_explicitPadding_boundaryMode_paddingStyle_paddingConstantSelector = mkSelector "descriptorWithReductionMode:offsets:strides:dilationRates:explicitPadding:boundaryMode:paddingStyle:paddingConstant:"

-- | @Selector@ for @descriptorWithOffsets:explicitPadding:@
descriptorWithOffsets_explicitPaddingSelector :: Selector
descriptorWithOffsets_explicitPaddingSelector = mkSelector "descriptorWithOffsets:explicitPadding:"

-- | @Selector@ for @descriptorWithExplicitPadding:@
descriptorWithExplicitPaddingSelector :: Selector
descriptorWithExplicitPaddingSelector = mkSelector "descriptorWithExplicitPadding:"

-- | @Selector@ for @descriptorWithPaddingStyle:@
descriptorWithPaddingStyleSelector :: Selector
descriptorWithPaddingStyleSelector = mkSelector "descriptorWithPaddingStyle:"

-- | @Selector@ for @reductionMode@
reductionModeSelector :: Selector
reductionModeSelector = mkSelector "reductionMode"

-- | @Selector@ for @setReductionMode:@
setReductionModeSelector :: Selector
setReductionModeSelector = mkSelector "setReductionMode:"

-- | @Selector@ for @offsets@
offsetsSelector :: Selector
offsetsSelector = mkSelector "offsets"

-- | @Selector@ for @setOffsets:@
setOffsetsSelector :: Selector
setOffsetsSelector = mkSelector "setOffsets:"

-- | @Selector@ for @strides@
stridesSelector :: Selector
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dilationRates@
dilationRatesSelector :: Selector
dilationRatesSelector = mkSelector "dilationRates"

-- | @Selector@ for @setDilationRates:@
setDilationRatesSelector :: Selector
setDilationRatesSelector = mkSelector "setDilationRates:"

-- | @Selector@ for @explicitPadding@
explicitPaddingSelector :: Selector
explicitPaddingSelector = mkSelector "explicitPadding"

-- | @Selector@ for @setExplicitPadding:@
setExplicitPaddingSelector :: Selector
setExplicitPaddingSelector = mkSelector "setExplicitPadding:"

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

