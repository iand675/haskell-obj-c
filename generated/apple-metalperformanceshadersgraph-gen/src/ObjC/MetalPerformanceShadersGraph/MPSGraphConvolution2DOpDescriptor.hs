{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that describes the properties of a 2D-convolution operator.
--
-- Use an instance of this class is to add a 2D-convolution operator with the desired properties to the graph.
--
-- Generated bindings for @MPSGraphConvolution2DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphConvolution2DOpDescriptor
  ( MPSGraphConvolution2DOpDescriptor
  , IsMPSGraphConvolution2DOpDescriptor(..)
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayout
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom
  , strideInX
  , setStrideInX
  , strideInY
  , setStrideInY
  , dilationRateInX
  , setDilationRateInX
  , dilationRateInY
  , setDilationRateInY
  , paddingLeft
  , setPaddingLeft
  , paddingRight
  , setPaddingRight
  , paddingTop
  , setPaddingTop
  , paddingBottom
  , setPaddingBottom
  , paddingStyle
  , setPaddingStyle
  , dataLayout
  , setDataLayout
  , weightsLayout
  , setWeightsLayout
  , groups
  , setGroups
  , dataLayoutSelector
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , groupsSelector
  , paddingBottomSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingStyleSelector
  , paddingTopSelector
  , setDataLayoutSelector
  , setDilationRateInXSelector
  , setDilationRateInYSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , setGroupsSelector
  , setPaddingBottomSelector
  , setPaddingLeftSelector
  , setPaddingRightSelector
  , setPaddingStyleSelector
  , setPaddingTopSelector
  , setStrideInXSelector
  , setStrideInYSelector
  , setWeightsLayoutSelector
  , strideInXSelector
  , strideInYSelector
  , weightsLayoutSelector

  -- * Enum types
  , MPSGraphPaddingStyle(MPSGraphPaddingStyle)
  , pattern MPSGraphPaddingStyleExplicit
  , pattern MPSGraphPaddingStyleTF_VALID
  , pattern MPSGraphPaddingStyleTF_SAME
  , pattern MPSGraphPaddingStyleExplicitOffset
  , pattern MPSGraphPaddingStyleONNX_SAME_LOWER
  , MPSGraphTensorNamedDataLayout(MPSGraphTensorNamedDataLayout)
  , pattern MPSGraphTensorNamedDataLayoutNCHW
  , pattern MPSGraphTensorNamedDataLayoutNHWC
  , pattern MPSGraphTensorNamedDataLayoutOIHW
  , pattern MPSGraphTensorNamedDataLayoutHWIO
  , pattern MPSGraphTensorNamedDataLayoutCHW
  , pattern MPSGraphTensorNamedDataLayoutHWC
  , pattern MPSGraphTensorNamedDataLayoutHW
  , pattern MPSGraphTensorNamedDataLayoutNCDHW
  , pattern MPSGraphTensorNamedDataLayoutNDHWC
  , pattern MPSGraphTensorNamedDataLayoutOIDHW
  , pattern MPSGraphTensorNamedDataLayoutDHWIO

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

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - groups: See ``groups`` property.   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution2DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout strideInX strideInY dilationRateInX dilationRateInY groups paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution2DOpDescriptor"
    sendClassMessage cls' descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector strideInX strideInY dilationRateInX dilationRateInY groups paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout weightsLayout

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - groups: See ``groups`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution2DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayout strideInX strideInY dilationRateInX dilationRateInY groups paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution2DOpDescriptor"
    sendClassMessage cls' descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector strideInX strideInY dilationRateInX dilationRateInY groups paddingStyle dataLayout weightsLayout

-- | Sets the left, right, top, and bottom padding values. - Parameters:   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphConvolution2DOpDescriptor paddingLeft paddingRight paddingTop paddingBottom =
  sendMessage mpsGraphConvolution2DOpDescriptor setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector paddingLeft paddingRight paddingTop paddingBottom

-- | The scale that maps @x@-coordinate of the destination to @x@-coordinate of the source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
strideInX mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor strideInXSelector

-- | The scale that maps @x@-coordinate of the destination to @x@-coordinate of the source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setStrideInXSelector value

-- | The scale that maps @y@-coordinate of the destination to @y@-coordinate of the source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
strideInY mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor strideInYSelector

-- | The scale that maps @y@-coordinate of the destination to @y@-coordinate of the source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setStrideInYSelector value

-- | The amount by which the weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
dilationRateInX mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor dilationRateInXSelector

-- | The amount by which the weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setDilationRateInXSelector value

-- | The amount by which the weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
dilationRateInY mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor dilationRateInYSelector

-- | The amount by which the weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setDilationRateInYSelector value

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingLeft mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor paddingLeftSelector

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setPaddingLeftSelector value

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingRight mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor paddingRightSelector

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setPaddingRightSelector value

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingTop mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor paddingTopSelector

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setPaddingTopSelector value

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingBottom mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor paddingBottomSelector

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setPaddingBottomSelector value

-- | The type of padding applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, and @paddingBottom@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor paddingStyleSelector

-- | The type of padding applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, and @paddingBottom@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setPaddingStyleSelector value

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCHW@, frameork interprets data in source tensor as @batch x channels x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor dataLayoutSelector

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCHW@, frameork interprets data in source tensor as @batch x channels x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setDataLayoutSelector value

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- weightsLayout@
weightsLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
weightsLayout mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor weightsLayoutSelector

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- setWeightsLayout:@
setWeightsLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setWeightsLayout mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setWeightsLayoutSelector value

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelWidth x kernelHeight@
--
-- ObjC selector: @- groups@
groups :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
groups mpsGraphConvolution2DOpDescriptor =
  sendMessage mpsGraphConvolution2DOpDescriptor groupsSelector

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelWidth x kernelHeight@
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setGroups mpsGraphConvolution2DOpDescriptor value =
  sendMessage mpsGraphConvolution2DOpDescriptor setGroupsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout, MPSGraphTensorNamedDataLayout] (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout, MPSGraphTensorNamedDataLayout] (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector '[CULong, CULong, CULong, CULong] ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector '[] CULong
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @setStrideInX:@
setStrideInXSelector :: Selector '[CULong] ()
setStrideInXSelector = mkSelector "setStrideInX:"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector '[] CULong
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @setStrideInY:@
setStrideInYSelector :: Selector '[CULong] ()
setStrideInYSelector = mkSelector "setStrideInY:"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector '[] CULong
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @setDilationRateInX:@
setDilationRateInXSelector :: Selector '[CULong] ()
setDilationRateInXSelector = mkSelector "setDilationRateInX:"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector '[] CULong
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @setDilationRateInY:@
setDilationRateInYSelector :: Selector '[CULong] ()
setDilationRateInYSelector = mkSelector "setDilationRateInY:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector '[] CULong
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector '[CULong] ()
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector '[] CULong
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector '[CULong] ()
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector '[] CULong
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector '[CULong] ()
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector '[] CULong
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector '[CULong] ()
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector '[] MPSGraphPaddingStyle
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] ()
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector '[] MPSGraphTensorNamedDataLayout
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector '[MPSGraphTensorNamedDataLayout] ()
setDataLayoutSelector = mkSelector "setDataLayout:"

-- | @Selector@ for @weightsLayout@
weightsLayoutSelector :: Selector '[] MPSGraphTensorNamedDataLayout
weightsLayoutSelector = mkSelector "weightsLayout"

-- | @Selector@ for @setWeightsLayout:@
setWeightsLayoutSelector :: Selector '[MPSGraphTensorNamedDataLayout] ()
setWeightsLayoutSelector = mkSelector "setWeightsLayout:"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] CULong
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector '[CULong] ()
setGroupsSelector = mkSelector "setGroups:"

