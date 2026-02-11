{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , strideInXSelector
  , setStrideInXSelector
  , strideInYSelector
  , setStrideInYSelector
  , dilationRateInXSelector
  , setDilationRateInXSelector
  , dilationRateInYSelector
  , setDilationRateInYSelector
  , paddingLeftSelector
  , setPaddingLeftSelector
  , paddingRightSelector
  , setPaddingRightSelector
  , paddingTopSelector
  , setPaddingTopSelector
  , paddingBottomSelector
  , setPaddingBottomSelector
  , paddingStyleSelector
  , setPaddingStyleSelector
  , dataLayoutSelector
  , setDataLayoutSelector
  , weightsLayoutSelector
  , setWeightsLayoutSelector
  , groupsSelector
  , setGroupsSelector

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

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - groups: See ``groups`` property.   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution2DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout strideInX strideInY dilationRateInX dilationRateInY groups paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:") (retPtr retVoid) [argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral groups), argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (coerce paddingStyle), argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - groups: See ``groups`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution2DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayout strideInX strideInY dilationRateInX dilationRateInY groups paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:") (retPtr retVoid) [argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral groups), argCULong (coerce paddingStyle), argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Sets the left, right, top, and bottom padding values. - Parameters:   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphConvolution2DOpDescriptor  paddingLeft paddingRight paddingTop paddingBottom =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:") retVoid [argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom)]

-- | The scale that maps @x@-coordinate of the destination to @x@-coordinate of the source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
strideInX mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "strideInX") retCULong []

-- | The scale that maps @x@-coordinate of the destination to @x@-coordinate of the source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | The scale that maps @y@-coordinate of the destination to @y@-coordinate of the source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
strideInY mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "strideInY") retCULong []

-- | The scale that maps @y@-coordinate of the destination to @y@-coordinate of the source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- | The amount by which the weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
dilationRateInX mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "dilationRateInX") retCULong []

-- | The amount by which the weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setDilationRateInX:") retVoid [argCULong (fromIntegral value)]

-- | The amount by which the weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
dilationRateInY mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "dilationRateInY") retCULong []

-- | The amount by which the weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setDilationRateInY:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingLeft mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "paddingLeft") retCULong []

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setPaddingLeft:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingRight mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "paddingRight") retCULong []

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setPaddingRight:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingTop mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "paddingTop") retCULong []

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setPaddingTop:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
paddingBottom mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "paddingBottom") retCULong []

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setPaddingBottom:") retVoid [argCULong (fromIntegral value)]

-- | The type of padding applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, and @paddingBottom@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | The type of padding applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, and @paddingBottom@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCHW@, frameork interprets data in source tensor as @batch x channels x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "dataLayout") retCULong []

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCHW@, frameork interprets data in source tensor as @batch x channels x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setDataLayout:") retVoid [argCULong (coerce value)]

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- weightsLayout@
weightsLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
weightsLayout mpsGraphConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "weightsLayout") retCULong []

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- setWeightsLayout:@
setWeightsLayout :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setWeightsLayout mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setWeightsLayout:") retVoid [argCULong (coerce value)]

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelWidth x kernelHeight@
--
-- ObjC selector: @- groups@
groups :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> IO CULong
groups mpsGraphConvolution2DOpDescriptor  =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "groups") retCULong []

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelWidth x kernelHeight@
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSGraphConvolution2DOpDescriptor mpsGraphConvolution2DOpDescriptor => mpsGraphConvolution2DOpDescriptor -> CULong -> IO ()
setGroups mpsGraphConvolution2DOpDescriptor  value =
  sendMsg mpsGraphConvolution2DOpDescriptor (mkSelector "setGroups:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector :: Selector
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector :: Selector
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_groups_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:groups:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @setStrideInX:@
setStrideInXSelector :: Selector
setStrideInXSelector = mkSelector "setStrideInX:"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @setStrideInY:@
setStrideInYSelector :: Selector
setStrideInYSelector = mkSelector "setStrideInY:"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @setDilationRateInX:@
setDilationRateInXSelector :: Selector
setDilationRateInXSelector = mkSelector "setDilationRateInX:"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @setDilationRateInY:@
setDilationRateInYSelector :: Selector
setDilationRateInYSelector = mkSelector "setDilationRateInY:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector
setDataLayoutSelector = mkSelector "setDataLayout:"

-- | @Selector@ for @weightsLayout@
weightsLayoutSelector :: Selector
weightsLayoutSelector = mkSelector "weightsLayout"

-- | @Selector@ for @setWeightsLayout:@
setWeightsLayoutSelector :: Selector
setWeightsLayoutSelector = mkSelector "setWeightsLayout:"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector
setGroupsSelector = mkSelector "setGroups:"

