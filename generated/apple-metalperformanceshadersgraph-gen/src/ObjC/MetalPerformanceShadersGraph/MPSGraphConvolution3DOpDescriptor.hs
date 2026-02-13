{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that describes the properties of a 3D-convolution operator.
--
-- Use an instance of this class is to add a 3D-convolution operator with desired properties to the graph.
--
-- Generated bindings for @MPSGraphConvolution3DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphConvolution3DOpDescriptor
  ( MPSGraphConvolution3DOpDescriptor
  , IsMPSGraphConvolution3DOpDescriptor(..)
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayout
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayout
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack
  , strideInX
  , setStrideInX
  , strideInY
  , setStrideInY
  , strideInZ
  , setStrideInZ
  , dilationRateInX
  , setDilationRateInX
  , dilationRateInY
  , setDilationRateInY
  , dilationRateInZ
  , setDilationRateInZ
  , paddingLeft
  , setPaddingLeft
  , paddingRight
  , setPaddingRight
  , paddingTop
  , setPaddingTop
  , paddingBottom
  , setPaddingBottom
  , paddingFront
  , setPaddingFront
  , paddingBack
  , setPaddingBack
  , paddingStyle
  , setPaddingStyle
  , dataLayout
  , setDataLayout
  , weightsLayout
  , setWeightsLayout
  , groups
  , setGroups
  , dataLayoutSelector
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , dilationRateInZSelector
  , groupsSelector
  , paddingBackSelector
  , paddingBottomSelector
  , paddingFrontSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingStyleSelector
  , paddingTopSelector
  , setDataLayoutSelector
  , setDilationRateInXSelector
  , setDilationRateInYSelector
  , setDilationRateInZSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector
  , setGroupsSelector
  , setPaddingBackSelector
  , setPaddingBottomSelector
  , setPaddingFrontSelector
  , setPaddingLeftSelector
  , setPaddingRightSelector
  , setPaddingStyleSelector
  , setPaddingTopSelector
  , setStrideInXSelector
  , setStrideInYSelector
  , setStrideInZSelector
  , setWeightsLayoutSelector
  , strideInXSelector
  , strideInYSelector
  , strideInZSelector
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

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - strideInZ: See ``strideInZ`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - dilationRateInZ: See ``dilationRateInZ`` property.   - groups: See ``groups`` property.   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingFront: See ``paddingFront`` property.   - paddingBack: See ``paddingBack`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution3DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayout strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution3DOpDescriptor"
    sendClassMessage cls' descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack paddingStyle dataLayout weightsLayout

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - strideInZ: See ``strideInZ`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - dilationRateInZ: See ``dilationRateInZ`` property.   - groups: See ``groups`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution3DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayout strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution3DOpDescriptor"
    sendClassMessage cls' descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingStyle dataLayout weightsLayout

-- | Sets the left, right, top, bottom, front, and back padding values. - Parameters:   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingFront: See ``paddingFront`` property.   - paddingBottom: See ``paddingBottom`` property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack mpsGraphConvolution3DOpDescriptor paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack =
  sendMessage mpsGraphConvolution3DOpDescriptor setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack

-- | The scale that maps@x@-coordinate of destination to @x@-coordinate of source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInX mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor strideInXSelector

-- | The scale that maps@x@-coordinate of destination to @x@-coordinate of source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setStrideInXSelector value

-- | The scale that maps@y@-coordinate of destination to @y@-coordinate of source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInY mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor strideInYSelector

-- | The scale that maps@y@-coordinate of destination to @y@-coordinate of source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setStrideInYSelector value

-- | The scale that maps@z@-coordinate of destination to @z@-coordinate of source.
--
-- Source @z@-coordinate, @sz@ is computed from destination @z@-coordinate, @dz@ as @sz = strideInZ*dz@. Default value is 1.
--
-- ObjC selector: @- strideInZ@
strideInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInZ mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor strideInZSelector

-- | The scale that maps@z@-coordinate of destination to @z@-coordinate of source.
--
-- Source @z@-coordinate, @sz@ is computed from destination @z@-coordinate, @dz@ as @sz = strideInZ*dz@. Default value is 1.
--
-- ObjC selector: @- setStrideInZ:@
setStrideInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInZ mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setStrideInZSelector value

-- | The amount by which weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInX mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor dilationRateInXSelector

-- | The amount by which weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setDilationRateInXSelector value

-- | The amount by which weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInY mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor dilationRateInYSelector

-- | The amount by which weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setDilationRateInYSelector value

-- | The amount by which weights tensor expands in the @z@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInZ-1@ zeros between consecutive values in @z@-dimension. Dilated weights tensor depth is @(dilationRateInZ-1)*kernelDepth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInZ@
dilationRateInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInZ mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor dilationRateInZSelector

-- | The amount by which weights tensor expands in the @z@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInZ-1@ zeros between consecutive values in @z@-dimension. Dilated weights tensor depth is @(dilationRateInZ-1)*kernelDepth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInZ:@
setDilationRateInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInZ mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setDilationRateInZSelector value

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingLeft mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingLeftSelector

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingLeftSelector value

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingRight mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingRightSelector

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingRightSelector value

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingTop mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingTopSelector

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingTopSelector value

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingBottom mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingBottomSelector

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingBottomSelector value

-- | The number of zeros added at the front of the source tensor.
--
-- ObjC selector: @- paddingFront@
paddingFront :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingFront mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingFrontSelector

-- | The number of zeros added at the front of the source tensor.
--
-- ObjC selector: @- setPaddingFront:@
setPaddingFront :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingFront mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingFrontSelector value

-- | The number of zeros added at the back of the source tensor.
--
-- ObjC selector: @- paddingBack@
paddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingBack mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingBackSelector

-- | The number of zeros added at the back of the source tensor.
--
-- ObjC selector: @- setPaddingBack:@
setPaddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingBack mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingBackSelector value

-- | The type of padding that is applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, @paddingBottom@,   @paddingFront@ and @paddingBack@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor paddingStyleSelector

-- | The type of padding that is applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, @paddingBottom@,   @paddingFront@ and @paddingBack@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setPaddingStyleSelector value

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Depth, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCDHW@, frameork interprets data in source tensor as @batch x channels x depth x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor dataLayoutSelector

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Depth, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCDHW@, frameork interprets data in source tensor as @batch x channels x depth x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setDataLayoutSelector value

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel depth, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIDHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelDepth x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- weightsLayout@
weightsLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
weightsLayout mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor weightsLayoutSelector

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel depth, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIDHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelDepth x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- setWeightsLayout:@
setWeightsLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setWeightsLayout mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setWeightsLayoutSelector value

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelDepth x kernelWidth x kernelHeight@
--
-- ObjC selector: @- groups@
groups :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
groups mpsGraphConvolution3DOpDescriptor =
  sendMessage mpsGraphConvolution3DOpDescriptor groupsSelector

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelDepth x kernelWidth x kernelHeight@
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setGroups mpsGraphConvolution3DOpDescriptor value =
  sendMessage mpsGraphConvolution3DOpDescriptor setGroupsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout, MPSGraphTensorNamedDataLayout] (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout, MPSGraphTensorNamedDataLayout] (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong] ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:"

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

-- | @Selector@ for @strideInZ@
strideInZSelector :: Selector '[] CULong
strideInZSelector = mkSelector "strideInZ"

-- | @Selector@ for @setStrideInZ:@
setStrideInZSelector :: Selector '[CULong] ()
setStrideInZSelector = mkSelector "setStrideInZ:"

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

-- | @Selector@ for @dilationRateInZ@
dilationRateInZSelector :: Selector '[] CULong
dilationRateInZSelector = mkSelector "dilationRateInZ"

-- | @Selector@ for @setDilationRateInZ:@
setDilationRateInZSelector :: Selector '[CULong] ()
setDilationRateInZSelector = mkSelector "setDilationRateInZ:"

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

-- | @Selector@ for @paddingFront@
paddingFrontSelector :: Selector '[] CULong
paddingFrontSelector = mkSelector "paddingFront"

-- | @Selector@ for @setPaddingFront:@
setPaddingFrontSelector :: Selector '[CULong] ()
setPaddingFrontSelector = mkSelector "setPaddingFront:"

-- | @Selector@ for @paddingBack@
paddingBackSelector :: Selector '[] CULong
paddingBackSelector = mkSelector "paddingBack"

-- | @Selector@ for @setPaddingBack:@
setPaddingBackSelector :: Selector '[CULong] ()
setPaddingBackSelector = mkSelector "setPaddingBack:"

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

