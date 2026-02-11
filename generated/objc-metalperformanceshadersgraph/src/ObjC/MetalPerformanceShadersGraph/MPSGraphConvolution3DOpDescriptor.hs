{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector
  , descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector
  , strideInXSelector
  , setStrideInXSelector
  , strideInYSelector
  , setStrideInYSelector
  , strideInZSelector
  , setStrideInZSelector
  , dilationRateInXSelector
  , setDilationRateInXSelector
  , dilationRateInYSelector
  , setDilationRateInYSelector
  , dilationRateInZSelector
  , setDilationRateInZSelector
  , paddingLeftSelector
  , setPaddingLeftSelector
  , paddingRightSelector
  , setPaddingRightSelector
  , paddingTopSelector
  , setPaddingTopSelector
  , paddingBottomSelector
  , setPaddingBottomSelector
  , paddingFrontSelector
  , setPaddingFrontSelector
  , paddingBackSelector
  , setPaddingBackSelector
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

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - strideInZ: See ``strideInZ`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - dilationRateInZ: See ``dilationRateInZ`` property.   - groups: See ``groups`` property.   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingFront: See ``paddingFront`` property.   - paddingBack: See ``paddingBack`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution3DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayout strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution3DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:") (retPtr retVoid) [argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral strideInZ), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral dilationRateInZ), argCULong (fromIntegral groups), argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (fromIntegral paddingFront), argCULong (fromIntegral paddingBack), argCULong (coerce paddingStyle), argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Creates a convolution descriptor with given values for parameters. - Parameters:   - strideInX: See ``strideInX`` property.   - strideInY: See ``strideInY`` property.   - strideInZ: See ``strideInZ`` property.   - dilationRateInX: See ``dilationRateInX`` property.   - dilationRateInY: See ``dilationRateInY`` property.   - dilationRateInZ: See ``dilationRateInZ`` property.   - groups: See ``groups`` property.   - paddingStyle: See ``paddingStyle`` property.   - dataLayout: See ``dataLayout`` property.   - weightsLayout: See ``weightsLayout`` property. - Returns: The @MPSGraphConvolution3DOpDescriptor@ on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphConvolution3DOpDescriptor)
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayout strideInX strideInY strideInZ dilationRateInX dilationRateInY dilationRateInZ groups paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphConvolution3DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:") (retPtr retVoid) [argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral strideInZ), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral dilationRateInZ), argCULong (fromIntegral groups), argCULong (coerce paddingStyle), argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Sets the left, right, top, bottom, front, and back padding values. - Parameters:   - paddingLeft: See ``paddingLeft`` property.   - paddingRight: See ``paddingRight`` property.   - paddingTop: See ``paddingTop`` property.   - paddingBottom: See ``paddingBottom`` property.   - paddingFront: See ``paddingFront`` property.   - paddingBottom: See ``paddingBottom`` property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack mpsGraphConvolution3DOpDescriptor  paddingLeft paddingRight paddingTop paddingBottom paddingFront paddingBack =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:") retVoid [argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (fromIntegral paddingFront), argCULong (fromIntegral paddingBack)]

-- | The scale that maps@x@-coordinate of destination to @x@-coordinate of source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInX mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "strideInX") retCULong []

-- | The scale that maps@x@-coordinate of destination to @x@-coordinate of source.
--
-- Source @x@-coordinate, @sx@ is computed from destination @x@-coordinate, @dx@ as @sx = strideInX*dx@. Default value is 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | The scale that maps@y@-coordinate of destination to @y@-coordinate of source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInY mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "strideInY") retCULong []

-- | The scale that maps@y@-coordinate of destination to @y@-coordinate of source.
--
-- Source @y@-coordinate, @sy@ is computed from destination @y@-coordinate, @dy@ as @sy = strideInY*dy@. Default value is 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- | The scale that maps@z@-coordinate of destination to @z@-coordinate of source.
--
-- Source @z@-coordinate, @sz@ is computed from destination @z@-coordinate, @dz@ as @sz = strideInZ*dz@. Default value is 1.
--
-- ObjC selector: @- strideInZ@
strideInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
strideInZ mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "strideInZ") retCULong []

-- | The scale that maps@z@-coordinate of destination to @z@-coordinate of source.
--
-- Source @z@-coordinate, @sz@ is computed from destination @z@-coordinate, @dz@ as @sz = strideInZ*dz@. Default value is 1.
--
-- ObjC selector: @- setStrideInZ:@
setStrideInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setStrideInZ mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setStrideInZ:") retVoid [argCULong (fromIntegral value)]

-- | The amount by which weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInX mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "dilationRateInX") retCULong []

-- | The amount by which weights tensor expands in the @x@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInX-1@ zeros between consecutive values in @x@-dimension. Dilated weights tensor width is @(dilationRateInX-1)*kernelWidth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setDilationRateInX:") retVoid [argCULong (fromIntegral value)]

-- | The amount by which weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInY mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "dilationRateInY") retCULong []

-- | The amount by which weights tensor expands in the @y@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInY-1@ zeros between consecutive values in @y@-dimension. Dilated weights tensor width is @(dilationRateInY-1)*kernelHeight+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setDilationRateInY:") retVoid [argCULong (fromIntegral value)]

-- | The amount by which weights tensor expands in the @z@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInZ-1@ zeros between consecutive values in @z@-dimension. Dilated weights tensor depth is @(dilationRateInZ-1)*kernelDepth+1@. Default value is 1.
--
-- ObjC selector: @- dilationRateInZ@
dilationRateInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
dilationRateInZ mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "dilationRateInZ") retCULong []

-- | The amount by which weights tensor expands in the @z@-direction.
--
-- The weights tensor is dilated by inserting @dilationRateInZ-1@ zeros between consecutive values in @z@-dimension. Dilated weights tensor depth is @(dilationRateInZ-1)*kernelDepth+1@. Default value is 1.
--
-- ObjC selector: @- setDilationRateInZ:@
setDilationRateInZ :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setDilationRateInZ mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setDilationRateInZ:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingLeft mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingLeft") retCULong []

-- | The number of zeros added on the left side of the source tensor.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingLeft:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingRight mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingRight") retCULong []

-- | The number of zeros added on the right side of the source tensor.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingRight:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingTop mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingTop") retCULong []

-- | The number of zeros added at the top of the source tensor.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingTop:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingBottom mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingBottom") retCULong []

-- | The number of zeros added at the bottom of the source tensor.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingBottom:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the front of the source tensor.
--
-- ObjC selector: @- paddingFront@
paddingFront :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingFront mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingFront") retCULong []

-- | The number of zeros added at the front of the source tensor.
--
-- ObjC selector: @- setPaddingFront:@
setPaddingFront :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingFront mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingFront:") retVoid [argCULong (fromIntegral value)]

-- | The number of zeros added at the back of the source tensor.
--
-- ObjC selector: @- paddingBack@
paddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
paddingBack mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingBack") retCULong []

-- | The number of zeros added at the back of the source tensor.
--
-- ObjC selector: @- setPaddingBack:@
setPaddingBack :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setPaddingBack mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingBack:") retVoid [argCULong (fromIntegral value)]

-- | The type of padding that is applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, @paddingBottom@,   @paddingFront@ and @paddingBack@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphConvolution3DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | The type of padding that is applied to the source tensor.
--
-- If paddingStyle is @MPSGraphPaddingStyleExplicit@, @paddingLeft@, @laddingRight@, @paddingTop@, @paddingBottom@,   @paddingFront@ and @paddingBack@ must to be specified. For all other padding styles, framework compute these values so you dont need to provide these values.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Depth, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCDHW@, frameork interprets data in source tensor as @batch x channels x depth x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphConvolution3DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "dataLayout") retCULong []

-- | The named layout of data in the source tensor.
--
-- It defines the order of named dimensions (Batch, Channel, Depth, Height, Width). The convolution operation uses this to interpret data in the source tensor. For example, if @dataLayout@ is @MPSGraphTensorNamedDataLayoutNCDHW@, frameork interprets data in source tensor as @batch x channels x depth x height x width@ with @width@ as fastest moving dimension.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setDataLayout:") retVoid [argCULong (coerce value)]

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel depth, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIDHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelDepth x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- weightsLayout@
weightsLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
weightsLayout mpsGraphConvolution3DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "weightsLayout") retCULong []

-- | The named layout of data in the weights tensor.
--
-- It defines the order of named dimensions (Output channels, Input channels, Kernel depth, Kernel height, Kernel width). The convolution operation uses this to interpret data in the weights tensor. For example, if @weightsLayout@ is @MPSGraphTensorNamedDataLayoutOIDHW@, frameork interprets data in weights tensor as @outputChannels x inputChannels x kernelDepth x kernelHeight x kernelWidth@ with @kernelWidth@ as fastest moving dimension.
--
-- ObjC selector: @- setWeightsLayout:@
setWeightsLayout :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setWeightsLayout mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setWeightsLayout:") retVoid [argCULong (coerce value)]

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelDepth x kernelWidth x kernelHeight@
--
-- ObjC selector: @- groups@
groups :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> IO CULong
groups mpsGraphConvolution3DOpDescriptor  =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "groups") retCULong []

-- | The number of partitions of the input and output channels.
--
-- The convolution operation divides input and output channels in @groups@ partitions. input channels in a group or partition are only connected to output channels in corresponding group. Number of weights the convolution needs is @outputFeatureChannels x inputFeatureChannels/groups x kernelDepth x kernelWidth x kernelHeight@
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSGraphConvolution3DOpDescriptor mpsGraphConvolution3DOpDescriptor => mpsGraphConvolution3DOpDescriptor -> CULong -> IO ()
setGroups mpsGraphConvolution3DOpDescriptor  value =
  sendMsg mpsGraphConvolution3DOpDescriptor (mkSelector "setGroups:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector :: Selector
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBack_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector :: Selector
descriptorWithStrideInX_strideInY_strideInZ_dilationRateInX_dilationRateInY_dilationRateInZ_groups_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:strideInZ:dilationRateInX:dilationRateInY:dilationRateInZ:groups:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector :: Selector
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom_paddingFront_paddingBackSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:paddingFront:paddingBack:"

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

-- | @Selector@ for @strideInZ@
strideInZSelector :: Selector
strideInZSelector = mkSelector "strideInZ"

-- | @Selector@ for @setStrideInZ:@
setStrideInZSelector :: Selector
setStrideInZSelector = mkSelector "setStrideInZ:"

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

-- | @Selector@ for @dilationRateInZ@
dilationRateInZSelector :: Selector
dilationRateInZSelector = mkSelector "dilationRateInZ"

-- | @Selector@ for @setDilationRateInZ:@
setDilationRateInZSelector :: Selector
setDilationRateInZSelector = mkSelector "setDilationRateInZ:"

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

-- | @Selector@ for @paddingFront@
paddingFrontSelector :: Selector
paddingFrontSelector = mkSelector "paddingFront"

-- | @Selector@ for @setPaddingFront:@
setPaddingFrontSelector :: Selector
setPaddingFrontSelector = mkSelector "setPaddingFront:"

-- | @Selector@ for @paddingBack@
paddingBackSelector :: Selector
paddingBackSelector = mkSelector "paddingBack"

-- | @Selector@ for @setPaddingBack:@
setPaddingBackSelector :: Selector
setPaddingBackSelector = mkSelector "setPaddingBack:"

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

