{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for an image to column or column to image operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/imToColWithSourceTensor:descriptor:name:`` - ``MPSGraph/colToImWithSourceTensor:outputShape:descriptor:name:``
--
-- Generated bindings for @MPSGraphImToColOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphImToColOpDescriptor
  ( MPSGraphImToColOpDescriptor
  , IsMPSGraphImToColOpDescriptor(..)
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayout
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayout
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
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
  , dataLayout
  , setDataLayout
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , kernelWidthSelector
  , setKernelWidthSelector
  , kernelHeightSelector
  , setKernelHeightSelector
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
  , dataLayoutSelector
  , setDataLayoutSelector

  -- * Enum types
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

-- | Creates an image to column descriptor with given values for parameters. - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@ property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - dilationRateInX: See @dilationRateInX@ property.   - dilationRateInY: See @dilationRateInY@ property.   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.   - dataLayout: See @dataLayout@ property. - Returns: A valid MPSGraphImToColOpDescriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphImToColOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayout kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY paddingLeft paddingRight paddingTop paddingBottom dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphImToColOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (coerce dataLayout)] >>= retainedObject . castPtr

-- | Creates column to image descriptor with given values for parameters. - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@ property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - dilationRateInX: See @dilationRateInX@ property.   - dilationRateInY: See @dilationRateInY@ property.   - dataLayout: See @dataLayout@ property. - Returns: A valid MPSGraphImToColOpDescriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphImToColOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayout kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphImToColOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (coerce dataLayout)] >>= retainedObject . castPtr

-- | Sets the descriptor's padding to the given values. - Parameters:   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphImToColOpDescriptor  paddingLeft paddingRight paddingTop paddingBottom =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:") retVoid [argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom)]

-- | The property that defines the kernel size in width dimension.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
kernelWidth mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "kernelWidth") retCULong []

-- | The property that defines the kernel size in width dimension.
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setKernelWidth mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the kernel size  in height dimension.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
kernelHeight mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "kernelHeight") retCULong []

-- | The property that defines the kernel size  in height dimension.
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setKernelHeight mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the stride in width dimension.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
strideInX mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "strideInX") retCULong []

-- | The property that defines the stride in width dimension.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the stride in height dimension.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
strideInY mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "strideInY") retCULong []

-- | The property that defines the stride in height dimension.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the dilation in width dimension.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
dilationRateInX mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "dilationRateInX") retCULong []

-- | The property that defines the dilation in width dimension.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setDilationRateInX:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the dilation in height dimension.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
dilationRateInY mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "dilationRateInY") retCULong []

-- | The property that defines the dilation in height dimension.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setDilationRateInY:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the padding in width dimension on the left side.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingLeft mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "paddingLeft") retCULong []

-- | The property that defines the padding in width dimension on the left side.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setPaddingLeft:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the padding in width dimension on the right side.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingRight mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "paddingRight") retCULong []

-- | The property that defines the padding in width dimension on the right side.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setPaddingRight:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the padding in height dimension at the top.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingTop mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "paddingTop") retCULong []

-- | The property that defines the padding in height dimension at the top.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setPaddingTop:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the padding in height dimension at the bottom.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingBottom mpsGraphImToColOpDescriptor  =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "paddingBottom") retCULong []

-- | The property that defines the padding in height dimension at the bottom.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setPaddingBottom:") retVoid [argCULong (fromIntegral value)]

-- | The property that defines the layout of source or output  tensor. e.g. @batch x channels x width x height@ for @NCHW@ layout
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphImToColOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphImToColOpDescriptor (mkSelector "dataLayout") retCULong []

-- | The property that defines the layout of source or output  tensor. e.g. @batch x channels x width x height@ for @NCHW@ layout
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphImToColOpDescriptor  value =
  sendMsg mpsGraphImToColOpDescriptor (mkSelector "setDataLayout:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector :: Selector
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:"

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector :: Selector
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector
setKernelHeightSelector = mkSelector "setKernelHeight:"

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

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector
setDataLayoutSelector = mkSelector "setDataLayout:"

