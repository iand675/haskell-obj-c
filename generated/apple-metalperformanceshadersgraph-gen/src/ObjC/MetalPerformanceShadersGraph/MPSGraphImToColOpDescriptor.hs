{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , paddingBottomSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingTopSelector
  , setDataLayoutSelector
  , setDilationRateInXSelector
  , setDilationRateInYSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , setKernelHeightSelector
  , setKernelWidthSelector
  , setPaddingBottomSelector
  , setPaddingLeftSelector
  , setPaddingRightSelector
  , setPaddingTopSelector
  , setStrideInXSelector
  , setStrideInYSelector
  , strideInXSelector
  , strideInYSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY paddingLeft paddingRight paddingTop paddingBottom dataLayout

-- | Creates column to image descriptor with given values for parameters. - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@ property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - dilationRateInX: See @dilationRateInX@ property.   - dilationRateInY: See @dilationRateInY@ property.   - dataLayout: See @dataLayout@ property. - Returns: A valid MPSGraphImToColOpDescriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphImToColOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayout kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphImToColOpDescriptor"
    sendClassMessage cls' descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY dataLayout

-- | Sets the descriptor's padding to the given values. - Parameters:   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphImToColOpDescriptor paddingLeft paddingRight paddingTop paddingBottom =
  sendMessage mpsGraphImToColOpDescriptor setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector paddingLeft paddingRight paddingTop paddingBottom

-- | The property that defines the kernel size in width dimension.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
kernelWidth mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor kernelWidthSelector

-- | The property that defines the kernel size in width dimension.
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setKernelWidth mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setKernelWidthSelector value

-- | The property that defines the kernel size  in height dimension.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
kernelHeight mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor kernelHeightSelector

-- | The property that defines the kernel size  in height dimension.
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setKernelHeight mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setKernelHeightSelector value

-- | The property that defines the stride in width dimension.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
strideInX mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor strideInXSelector

-- | The property that defines the stride in width dimension.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setStrideInXSelector value

-- | The property that defines the stride in height dimension.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
strideInY mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor strideInYSelector

-- | The property that defines the stride in height dimension.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setStrideInYSelector value

-- | The property that defines the dilation in width dimension.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
dilationRateInX mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor dilationRateInXSelector

-- | The property that defines the dilation in width dimension.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setDilationRateInXSelector value

-- | The property that defines the dilation in height dimension.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
dilationRateInY mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor dilationRateInYSelector

-- | The property that defines the dilation in height dimension.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setDilationRateInYSelector value

-- | The property that defines the padding in width dimension on the left side.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingLeft mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor paddingLeftSelector

-- | The property that defines the padding in width dimension on the left side.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setPaddingLeftSelector value

-- | The property that defines the padding in width dimension on the right side.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingRight mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor paddingRightSelector

-- | The property that defines the padding in width dimension on the right side.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setPaddingRightSelector value

-- | The property that defines the padding in height dimension at the top.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingTop mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor paddingTopSelector

-- | The property that defines the padding in height dimension at the top.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setPaddingTopSelector value

-- | The property that defines the padding in height dimension at the bottom.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO CULong
paddingBottom mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor paddingBottomSelector

-- | The property that defines the padding in height dimension at the bottom.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setPaddingBottomSelector value

-- | The property that defines the layout of source or output  tensor. e.g. @batch x channels x width x height@ for @NCHW@ layout
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphImToColOpDescriptor =
  sendMessage mpsGraphImToColOpDescriptor dataLayoutSelector

-- | The property that defines the layout of source or output  tensor. e.g. @batch x channels x width x height@ for @NCHW@ layout
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphImToColOpDescriptor mpsGraphImToColOpDescriptor => mpsGraphImToColOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphImToColOpDescriptor value =
  sendMessage mpsGraphImToColOpDescriptor setDataLayoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphTensorNamedDataLayout] (Id MPSGraphImToColOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:dataLayout:"

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphTensorNamedDataLayout] (Id MPSGraphImToColOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:dataLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector '[CULong, CULong, CULong, CULong] ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector '[CULong] ()
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector '[CULong] ()
setKernelHeightSelector = mkSelector "setKernelHeight:"

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

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector '[] MPSGraphTensorNamedDataLayout
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector '[MPSGraphTensorNamedDataLayout] ()
setDataLayoutSelector = mkSelector "setDataLayout:"

