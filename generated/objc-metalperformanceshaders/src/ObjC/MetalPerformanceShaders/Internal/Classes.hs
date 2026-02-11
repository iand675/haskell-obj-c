{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MetalPerformanceShaders.Internal.Classes (
    module ObjC.MetalPerformanceShaders.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Metal.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- ---------- MPSAccelerationStructureGroup ----------

-- | A group of acceleration structures which may be used together in an instance acceleration structure.
--
-- All acceleration structures in an instance acceleration structures must be created with the same group, although they do not all need to be used in the same instance acceleration structure. The acceleration structures in a group share internal GPU memory allocations, so the total number and size of acceleration structures that can be created with the same group is limited by the Metal device's buffer size limits. Therefore, do not group acceleration structures unless they are likely to be used in the same instance acceleration structure.
-- 
-- Phantom type for @MPSAccelerationStructureGroup@.
data MPSAccelerationStructureGroup

instance IsObjCObject (Id MPSAccelerationStructureGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSAccelerationStructureGroup"

class IsNSObject a => IsMPSAccelerationStructureGroup a where
  toMPSAccelerationStructureGroup :: a -> Id MPSAccelerationStructureGroup

instance IsMPSAccelerationStructureGroup (Id MPSAccelerationStructureGroup) where
  toMPSAccelerationStructureGroup = unsafeCastId

instance IsNSObject (Id MPSAccelerationStructureGroup) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionDescriptor ----------

-- | MPSCNNConvolutionDescriptor
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionDescriptor specifies a convolution descriptor
-- 
-- Phantom type for @MPSCNNConvolutionDescriptor@.
data MPSCNNConvolutionDescriptor

instance IsObjCObject (Id MPSCNNConvolutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionDescriptor"

class IsNSObject a => IsMPSCNNConvolutionDescriptor a where
  toMPSCNNConvolutionDescriptor :: a -> Id MPSCNNConvolutionDescriptor

instance IsMPSCNNConvolutionDescriptor (Id MPSCNNConvolutionDescriptor) where
  toMPSCNNConvolutionDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLossDataDescriptor ----------

-- | MPSCNNLossDataDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossDataDescriptor specifies a loss data descriptor.              The same descriptor can be used to initialize both the              labels and the optional weights data.
-- 
-- Phantom type for @MPSCNNLossDataDescriptor@.
data MPSCNNLossDataDescriptor

instance IsObjCObject (Id MPSCNNLossDataDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLossDataDescriptor"

class IsNSObject a => IsMPSCNNLossDataDescriptor a where
  toMPSCNNLossDataDescriptor :: a -> Id MPSCNNLossDataDescriptor

instance IsMPSCNNLossDataDescriptor (Id MPSCNNLossDataDescriptor) where
  toMPSCNNLossDataDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNLossDataDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLossDescriptor ----------

-- | MPSCNNLossDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossDescriptor specifies a loss filter descriptor.              The same descriptor can be used to initialize both the              MPSCNNLoss and the MPSNNLossGradient filters.
-- 
-- Phantom type for @MPSCNNLossDescriptor@.
data MPSCNNLossDescriptor

instance IsObjCObject (Id MPSCNNLossDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLossDescriptor"

class IsNSObject a => IsMPSCNNLossDescriptor a where
  toMPSCNNLossDescriptor :: a -> Id MPSCNNLossDescriptor

instance IsMPSCNNLossDescriptor (Id MPSCNNLossDescriptor) where
  toMPSCNNLossDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNLossDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNYOLOLossDescriptor ----------

-- | MPSCNNYOLOLossDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNYOLOLossDescriptor specifies a loss filter descriptor              that is used to create a MPSCNNLoss filter. The MPSCNNYOLOLoss is a filter that              has been specialized for object detection tasks and follows a specific layout              for the feature-channels of the input, output, weight and label data.
--
-- The layout of the data within the feature-channels is as follows:
--
-- Each anchorbox uses ( 2+2+1 + numberOfClasses = 5 + numberOfClasses ) feature channels.
--
-- Therefore the total number of feature channels used is: (5 + numberOfClasses) * numberOfAnchorBoxes.              The first feature channel for anchorbox index 'anchorIdx' is at fcIndex = (5 + numberOfClasses) * anchorIdx,              and the feature channels within each anchorbox are stored in the layout: 'XYWHCFFFFFF...', where (XY) are              the so-called raw x and y coordinates of the bounding box within each gridcell and (WH) are the corresponding              width and height. 'C' signifies a confidence for having an object in the cell and FFFFF... are the feature channel              values for each class of object to be classified in the object detector.
--
-- The YOLO-loss filter works by operating mostly independently on each anchorbox:                  *   The XY-channels of the inputs are first transformed to relative XY-values by applying the sigmoid-neuron on them,                      after which they are passed through the loss function defined by XYLossDescriptor, which is typically chosen                      to be the MPSCNNLossTypeMeanSquaredError type loss function.                  *   The WH-channels contain the raw width and height of the bounding box and they are operated with the                      loss function defined by WHLossDescriptor, which is typically of type MPSCNNLossTypeHuber.                  *   The C-channel contains the confidence value of having an object in the bounding box and it is operated                      by the loss function defined by confidenceLossDescriptor, which is typically chosen to be                      MPSCNNLossTypeSigmoidCrossEntropy.                  *   The FFFFF... (number of channels is number of classes) channels contains the raw feature channels for                      object classes, used to identify which objects are the most probable ones in the bounding box and                      these channels are passed through the loss function defined by classesLossDescriptor, which in                      typical cases is of the type MPSCNNLossTypeSoftMaxCrossEntropy.
--
-- For details on how to set up the label values and anchorboxes see https://arxiv.org/abs/1612.08242
-- 
-- Phantom type for @MPSCNNYOLOLossDescriptor@.
data MPSCNNYOLOLossDescriptor

instance IsObjCObject (Id MPSCNNYOLOLossDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNYOLOLossDescriptor"

class IsNSObject a => IsMPSCNNYOLOLossDescriptor a where
  toMPSCNNYOLOLossDescriptor :: a -> Id MPSCNNYOLOLossDescriptor

instance IsMPSCNNYOLOLossDescriptor (Id MPSCNNYOLOLossDescriptor) where
  toMPSCNNYOLOLossDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNYOLOLossDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCommandBuffer ----------

-- | MPSCommandBuffer
--
-- This depends on Metal.framework
--
-- A MPSCommandBuffer object is used to wrap an existing command buffer with MPS specific options.
--
-- A MPS kernel typically operates between a fixed set of inputs and outputs.              The MPSCommandBuffer class provides a way to add further encode-time parameters              to the encode call using the command buffer. Currently the only parameter included in the              MPSCommandBuffer that all MPS kernels support is the the predicate option,              which can be used to pre-empt the kernel from the GPU side.              NOTE: the options that contain metal resources will be referenced by this object and              therefore it is advisable to make the lifetime of this object as short as possible as is the              case for all command buffers.
-- 
-- Phantom type for @MPSCommandBuffer@.
data MPSCommandBuffer

instance IsObjCObject (Id MPSCommandBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCommandBuffer"

class IsNSObject a => IsMPSCommandBuffer a where
  toMPSCommandBuffer :: a -> Id MPSCommandBuffer

instance IsMPSCommandBuffer (Id MPSCommandBuffer) where
  toMPSCommandBuffer = unsafeCastId

instance IsNSObject (Id MPSCommandBuffer) where
  toNSObject = unsafeCastId

-- ---------- MPSImage ----------

-- | MPSImage
--
-- This depends on Metal.framework
--
-- A MPSImage object describes a MTLTexture that may have more than 4 channels.
--
-- Some image types, such as those found in convolutional neural networks (CNN)               differ from a standard texture in that they may have more than 4 channels              per image. While the channels could hold RGBA data, they will more commonly               hold a number of structural permutations upon a multi-channel image as the neural              network progresses. It is not uncommon for each pixel to have 32 or 64 channels               in it.
--
-- A standard MTLTexture may have no more than 4 channels. The additional              channels are stored in slices of 2d texture array (i.e. texture type is MTLTextureType2DArray)               such that 4 consecutive channels are stored in each slice of this array.              If the number of feature channels is N, number of array slices needed is (N+3)/4.              E.g. a CNN image with width 3 and height 2 with 9 channels will be stored as
--
-- slice 0   RGBA   RGBA  RGBA
-- RGBA   RGBA  RGBA
--
-- slice 1      RGBA   RGBA   RGBA
-- RGBA   RGBA   RGBA         (ASCII art /diagonal offset/ intended to show a Z dimension)
--
-- slice 2         R???   R???   R???
-- R???   R???   R???
--
-- The width and height of underlying 2d texture array is the same as the width and height of the MPSImage.              The array length is equal to (featureChannels + 3) / 4. Channels marked with ? are just              for padding and should not contain NaNs or Infs.
--
-- A MPSImage can be container of multiple CNN images for batch processing. In order to create a              MPSImage that contains N images, create MPSImageDescriptor with numberOfImages set to N.
--
-- Although a MPSImage can contain numberOfImages > 1, the actual number of images among these processed by MPSCNNKernel              is controlled by z-dimension of the clipRect. A MPSCNNKernel processes n=clipRect.size.depth images from this collection.              The starting source image index to process is given by offset.z. The starting index of the destination image is given by               clipRect.origin.z. The MPSCNNKernel takes n=clipRect.size.depth images from tje source at indices [offset.z, offset.z+n],               processes each independently and stores the result in the destination at indices [clipRect.origin.z, clipRect.origin.z+n]               respectively. Offset.z+n should be <= [src numberOfImage] and clipRect.origin.z+n should be <= [dest numberOfImages] and               offset.z must be >= 0.
--
-- Example: Suppose MPSCNNConvolution takes an input image with 8 channels and outputs an image with 16 channels. The number of              slices needed in the source 2d texture array is 2 and the number of slices needed in the destination 2d array is 4. Suppose               the source batch size is 5 and destination batch size is 4. (Multiple N-channel images can be processed concurrently in a               batch.) The number of source slices will be 2*5=10 and number of destination slices will be 4*4=16. If you want to process              just images 2 and 3 of the source and store the result at index 1 and 2 in the destination, you may achieve this by setting              offset.z=2, clipRect.origin.z=1 and clipRect.size.depth=2. MPSCNNConvolution will take, in this case, slice 4 and 5 of source and              produce slices 4 to 7 of destination. Similarly, slices 6 and 7 will be used to produce slices 8 to 11 of destination.
--
-- All MPSCNNKernels process images within each batch independently. That is, calling a MPSCNNKernel on an              batch is formally the same as calling it on each image in the batch one at a time. However, quite a lot of CPU and GPU overhead               will be avoided if batch processing is used. This is especially important for better performance on small images.
--
-- If the number of feature channels is <= 4 and numberOfImages = 1 i.e. only one slice is needed to represent a MPSImage, the underlying              metal texture type will be MTLTextureType2D rather than MTLTextureType2DArray.
--
-- There are also MPSTemporaryImages, intended for use for very short-lived image data that are produced and consumed              immediately in the same MTLCommandBuffer. They are a useful way to minimize CPU-side texture allocation costs and               greatly reduce the amount of memory used by your image pipeline.
--
-- Creation of the underlying texture may in some cases occur lazily.  You should              in general avoid calling MPSImage.texture except when unavoidable to avoid              materializing memory for longer than necessary. When possible, use the other MPSImage              properties to get information about the MPSImage instead.
--
-- Most MPSImages of 4 or fewer feature channels can generate quicklooks output in              Xcode for easy visualization of image data in the object. MPSTemporaryImages can not.
-- 
-- Phantom type for @MPSImage@.
data MPSImage

instance IsObjCObject (Id MPSImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImage"

class IsNSObject a => IsMPSImage a where
  toMPSImage :: a -> Id MPSImage

instance IsMPSImage (Id MPSImage) where
  toMPSImage = unsafeCastId

instance IsNSObject (Id MPSImage) where
  toNSObject = unsafeCastId

-- ---------- MPSImageDescriptor ----------

-- | MPSImageDescriptor
--
-- This depends on Metal.framework
--
-- A MPSImageDescriptor object describes a attributes of MPSImage and is used to              create one (see MPSImage discussion below)
-- 
-- Phantom type for @MPSImageDescriptor@.
data MPSImageDescriptor

instance IsObjCObject (Id MPSImageDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageDescriptor"

class IsNSObject a => IsMPSImageDescriptor a where
  toMPSImageDescriptor :: a -> Id MPSImageDescriptor

instance IsMPSImageDescriptor (Id MPSImageDescriptor) where
  toMPSImageDescriptor = unsafeCastId

instance IsNSObject (Id MPSImageDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSKernel ----------

-- | MPSKernel
--
-- This depends on Metal.framework
--
-- The MPSKernel class is the base class for all MPS objects.  It defines a standard interface for              MPS kernels.   You should not use the MPSKernel class directly. Instead, a  number of MPSKernel               subclasses are available in MetalPerformanceShaders.framework that define specific high-performance              image processing operations.
--
-- The basic sequence for applying a MPSKernel to an image is as follows:
--
-- 1.  Create a MPSKernel corresponding to the operation you wish to perform:
--
-- MPSImageSobel *sobel = [[MPSImageSobel alloc] initWithDevice: mtlDevice];
--
-- 2.  Encode the filter into a command buffer:
--
-- sobel.offset = ...;
-- sobel.clipRect = ...;
-- sobel.options = ...;
-- [sobel encodeToCommandBuffer: commandBuffer
-- sourceTexture: inputImage
-- destinationTexture: resultImage ];
--
-- Encoding the kernel merely encodes the operation into a MTLCommandBuffer. It does not modify any pixels, yet.                  All MPSKernel state has been copied to the command buffer. MPSKernels may be reused.  If the texture was previously                  operated on by another command encoder (e.g. MTLRenderCommandEncoder), you should call -endEncoding on the other                  encoder before encoding the filter.
--
-- Some MPS filters work in place (inputImage = resultImage) even in situations where Metal might not                  normally allow in place operation on textures. If in-place operation is desired, you may attempt to call                  [MPSKernel encodeKernelInPlace...]. If the operation can not be completed in place, then                  NO will be returned and you will have to create a new result texture and try again. To make an in-place                  image filter reliable, pass a fallback MPSCopyAllocator to the method to create a new texture to write                  to in the event that a filter can not operate in place.
--
-- (Repeat steps 2 for more filters, as desired.)
--
-- It should be self evident that step 2 may not be thread safe. That is, you can not have                      multiple threads manipulating the same properties on the same MPSKernel object at the                      same time and achieve coherent output. In common usage, the MPSKernel properties don't                      often need to be changed from their default values, but if you need to apply the same                      filter to multiple images on multiple threads with cropping / tiling, make additional                      MPSKernel objects per thread. They are cheap. You can use multiple MPSKernel objects on                      multiple threads, as long as only one thread is operating on any particular MPSKernel                      object at a time.
--
-- 3.  After encoding any additional work to the command buffer using other encoders, submit the MTLCommandBuffer                  to your MTLCommandQueue, using:
--
-- [mtlCommandBuffer commit];
--
-- A MPSKernel can be saved to disk / network using NSCoders such as NSKeyedArchiver.              When decoding, the system default MTLDevice will be chosen unless the NSCoder adopts              the <MPSDeviceProvider> protocol.  To accomplish this, subclass or extend your unarchiver               to add this method.
-- 
-- Phantom type for @MPSKernel@.
data MPSKernel

instance IsObjCObject (Id MPSKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSKernel"

class IsNSObject a => IsMPSKernel a where
  toMPSKernel :: a -> Id MPSKernel

instance IsMPSKernel (Id MPSKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrix ----------

-- | MPSMatrix
--
-- This depends on Metal.framework
--
-- A MPSMatrix object describes a set of 2-dimensional arrays of data and provides storage              for its values.  MPSMatrix objects serve as inputs and outputs of MPSMatrixKernel              objects.
--
-- Implementation note:              A MPSMatrix object maintains its internal storage using a MTLBuffer object and thus              the same rules for maintaining coherency of a MTLBuffer's data between CPU memory and GPU              memory apply to a MPSMatrix.  An MPSMatrix object's data refers to an array of matrices.              Data is assumed to be ordered by matrix first, followed by row, followed by column.
--
-- For example, index [i,j] of the k'th matrix of an MPSMatrix is located at byte offset:                       k * matrixBytes + i * rowBytes + j * sizeof(dataType)
--
-- Where matrixBytes is a multiple of rowBytes at least equal to rows * rowBytes.
-- 
-- Phantom type for @MPSMatrix@.
data MPSMatrix

instance IsObjCObject (Id MPSMatrix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrix"

class IsNSObject a => IsMPSMatrix a where
  toMPSMatrix :: a -> Id MPSMatrix

instance IsMPSMatrix (Id MPSMatrix) where
  toMPSMatrix = unsafeCastId

instance IsNSObject (Id MPSMatrix) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixCopyDescriptor ----------

-- | A list of copy operations
--
-- The MPSMatrixCopy filter can do multiple copy operations.  For RNN filters, these              copies are often small, and are more efficient when grouped together.              The MPSMatriceCopyDescriptor provides a container to list the operations.              The operations occur in any order, and may not alias.
-- 
-- Phantom type for @MPSMatrixCopyDescriptor@.
data MPSMatrixCopyDescriptor

instance IsObjCObject (Id MPSMatrixCopyDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixCopyDescriptor"

class IsNSObject a => IsMPSMatrixCopyDescriptor a where
  toMPSMatrixCopyDescriptor :: a -> Id MPSMatrixCopyDescriptor

instance IsMPSMatrixCopyDescriptor (Id MPSMatrixCopyDescriptor) where
  toMPSMatrixCopyDescriptor = unsafeCastId

instance IsNSObject (Id MPSMatrixCopyDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixDescriptor ----------

-- | MPSMatrixDescriptor
--
-- This depends on Metal.framework
--
-- A MPSMatrixDescriptor describes the sizes, strides, and data type of a              an array of 2-dimensional matrices.  All storage is assumed to be in              "matrix-major".  See the description for MPSMatrix for further details.
-- 
-- Phantom type for @MPSMatrixDescriptor@.
data MPSMatrixDescriptor

instance IsObjCObject (Id MPSMatrixDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixDescriptor"

class IsNSObject a => IsMPSMatrixDescriptor a where
  toMPSMatrixDescriptor :: a -> Id MPSMatrixDescriptor

instance IsMPSMatrixDescriptor (Id MPSMatrixDescriptor) where
  toMPSMatrixDescriptor = unsafeCastId

instance IsNSObject (Id MPSMatrixDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixRandomDistributionDescriptor ----------

-- | MPSMatrixRandomDistributionDescriptor
--
-- This depends on Metal.framework
--
-- Decribes properties of a distribution of random values.
-- 
-- Phantom type for @MPSMatrixRandomDistributionDescriptor@.
data MPSMatrixRandomDistributionDescriptor

instance IsObjCObject (Id MPSMatrixRandomDistributionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixRandomDistributionDescriptor"

class IsNSObject a => IsMPSMatrixRandomDistributionDescriptor a where
  toMPSMatrixRandomDistributionDescriptor :: a -> Id MPSMatrixRandomDistributionDescriptor

instance IsMPSMatrixRandomDistributionDescriptor (Id MPSMatrixRandomDistributionDescriptor) where
  toMPSMatrixRandomDistributionDescriptor = unsafeCastId

instance IsNSObject (Id MPSMatrixRandomDistributionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArray ----------

-- | MPSNDArray
--
-- A MPSNDArray object is a MTLBuffer based storage container for multi-dimensional data.
--
-- Operations on MPSNDArrays will commonly implicitly reshape the multidimensional              structure into a 2-dimensional structure by reinterpreting higher dimensions as a single dimensional              array of matrix rows. For example a [a, b, c, d] NDArray passed to a matrix multiplication may              be implicitly reinterpreted as a [a*b*c, d] matrix and a 2D matrix multiplication performed.              In practice, the major row (the dimension in which successive elements appear adjacent to one              another in memory) is the 0th dimension (represented as 'd' in the above example).  It has both a              dimension size indicating the number of elements and a storage size which may be slightly bigger              to allow for performance improvement arising from better data alignment in memory.  In principle,              the rowBytes may also be used to create a 0th-dimension slice out of a larger array stored in the              underlying MTLBuffer.
--
-- MPS will automatically manage the storage size of the major row ("rowBytes") though you may              set it in the descriptor if you have a need to do so. Generally, it should be at least a multiple              of 16 bytes.   Dimensions after the 0th are a densely packed array of rows of size rowBytes.              Thus, the 1st dimension is an array of rows. The 2nd dimension is an array of arrays of rows with              identical size, and so forth.  When the reduction to 2 dimensions is done, no data is moved. MPS              just reinterprets a higher order N-1 dimensions of matrix rows as a single large 1-dimensional              array of rows.
--
-- It is a common desire to reorder the dimensions of NDArrays or define a subregion thereof. A transpose              or slice operation is performed by making a MPSNDArray view of the original. The dimensions to transpose              or slice are given by the descriptor for the new view. If both a transpose and slice operation are defined,              then the slice is performed first and the result of the slice is transposed. Because many MPS kernels can              operate on transposed data at speed, MPS will usually defer doing a physical transpose operation until later,              when it becomes clear that one is actually required. For this reason, conversions to formats that do not              support deferred transposes and slices such as MPSMatrix MPSVector view or using -exportWithCommandBuffer:              toBuffer:offset:rowStrides, may cause substantial new computation to be done and new memory to be allocated.              These should be avoided except when necessary.  As a general rule, transposes that do not involve the 0th              dimension should be able to be handled by nearly everything natively. MPSNDArrayMatrixMultiplication and reductions              can handle 0th dimension transposes. Other filters may insert a physical repacking operation. If you wish              to force a physical repacking use MPSAliasingStrategyShallNotAlias. To avoid confusion with aliased NDArrays              the parent property is provided.  MPSNDArrays that alias share a common ancestor.
-- 
-- Phantom type for @MPSNDArray@.
data MPSNDArray

instance IsObjCObject (Id MPSNDArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArray"

class IsNSObject a => IsMPSNDArray a where
  toMPSNDArray :: a -> Id MPSNDArray

instance IsMPSNDArray (Id MPSNDArray) where
  toMPSNDArray = unsafeCastId

instance IsNSObject (Id MPSNDArray) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayDescriptor ----------

-- | MPSNDArrayDescriptor
--
-- This depends on Metal.framework
--
-- A MPSNDArrayDescriptor object describes a attributes of MPSNDArray and is used to              create one (see MPSNDArray discussion below)
-- 
-- Phantom type for @MPSNDArrayDescriptor@.
data MPSNDArrayDescriptor

instance IsObjCObject (Id MPSNDArrayDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayDescriptor"

class IsNSObject a => IsMPSNDArrayDescriptor a where
  toMPSNDArrayDescriptor :: a -> Id MPSNDArrayDescriptor

instance IsMPSNDArrayDescriptor (Id MPSNDArrayDescriptor) where
  toMPSNDArrayDescriptor = unsafeCastId

instance IsNSObject (Id MPSNDArrayDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayQuantizationDescriptor ----------

-- | MPSNDArrayQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Common methods for quantization descriptors
-- 
-- Phantom type for @MPSNDArrayQuantizationDescriptor@.
data MPSNDArrayQuantizationDescriptor

instance IsObjCObject (Id MPSNDArrayQuantizationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayQuantizationDescriptor"

class IsNSObject a => IsMPSNDArrayQuantizationDescriptor a where
  toMPSNDArrayQuantizationDescriptor :: a -> Id MPSNDArrayQuantizationDescriptor

instance IsMPSNDArrayQuantizationDescriptor (Id MPSNDArrayQuantizationDescriptor) where
  toMPSNDArrayQuantizationDescriptor = unsafeCastId

instance IsNSObject (Id MPSNDArrayQuantizationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNNDefaultPadding ----------

-- | This class provides some pre-rolled padding policies for common tasks
--
-- You are, of course, welcome to write your own class that conforms to              The MPSNNPadding protocol and use that instead.
-- 
-- Phantom type for @MPSNNDefaultPadding@.
data MPSNNDefaultPadding

instance IsObjCObject (Id MPSNNDefaultPadding) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNDefaultPadding"

class IsNSObject a => IsMPSNNDefaultPadding a where
  toMPSNNDefaultPadding :: a -> Id MPSNNDefaultPadding

instance IsMPSNNDefaultPadding (Id MPSNNDefaultPadding) where
  toMPSNNDefaultPadding = unsafeCastId

instance IsNSObject (Id MPSNNDefaultPadding) where
  toNSObject = unsafeCastId

-- ---------- MPSNNFilterNode ----------

-- | MPSNNFilterNode
--
-- A placeholder node denoting a neural network filter stage
--
-- There are as many MPSNNFilterNode subclasses as there are              MPS neural network filter objects. Make one of those.               This class defines an polymorphic interface for them.
-- 
-- Phantom type for @MPSNNFilterNode@.
data MPSNNFilterNode

instance IsObjCObject (Id MPSNNFilterNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNFilterNode"

class IsNSObject a => IsMPSNNFilterNode a where
  toMPSNNFilterNode :: a -> Id MPSNNFilterNode

instance IsMPSNNFilterNode (Id MPSNNFilterNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNFilterNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNImageNode ----------

-- | MPSNNImageNode
--
-- A placeholder node denoting the position of a MPSImage in a graph
--
-- MPS neural network graphs are made up of filter nodes connected by              image (or state) nodes. An image node is produced by one filter but              may be consumed by more than one filter.
--
-- Most image nodes will be created by MPS and made available through              MPSNNFilterNode.resultImage. Image nodes that are not created by MPS              (i.e. "the graph inputs") must be created by you.
-- 
-- Phantom type for @MPSNNImageNode@.
data MPSNNImageNode

instance IsObjCObject (Id MPSNNImageNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNImageNode"

class IsNSObject a => IsMPSNNImageNode a where
  toMPSNNImageNode :: a -> Id MPSNNImageNode

instance IsMPSNNImageNode (Id MPSNNImageNode) where
  toMPSNNImageNode = unsafeCastId

instance IsNSObject (Id MPSNNImageNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNNeuronDescriptor ----------

-- | MPSNNNeuronDescriptor
--
-- This depends on Metal.framework
--
-- The MPSNNNeuronDescriptor specifies a neuron descriptor.              Supported neuron types:
--
-- Neuron type "none": f(x) = x              Parameters: none
--
-- ReLU neuron filter: f(x) = x >= 0 ? x : a * x              This is called Leaky ReLU in literature. Some literature defines              classical ReLU as max(0, x). If you want this behavior, simply pass a = 0.              Parameters: a              For default behavior, set the value of a to 0.0f.
--
-- Linear neuron filter: f(x) = a * x + b              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 0.0f.
--
-- Sigmoid neuron filter: f(x) = 1 / (1 + e^-x)              Parameters: none
--
-- Hard Sigmoid filter: f(x) = clamp((x * a) + b, 0, 1)              Parameters: a, b              For default behavior, set the value of a to 0.2f and the value of b to 0.5f.
--
-- Hyperbolic tangent (TanH) neuron filter: f(x) = a * tanh(b * x)              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 1.0f.
--
-- Absolute neuron filter: f(x) = fabs(x)              Parameters: none
--
-- Parametric Soft Plus neuron filter: f(x) = a * log(1 + e^(b * x))              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 1.0f.
--
-- Parametric Soft Sign neuron filter: f(x) = x / (1 + abs(x))              Parameters: none
--
-- Parametric ELU neuron filter: f(x) = x >= 0 ? x : a * (exp(x) - 1)              Parameters: a              For default behavior, set the value of a to 1.0f.
--
-- Parametric ReLU (PReLU) neuron filter: Same as ReLU, except parameter              aArray is per channel.              For each pixel, applies the following function: f(x_i) = x_i, if x_i >= 0                                                                     = a_i * x_i if x_i < 0              i in [0...channels-1]              i.e. parameters a_i are learned and applied to each channel separately. Compare              this to ReLu where parameter a is shared across all channels.              See https://arxiv.org/pdf/1502.01852.pdf for details.              Parameters: aArray - Array of floats containing per channel value of PReLu parameter                          count - Number of float values in array aArray.
--
-- ReLUN neuron filter: f(x) = min((x >= 0 ? x : a * x), b)              Parameters: a, b              As an example, the TensorFlow Relu6 activation layer can be implemented              by setting the parameter b to 6.0f:              https://www.tensorflow.org/api_docs/cc/class/tensorflow/ops/relu6.              For default behavior, set the value of a to 1.0f and the value of b to 6.0f.
-- 
-- Phantom type for @MPSNNNeuronDescriptor@.
data MPSNNNeuronDescriptor

instance IsObjCObject (Id MPSNNNeuronDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNNeuronDescriptor"

class IsNSObject a => IsMPSNNNeuronDescriptor a where
  toMPSNNNeuronDescriptor :: a -> Id MPSNNNeuronDescriptor

instance IsMPSNNNeuronDescriptor (Id MPSNNNeuronDescriptor) where
  toMPSNNNeuronDescriptor = unsafeCastId

instance IsNSObject (Id MPSNNNeuronDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNNOptimizerDescriptor ----------

-- | MPSNNOptimizerDescriptor
--
-- The MPSNNOptimizerDescriptor base class. Optimizers are generally used to update trainable neural network parameters.              Users are usually expected to call these MPSKernels from the update methods on their Convolution or BatchNormalization data sources.
--
-- Before the gradient is used to update the original value, some preprocessing occurs on each gradient where it is scaled or clipped              If regularization is chosen the appropriate regularization loss gradient is added to the value gradient.
-- 
-- Phantom type for @MPSNNOptimizerDescriptor@.
data MPSNNOptimizerDescriptor

instance IsObjCObject (Id MPSNNOptimizerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNOptimizerDescriptor"

class IsNSObject a => IsMPSNNOptimizerDescriptor a where
  toMPSNNOptimizerDescriptor :: a -> Id MPSNNOptimizerDescriptor

instance IsMPSNNOptimizerDescriptor (Id MPSNNOptimizerDescriptor) where
  toMPSNNOptimizerDescriptor = unsafeCastId

instance IsNSObject (Id MPSNNOptimizerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNNStateNode ----------

-- | MPSNNStateNode
--
-- A placeholder node denoting the position in the graph of a MPSState object
--
-- Some filters need additional information about an image in order to function. For example              a max-pooling gradient filter needs to know which position the max result came from in the              original pooling filter in order to select the right data for gradient computation.  In other cases,              state may be moved into a MPSState object in order to keep the filter itself immutable.              The MPSState object typically encapsulates one or more MTLResource objects.
-- 
-- Phantom type for @MPSNNStateNode@.
data MPSNNStateNode

instance IsObjCObject (Id MPSNNStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNStateNode"

class IsNSObject a => IsMPSNNStateNode a where
  toMPSNNStateNode :: a -> Id MPSNNStateNode

instance IsMPSNNStateNode (Id MPSNNStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSPolygonBuffer ----------

-- | A vertex buffer and optional index and mask buffer for a set of polygons
-- 
-- Phantom type for @MPSPolygonBuffer@.
data MPSPolygonBuffer

instance IsObjCObject (Id MPSPolygonBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSPolygonBuffer"

class IsNSObject a => IsMPSPolygonBuffer a where
  toMPSPolygonBuffer :: a -> Id MPSPolygonBuffer

instance IsMPSPolygonBuffer (Id MPSPolygonBuffer) where
  toMPSPolygonBuffer = unsafeCastId

instance IsNSObject (Id MPSPolygonBuffer) where
  toNSObject = unsafeCastId

-- ---------- MPSPredicate ----------

-- | MPSPredicate
--
-- This depends on Metal.framework
--
-- A MPSPredicate can be used to run MPS kernels subject to a predicate.
--
-- The MPSPredicate defines a way to refrain running a kernel on the GPU              based on values computed on the GPU. That way one can build control flow operations              that do the decisions on the GPU side mitigating the need to synchronize CPU and GPU              execution. The predicate is used with the version of encode calls that take              a object of type MPSKernelEncodeOptions as a parameter (
--
-- See: MPSCNNKernel for example).              The code associated with the kernel's encode call is executed on the GPU if and only if              the predicate is considered to be true.              NOTE: It is advisable to release MPSPredicate objects promptly as they take a reference              to a MTLBuffer object and therefore can keep the memory allocated for long periods of time.
-- 
-- Phantom type for @MPSPredicate@.
data MPSPredicate

instance IsObjCObject (Id MPSPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSPredicate"

class IsNSObject a => IsMPSPredicate a where
  toMPSPredicate :: a -> Id MPSPredicate

instance IsMPSPredicate (Id MPSPredicate) where
  toMPSPredicate = unsafeCastId

instance IsNSObject (Id MPSPredicate) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNDescriptor ----------

-- | MPSRNNDescriptor
--
-- This depends on Metal.framework
--
-- The MPSRNNDescriptor specifies a Recursive neural network block/layer descriptor.
-- 
-- Phantom type for @MPSRNNDescriptor@.
data MPSRNNDescriptor

instance IsObjCObject (Id MPSRNNDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNDescriptor"

class IsNSObject a => IsMPSRNNDescriptor a where
  toMPSRNNDescriptor :: a -> Id MPSRNNDescriptor

instance IsMPSRNNDescriptor (Id MPSRNNDescriptor) where
  toMPSRNNDescriptor = unsafeCastId

instance IsNSObject (Id MPSRNNDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSSVGFDefaultTextureAllocator ----------

-- | A default implementation of the MPSSVGFTextureAllocator protocol. Maintains a cache of textures which is checked first when a texture is requested. If there is no suitable texture in the cache, allocates a texture directly from the Metal device.
-- 
-- Phantom type for @MPSSVGFDefaultTextureAllocator@.
data MPSSVGFDefaultTextureAllocator

instance IsObjCObject (Id MPSSVGFDefaultTextureAllocator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSSVGFDefaultTextureAllocator"

class IsNSObject a => IsMPSSVGFDefaultTextureAllocator a where
  toMPSSVGFDefaultTextureAllocator :: a -> Id MPSSVGFDefaultTextureAllocator

instance IsMPSSVGFDefaultTextureAllocator (Id MPSSVGFDefaultTextureAllocator) where
  toMPSSVGFDefaultTextureAllocator = unsafeCastId

instance IsNSObject (Id MPSSVGFDefaultTextureAllocator) where
  toNSObject = unsafeCastId

-- ---------- MPSSVGFDenoiser ----------

-- | A convenience object which uses an MPSSVGF object to manage the denoising process
--
-- The MPSSVGF object can also be used directly to customize the denoising process. This object keeps track of auxilary textures used by the MPSSVGF object, manages a temporal history, and encodes the entire denoising process into a command buffer.
--
-- To use this class, first create and customize an MPSSVGF object. This object allows you to tweak various aspect of the denoising process such as temporal reprojection and bilateral blur settings. Then create a texture allocator object which will allocate temporary textures during denoising. This can either be an object conforming to the MPSSVGFTextureAllocator protocol or an instance of the MPSSVGFDefaultTextureAllocator class. Next, create an MPSSVGFDenoiser object. To perform denoising, assign inputs textures to the denoiser object's properties and call encodeToCommandBuffer:. Finally, read the output from the destinationTexture property. Note that this class can denoise up to two independent textures simultaneously, e.g. specular and diffuse, direct and indirect lighting, shadows and AO, etc.
--
-- MPSSVGF *svgf = [[MPSSVGF alloc] initWithDevice:device];
--
-- // configure svgf properties
--
-- MPSSVGFDefaultTextureAllocator *allocator =
-- [[MPSSVGFDefaultTextureAllocator alloc] initWithDevice:device];
--
-- MPSSVGFDenoiser *denoiser = [[MPSSVGFDenoiser alloc] initWithSVGF:svgf
-- textureAllocator:allocator];
--
-- // configure denoiser properties
--
-- denoiser.sourceTexture = noisyTexture;
-- denoiser.depthNormalTexture = depthNormalTexture;
-- denoiser.previousDepthNormalTexture = depthNormalTextureFromPreviousFrame;
-- denoiser.motionVectorTexture = motionVectorTexture;
--
-- [denoiser encodeToCommandBuffer:commandBuffer];
--
-- id <MTLTexture> cleanTexture = denoiser.destinationTexture;
-- 
-- Phantom type for @MPSSVGFDenoiser@.
data MPSSVGFDenoiser

instance IsObjCObject (Id MPSSVGFDenoiser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSSVGFDenoiser"

class IsNSObject a => IsMPSSVGFDenoiser a where
  toMPSSVGFDenoiser :: a -> Id MPSSVGFDenoiser

instance IsMPSSVGFDenoiser (Id MPSSVGFDenoiser) where
  toMPSSVGFDenoiser = unsafeCastId

instance IsNSObject (Id MPSSVGFDenoiser) where
  toNSObject = unsafeCastId

-- ---------- MPSState ----------

-- | MPSState
--
-- This depends on Metal Framework
--
-- A semi-opaque data container for large storage in MPS CNN filters
--
-- Some MPS CNN kernels produce additional information beyond a                  MPSImage. These may be pooling indices where the result came from,                  convolution weights, or other information not contained in the                  usual MPSImage result from a MPSCNNKernel. A MPSState object                   typically contains one or more expensive MTLResources such as                   textures or buffers to store this information.  It provides a                   base class with interfaces for managing this storage. Child                   classes may add additional functionality specific to their                  contents.
--
-- Some MPSState objects are temporary. Temporary state objects,                   like MPSTemporaryImages and Matrices, are for very short lived storage,                  perhaps just a few lines of code within the scope of a single                  MTLCommandBuffer.  They are very efficient for storage, as several                  temporary objects can share the same memory over the course of a                   MTLCommandBuffer. This can improve both memory usage and time spent                  in the kernel wiring down memory and such. You may find that some                   large CNN tasks can not be computed without them, as non-temporary                  storage would simply take up too much memory.
--
-- In exchange, the lifetime of the underlying storage in temporary                   MPSState objects needs to be carefully managed. ARC often waits                   until the end of scope to release objects. Temporary storage often                   needs to be released sooner than that. Consequently the lifetime of                   the data in the underlying MTLResources is managed by a readCount                   property. Each time a MPSCNNKernel reads a temporary MPSState object                  the readCount is automatically decremented. When it reaches zero, the                  underlying storage is recycled for use by other MPS temporary objects,                  and the data is becomes undefined.  If you need to consume the data                  multiple times, you should set the readCount to a larger number to                   prevent the data from becomming undefined.  You may set the readCount                  to 0 yourself to return the storage to MPS, if for any reason, you                   realize that the MPSState object will no longer be used.
--
-- The contents of a temporary MPSState object are only valid from                   creation to the time the readCount reaches 0. The data is only valid                   for the MTLCommandBuffer on which it was created.  Non-temporary                  MPSState objects are valid on any MTLCommandBuffer on the same                  device until they are released.
--
-- Finally, temporary MPSState objects are complicated to use with blit encoders.                  Your application should assume that the temporary MPSState is backed by a MTLHeap,                  and consequently needs a MTLFence to ensure that compute command encoders and other                  encoders do not trip over one another with heap based memory. MPS will almost never                  use a blit encoder for this reason. If you do need one, then you will need to make                  a new compute encoder to block on whatever previous compute encoder last used the                  heap block. (MPS will not tell you who previously used the heap block. That encoder                  is almost certainly long dead anyway.) If concurrent encoders are involved, then a                  barrier might be needed. Within that compute encoder, you will call -updateFence.                  End the compute encoder, make a blit encoder wait for the fence, do the blit, update                  a new fence, then make a new compute encoder, wait for the second fence, then you                  can continue. Possibly the second do-nothing compute encoder needs to be ended so                  MPS can be called. Frankly, we don't bother with blit encoders and just write a compute                  operation for copy / clear as needed, or better yet find a way to eliminate the                  clear / copy pass so we don't have to pay for it. Your application needs to use                  temporary MPSStates and MPSTemporaryImages. Memory costs skyrocket, otherwise.                  It is the blit encoder that is hopefully optional. Note: the most common use of a                  blit encoder, -synchronizeResource: can not encounter this problem because temporary                  images and states live in GPU private memory and can not be read by the CPU.
-- 
-- Phantom type for @MPSState@.
data MPSState

instance IsObjCObject (Id MPSState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSState"

class IsNSObject a => IsMPSState a where
  toMPSState :: a -> Id MPSState

instance IsMPSState (Id MPSState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSState) where
  toNSObject = unsafeCastId

-- ---------- MPSStateResourceList ----------

-- | Phantom type for @MPSStateResourceList@.
data MPSStateResourceList

instance IsObjCObject (Id MPSStateResourceList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSStateResourceList"

class IsNSObject a => IsMPSStateResourceList a where
  toMPSStateResourceList :: a -> Id MPSStateResourceList

instance IsMPSStateResourceList (Id MPSStateResourceList) where
  toMPSStateResourceList = unsafeCastId

instance IsNSObject (Id MPSStateResourceList) where
  toNSObject = unsafeCastId

-- ---------- MPSVector ----------

-- | MPSVector
--
-- This depends on Metal.framework
--
-- A MPSVector object describes a 1-dimensional array of data and provides storage              for its values.  Some MPSMatrixKernel objects operate on MPSVector objects              for convenience.
-- 
-- Phantom type for @MPSVector@.
data MPSVector

instance IsObjCObject (Id MPSVector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSVector"

class IsNSObject a => IsMPSVector a where
  toMPSVector :: a -> Id MPSVector

instance IsMPSVector (Id MPSVector) where
  toMPSVector = unsafeCastId

instance IsNSObject (Id MPSVector) where
  toNSObject = unsafeCastId

-- ---------- MPSVectorDescriptor ----------

-- | MPSVectorDescriptor
--
-- This depends on Metal.framework
--
-- A MPSVectorDescriptor describes the length and data type of a              an array of 1-dimensional vectors.  All vectors are stored as              contiguous arrays of data.
-- 
-- Phantom type for @MPSVectorDescriptor@.
data MPSVectorDescriptor

instance IsObjCObject (Id MPSVectorDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSVectorDescriptor"

class IsNSObject a => IsMPSVectorDescriptor a where
  toMPSVectorDescriptor :: a -> Id MPSVectorDescriptor

instance IsMPSVectorDescriptor (Id MPSVectorDescriptor) where
  toMPSVectorDescriptor = unsafeCastId

instance IsNSObject (Id MPSVectorDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDepthWiseConvolutionDescriptor ----------

-- | MPSCNNDepthWiseConvolutionDescriptor can be used to create MPSCNNConvolution object that does depthwise convolution
--
-- Depthwise convolution applies different filter to each input feature channel i.e. no cross channel mixing.                    Number of outputFeatureChannels can be greater than number of inputFeatureChannels, in which case convolution                    expects channelMultipler = outputFeactureChannels/inputFeatureChannels number of filters for each input channel.                    This means channelMultipler filters are applied to each input feature channel producing channelMultipler output feature channels.                    All channelMultipler output feature channels produced by single input feature channel are stored togather in output image i.e.                              output[x,y,k*channelMultiplier + q] = input[x,y,k] * filter[k,q]                    where * here denotes convolution.                    group must be 1.                    Weights array returned by MPSCNNConvolutionDataProvier is interpreted as                              Weights [inputFeatureChannels] [channelMultiplier] [kH] [kW]                            = Weights [ inputFeatureChannels * channelMultiplier ] [kH] [kW]                            = Weights [ outputFeatureChannels ] [kH] [kW]
--
-- Currently only channel multipler of 1 is supported i.e. inputFeatureChannels == outputFeatureChannels
-- 
-- Phantom type for @MPSCNNDepthWiseConvolutionDescriptor@.
data MPSCNNDepthWiseConvolutionDescriptor

instance IsObjCObject (Id MPSCNNDepthWiseConvolutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDepthWiseConvolutionDescriptor"

class IsMPSCNNConvolutionDescriptor a => IsMPSCNNDepthWiseConvolutionDescriptor a where
  toMPSCNNDepthWiseConvolutionDescriptor :: a -> Id MPSCNNDepthWiseConvolutionDescriptor

instance IsMPSCNNDepthWiseConvolutionDescriptor (Id MPSCNNDepthWiseConvolutionDescriptor) where
  toMPSCNNDepthWiseConvolutionDescriptor = unsafeCastId

instance IsMPSCNNConvolutionDescriptor (Id MPSCNNDepthWiseConvolutionDescriptor) where
  toMPSCNNConvolutionDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNDepthWiseConvolutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSubPixelConvolutionDescriptor ----------

-- | MPSCNNSubPixelConvolutionDescriptor can be used to create MPSCNNConvolution object that does sub pixel upsamling                    and reshaping opeartion as described in                        http://www.cv-foundation.org/openaccess/content_cvpr_2016/papers/Shi_Real-Time_Single_Image_CVPR_2016_paper.pdf
--
-- Conceptually MPSCNNConvolution with subPixelScaleFactor > 1 can be thought of as filter performing regular CNN convolution producing N output feature channels at each pixel of                   an intermediate MPSImage followed by a kernel that rearranges/reshapes these N channels at each pixel of intermediate MPSImage into a pixel block of                   size subPixelScaleFactor x subPixelScaleFactor with N/(subPixelScaleFactor * subPixelScaleFactor) featureChannels at each pixel of this pixel block. Thus each pixel in intermedaite                   MPSImage with N channels map to subPixelScaleFactor x subPixelScaleFactor pixel block in final destination MPSImage with N/(subPixelScaleFactor * subPixelScaleFactor) featureChannels.                   MPSCNNConvolution with subPixelScaleFactor > 1 fuses the convolution and reshaping operation into single compute kernel thus not only saving DRAM roundtrip but also memory                   needed for intermediate MPSImage had these operation done separately.                   Let N be the value of outputFeatureChannels property and let r = subPixelScaleFactor.                   Conceptually Convolution will produce intermedaite image Io of dimensions (treated as 3D tensor) width x height x N where                              width = (clipRect.size.width + r - 1) / r                              height = (clipRect.size.height + r -1) / r                   Reshaping happens as follows
--
-- Destination[clipRect.origin.x+x][clipRect.origin.y+y][c] = Io[ floor(x/r) ][ floor(y/r) ][ (N/r^2) * ( r * mod(y,r) + mod(x,r) ) + c ]
-- where x in [0,clipRect.size.width-1], y in [0,clipRect.size.height-1], c in [0,N/r^2 - 1]
--
-- The following conditions must be met:                   1) N (outputFeatureChannels) must be multiple of r^2 (subPixelScaleFactor * subPixelScaleFactor).                   2) The destination MPSImage to encode call must have at least N/r^2 + destinationFeatureChannelOffset channels.                   3) Number of feature channels in reshaped output image (N/r^2) can be any value when groups = 1 but must be multiple of 4 when groups > 1.
-- 
-- Phantom type for @MPSCNNSubPixelConvolutionDescriptor@.
data MPSCNNSubPixelConvolutionDescriptor

instance IsObjCObject (Id MPSCNNSubPixelConvolutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSubPixelConvolutionDescriptor"

class IsMPSCNNConvolutionDescriptor a => IsMPSCNNSubPixelConvolutionDescriptor a where
  toMPSCNNSubPixelConvolutionDescriptor :: a -> Id MPSCNNSubPixelConvolutionDescriptor

instance IsMPSCNNSubPixelConvolutionDescriptor (Id MPSCNNSubPixelConvolutionDescriptor) where
  toMPSCNNSubPixelConvolutionDescriptor = unsafeCastId

instance IsMPSCNNConvolutionDescriptor (Id MPSCNNSubPixelConvolutionDescriptor) where
  toMPSCNNConvolutionDescriptor = unsafeCastId

instance IsNSObject (Id MPSCNNSubPixelConvolutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSTemporaryImage ----------

-- | MPSTemporaryImage
--
-- MPSImage
--
-- MPSTemporaryImages are for MPSImages with short lifetimes.
--
-- What is temporary memory? It is memory, plain and simple. Analogy: If we              use an app as an analogy for a command buffer, then "Regular memory"              (such as what backs a MPSImage or the typical MTLTexture) would be memory               that you allocate at launch and never free. Temporary memory would be memory               that you free when you are done with it so it can be used for something               else as needed later in your app.  You /could/ write your app to allocate               everything you will ever need up front, but this is very inefficient and               quite frankly a pain to plan out in advance. You don't do it for your app,              so why would you do it for your command buffers?
--
-- Welcome to the 1970's! We have added a heap.
--
-- Unsurprisingly, MPSTemporaryImages can provide for profound reduction in              the the amount of memory used by your application.  Like malloc, MPS               maintains a heap of memory usable in a command buffer. Over the lifetime               of a command buffer, the same piece of memory may be reused many times.               This means that each time the same memory is reused, it aliases with previous              uses. If we aren't careful, we might find that needed data is overwritten              by successive allocations. However, this is no different than accessing freed              memory only to discover it doesn't contain what you thought it did anymore,               so you should be able to keep out of trouble by following a few simple rules,              like with malloc.
--
-- To this end, we added some restrictions to help you out and get a bit more               performance. Some comments are appended in parentheses below to extend the              analogy of command buffer = program:
--
-- - The textures are MTLStorageModePrivate. You can not, for example, use                [MTLTexture getBytes...] or [MTLTexture replaceRegion...] with them.                 MPSTemporaryImages are strictly read and written by the GPU. (There is                protected memory to prevent other processes from overwriting your heap.)
--
-- - The temporary image may be used only on a single MTLCommandBuffer.                This limits the chronology to a single linear time stream. (The heap                is specific to just one command buffer. There are no mutexes to                coordinate timing of simultaneous access by multiple GPUs. Nor are we                likely to like them if there were. So, we disallow it.)
--
-- - The readCount property must be managed correctly. Please see                the description of the readCount property for full details. (The readCount                is a reference count for the block of memory that holds your data. The                usual undefined behaviors apply to reading data that has been released.                We assert when we can to prevent that from happening accidentally,                just as a program might segfault. The readCount counts procedural users                 of the object -- MPSKernel.encode... calls that read the MPSTemporaryImage.                 As each reads from it, the readCount is automatically decremented. The                 texture data will be freed in typical usage at the right time as the                 readCount reaches zero, typically with little user involvement other                than to set the readCount up front. We did examine using the main MPSTemporaryImage                reference count for this instead so that ARC would do work for you automatically.                Alas, ARC destroys things at end of scope rather than promptly, sometimes resulting                in greatly increased memory usage. These allocations are large! So, we                 use this method instead.)
--
-- Since MPSTemporaryImages can only be used with a single MTLCommandBuffer,              and can not be used off the GPU, they generally should not be kept               around past the completion of the MTLCommandBuffer. The lifetime of              MPSTemporaryImages is expected to be typically extremely short, perhaps               only a few lines of code. Like malloc, it is intended to be fairly cheap               to make MPSTemporaryImages and throw them away. Please do so.
--
-- To keep the lifetime of the underlying texture allocation as short as               possible, the underlying texture is not allocated until the first time              the MPSTemporaryImage is used by a MPSCNNKernel or the .texture property              is read. The readCount property serves to limit the lifetime on the              other end.
--
-- You may use the MPSTemporaryImage.texture with MPSUnaryImageKernel -encode... methods,              iff featureChannels <= 4 and the MTLTexture conforms to requirements of that MPSKernel.              There is no locking mechanism provided to prevent a MTLTexture returned               from the .texture property from becoming invalid when the readCount reaches 0.
--
-- Finally, MPSTemporaryImages are complicated to use with blit encoders.              Your application should assume that the MPSTemporaryImage is backed by a MTLHeap,              and consequently needs a MTLFence to ensure that compute command encoders and other              encoders do not trip over one another with heap based memory. MPS will almost never              use a blit encoder for this reason. If you do need one, then you will need to make              a new compute encoder to block on whatever previous compute encoder last used the              heap block. (MPS will not tell you who previously used the heap block. That encoder              is almost certainly long dead anyway.) If concurrent encoders are involved, then a              barrier might be needed. Within that compute encoder, you will call -updateFence.              End the compute encoder, make a blit encoder wait for the fence, do the blit, update              a new fence, then make a new compute encoder, wait for the second fence, then you              can continue. Possibly the second do-nothing compute encoder needs to be ended so              MPS can be called. Frankly, we don't bother with blit encoders and just write a compute              operation for copy / clear as needed, or better yet find a way to eliminate the              clear / copy pass so we don't have to pay for it. Note: the most common use of a              blit encoder, -synchronizeResource: can not encounter this problem because              MPSTemporaryImages live in GPU private memory and can not be read by the CPU.
--
-- MPSTemporaryImages can otherwise be used wherever MPSImages are used.
-- 
-- Phantom type for @MPSTemporaryImage@.
data MPSTemporaryImage

instance IsObjCObject (Id MPSTemporaryImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTemporaryImage"

class IsMPSImage a => IsMPSTemporaryImage a where
  toMPSTemporaryImage :: a -> Id MPSTemporaryImage

instance IsMPSTemporaryImage (Id MPSTemporaryImage) where
  toMPSTemporaryImage = unsafeCastId

instance IsMPSImage (Id MPSTemporaryImage) where
  toMPSImage = unsafeCastId

instance IsNSObject (Id MPSTemporaryImage) where
  toNSObject = unsafeCastId

-- ---------- MPSAccelerationStructure ----------

-- | A data structure built over geometry used to accelerate ray tracing
--
-- Do not use this base class directly. Use one of the derived classes instead. The general pattern for creating an acceleration structure is as follows. First, create the acceleration structure:
--
-- MPSTriangleAccelerationStructure *accelerationStructure = nil;
-- accelerationStructure = [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- Then, assign values to the acceleration structure's properties:
--
-- accelerationStructure.vertexBuffer = vertexBuffer;
-- accelerationStructure.triangleCount = triangleCount;
--
-- Finally, the acceleration structure must be built:
--
-- [accelerationStructure rebuild];
--
-- The acceleration structure can then be used to encode ray intersection tests with an MPSRayIntersector:
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- Asynchronous Acceleration Structure Builds: Rebuilding an acceleration structure is an expensive operation. Note that there is also a method to rebuild the acceleration structure asynchronously to avoid blocking the main thread.
--
-- [accelerationStructure rebuildWithCompletionHandler:^(MPSAccelerationStructure *accel) {
-- // Kick off ray intersection work
-- }];
--
-- Streaming Geometry Updates: It is generally safe to change buffer properties such as the vertex buffer after intersection tests have been encoded into a command buffer, but the contents of those buffers cannot be safely changed by the CPU until the command buffer has finished executing on the GPU. It is also not safe to rebuild the acceleration structure until the command buffer has completed.
--
-- If the CPU needs to stream geometry updates to the GPU, ensure the vertex and other buffers are double or triple buffered.
--
-- #define MAX_ASYNC_OPERATIONS 3
--
-- // Initialization:
--
-- // Create a semaphore with the maximum number of asynchronous operations in flight
-- dispatch_semaphore_t asyncOperationSemaphore = dispatch_semaphore_create(MAX_ASYNC_OPERATIONS);
--
-- // Create an acceleration structure for each vertex buffer range
-- NSMutableArray *accelerationStructures = [NSMutableArray array];
--
-- NSUInteger vertexBufferLength = sizeof(float3) * vertexCount * MAX_ASYNC_OPERATIONS;
-- id <MTLBuffer> vertexBuffer = [device newBufferWithLength:vertexBufferLength
-- options:MTLResourceStorageModeManaged];
--
-- for (NSUInteger i = 0; i < MAX_ASYNC_OPERATIONS; i++) {
-- MPSTriangleAccelerationStructure *accel = nil;
-- accel = [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- // Configure acceleration structure
-- accel.vertexBuffer = vertexBuffer;
-- accel.vertexBufferOffset = i * sizeof(float3) * vertexCount;
--
-- [accelerationStructures addObject:accel];
-- }
--
-- NSUInteger asyncOperationIndex = 0;
--
-- // Encode intersection testing:
--
-- // Wait until there is a free acceleration structure
-- dispatch_semaphore_wait(asyncOperationSemaphore, DISPATCH_TIME_FOREVER);
--
-- MPSTriangleAccelerationStructure *accel = accelerationStructures[asyncOperationIndex];
-- asyncOperationIndex = (asyncOperationIndex + 1) % MAX_ASYNC_OPERATIONS;
--
-- float3 *vertices = (float3 *)((uint8_t *)vertexBuffer.contents + accel.vertexBufferOffset);
-- // Update vertices
-- MPSDidModifyRange(vertexBuffer, NSMakeRange(accel.vertexBufferOffset, sizeof(float3) * vertexCount));
--
-- // Rebuild the acceleration structure
-- [accel rebuild];
--
-- // Encode actual intersection work
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:rayBufferOffset
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:intersectionBufferOffset
-- rayCount:rayCount
-- accelerationStructure:accel];
--
-- // Register a completion handler to run when the GPU finishes executing
-- [commandBuffer addCompletedHandler:^(id <MTLCommandBuffer> commandBuffer) {
-- Intersection *intersections = (Intersection *)((uint8_t *)intersectionBuffer.contents +
-- intersectionBufferOffset);
--
-- // Process intersections
--
-- // Signal that the acceleration structure is now available for reuse
-- dispatch_semaphore_signal(asyncOperationSemaphore);
-- }];
--
-- // Commit the command buffer to allow the GPU to start executing
-- [commandBuffer commit];
--
-- Refitting acceleration structures: If geometry has only moved slightly and not added or removed from the scene, it can be much faster to refit the existing topology of an acceleration structure to the new geometry than to rebuild the acceleration structure from scratch. Refitting can also be pipelined with other GPU work such as intersection testing. If the geometry is transformed entirely on the GPU, it is not necessary to use double or triple buffering. For example:
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- id <MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:untransformedVertexBuffer offset:0 atIndex:0];
--
-- [encoder setBuffer:accelerationStructure.vertexBuffer
-- offset:accelerationStructure.vertexBufferOffset
-- atIndex:1];
--
-- [encoder setBuffer:transformationMatrices offset:0 atIndex:2];
--
-- [encoder setComputePipelineState:transformVerticesPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(accelerationStructure.triangleCount * 3, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
--
-- [accelerationStructure encodeRefitToCommandBuffer:commandBuffer];
--
-- [commandBuffer commit];
--
-- Serializing Acceleration Structures: Instead of rebuilding acceleration structures from scratch they can be built offline, serialized, and reloaded at runtime using the NSSecureCoding protocol:
--
-- // Build time:
-- NSError *error = nil;
-- NSData *data = [NSKeyedArchiver archivedDataWithRootObject:accel
-- requiringSecureCoding:YES
-- error:&error];
--
-- if (!data)
-- NSLog(@"Error archiving MPSAccelerationStructure: %@",
-- error.localizedDescription);
--
-- // Runtime:
-- MPSTriangleAccelerationStructure *accel;
-- accel = [NSKeyedUnarchiver unarchivedObjectOfClass:[MPSTriangleAccelerationStructure class]
-- fromData:data
-- error:&error];
--
-- if (!accel)
-- NSLog(@"Error unarchiving MPSAccelerationStructure: %@",
-- error.localizedDescription);
--
-- Copying Acceleration Structures: Acceleration structures can be copied using the NSCopying protocol, even to a different Metal device. This can be used for multi-GPU raytracing. Buffer properties are not copied to the new acceleration structure. These buffers must instead be copied to the new Metal device and assigned to the new acceleration structure. For example:
--
-- MPSTriangleAccelerationStructure *copy = [accelerationStructure copyWithZone:nil
-- device:newDevice];
--
-- copy.vertexBuffer = [self copyBuffer:accelerationStructure.vertexBuffer
-- withDevice:newDevice];
--
-- Performance Guidelines:
--
-- - Provide accurate acceleration structure hints: if an acceleration structure does not       require support for refitting, a higher quality construction algorithm can be used.       However, if an acceleration structure must be rebuilt frequently, a lower quality       but higher performance construction algorithm can be used.
--
-- - Consider refitting existing acceleration structures rather than rebuilding them from       scratch. This is typically much faster and can result in a reasonably high quality       tree if the geometry has not been modified dramatically. Refitting can also be pipelined       with other GPU work. If objects have been added to or removed from the scene, it is       typically necessary to rebuild the acceleration structure rather than refit it.
--
-- - Rebuild acceleration structures asynchronously when possible to avoid blocking the main       thread. Consider presenting a UI indicating that work is happening in the background while       allowing the user to consider interacting with your application.
--
-- - If you need to mix intersection testing with acceleration structure builds (e.g. if the       user is interactively editing the scene while rendering or if objects are moving       significantly) consider allocating two independent acceleration structures that refer to       two copies of the scene data. Then, asynchronously rebuild one acceleration structure       while the other one is used for rendering. Once the rebuild has completed, swap the       acceleration structures. The intermediate frames could be filled by refitting the       rendering acceleration structure until the rebuilt acceleration structure is ready.
--
-- - When running in Xcode, disable "Enable Backtrace Recording" in your scheme settings.       Enabling this setting can significantly increase acceleration structure build time.
--
-- - Consider using quadrilaterals instead of triangles to represent your geometry.       The cost of intersecting a quadrilateral is typically less than the cost of intersecting       two triangles, so quadrilaterals can improve performance. Quadrilaterals also typically       require 30-40% less memory than triangles including vertex data and internal buffers       allocated by the acceleration structure. Whether quadrilaterals improve or hurt       performance can depend on the geometry and ray distribution, so you should choose       whichever performs better for your application.
--
-- Thread Safety: MPSAccelerationStructures are generally not thread safe. Changing properties and rebuilding acceleration structures from multiple threads result in undefined behavior. However, it is safe to encode intersection tests with a single acceleration structure from multiple threads as long as each thread uses its own MPSRayIntersector.
-- 
-- Phantom type for @MPSAccelerationStructure@.
data MPSAccelerationStructure

instance IsObjCObject (Id MPSAccelerationStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSAccelerationStructure"

class IsMPSKernel a => IsMPSAccelerationStructure a where
  toMPSAccelerationStructure :: a -> Id MPSAccelerationStructure

instance IsMPSAccelerationStructure (Id MPSAccelerationStructure) where
  toMPSAccelerationStructure = unsafeCastId

instance IsMPSKernel (Id MPSAccelerationStructure) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSAccelerationStructure) where
  toNSObject = unsafeCastId

-- ---------- MPSBinaryImageKernel ----------

-- | MPSBinaryImageKernel
--
-- This depends on Metal.framework
--
-- A MPSBinaryImageKernel consumes two MTLTextures and produces one MTLTexture.
-- 
-- Phantom type for @MPSBinaryImageKernel@.
data MPSBinaryImageKernel

instance IsObjCObject (Id MPSBinaryImageKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSBinaryImageKernel"

class IsMPSKernel a => IsMPSBinaryImageKernel a where
  toMPSBinaryImageKernel :: a -> Id MPSBinaryImageKernel

instance IsMPSBinaryImageKernel (Id MPSBinaryImageKernel) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSKernel (Id MPSBinaryImageKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSBinaryImageKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBinaryKernel ----------

-- | MPSCNNBinaryKernel
--
-- This depends on Metal.framework
--
-- Describes a convolution neural network kernel.
--
-- A MPSCNNKernel consumes two MPSImages, primary and secondary, and produces one MPSImage.
-- 
-- Phantom type for @MPSCNNBinaryKernel@.
data MPSCNNBinaryKernel

instance IsObjCObject (Id MPSCNNBinaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBinaryKernel"

class IsMPSKernel a => IsMPSCNNBinaryKernel a where
  toMPSCNNBinaryKernel :: a -> Id MPSCNNBinaryKernel

instance IsMPSCNNBinaryKernel (Id MPSCNNBinaryKernel) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBinaryKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBinaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNKernel ----------

-- | MPSCNNKernel
--
-- This depends on Metal.framework
--
-- Describes a convolution neural network kernel.
--
-- A MPSCNNKernel consumes one MPSImage and produces one MPSImage.
--
-- The region overwritten in the destination MPSImage is described              by the clipRect.  The top left corner of the region consumed (ignoring              adjustments for filter size -- e.g. convolution filter size) is given              by the offset. The size of the region consumed is a function of the              clipRect size and any subsampling caused by pixel strides at work,              e.g. MPSCNNPooling.strideInPixelsX/Y.  Where the offset + clipRect              would cause a {x,y} pixel address not in the image to be read, the              edgeMode is used to determine what value to read there.
--
-- The Z/depth component of the offset, clipRect.origin and clipRect.size              indexes which images to use. If the MPSImage contains only a single image              then these should be offset.z = 0, clipRect.origin.z = 0              and clipRect.size.depth = 1. If the MPSImage contains multiple images,              clipRect.size.depth refers to number of images to process. Both source              and destination MPSImages must have at least this many images. offset.z              refers to starting source image index. Thus offset.z + clipRect.size.depth must              be <= source.numberOfImages. Similarly, clipRect.origin.z refers to starting              image index in destination. So clipRect.origin.z + clipRect.size.depth must be              <= destination.numberOfImage.
--
-- destinationFeatureChannelOffset property can be used to control where the MPSKernel will              start writing in feature channel dimension. For example, if the destination image has              64 channels, and MPSKernel outputs 32 channels, by default channels 0-31 of destination              will be populated by MPSKernel. But if we want this MPSKernel to populate channel 32-63              of the destination, we can set destinationFeatureChannelOffset = 32.              A good example of this is concat (concatenation) operation in Tensor Flow. Suppose              we have a src = w x h x Ni which goes through CNNConvolution_0 which produces              output O0 = w x h x N0 and CNNConvolution_1 which produces output O1 = w x h x N1 followed              by concatenation which produces O = w x h x (N0 + N1). We can achieve this by creating              an MPSImage with dimensions O = w x h x (N0 + N1) and using this as destination of              both convolutions as follows                  CNNConvolution0: destinationFeatureChannelOffset = 0, this will output N0 channels starting at                                   channel 0 of destination thus populating [0,N0-1] channels.                  CNNConvolution1: destinationFeatureChannelOffset = N0, this will output N1 channels starting at                                   channel N0 of destination thus populating [N0,N0+N1-1] channels.
--
-- A MPSCNNKernel can be saved to disk / network using NSCoders such as NSKeyedArchiver.               When decoding, the system default MTLDevice will be chosen unless the NSCoder adopts               the <MPSDeviceProvider> protocol.  To accomplish this you will likely need to subclass your              unarchiver to add this method.
-- 
-- Phantom type for @MPSCNNKernel@.
data MPSCNNKernel

instance IsObjCObject (Id MPSCNNKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNKernel"

class IsMPSKernel a => IsMPSCNNKernel a where
  toMPSCNNKernel :: a -> Id MPSCNNKernel

instance IsMPSCNNKernel (Id MPSCNNKernel) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNMultiaryKernel ----------

-- | MPSCNNMultiaryKernel
--
-- This depends on Metal.framework
--
-- Describes a  neural network kernel with multiple image sources.
--
-- A MPSCNNKernel consumes multiple MPSImages, possibly a MPSState, and produces one MPSImage.
-- 
-- Phantom type for @MPSCNNMultiaryKernel@.
data MPSCNNMultiaryKernel

instance IsObjCObject (Id MPSCNNMultiaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNMultiaryKernel"

class IsMPSKernel a => IsMPSCNNMultiaryKernel a where
  toMPSCNNMultiaryKernel :: a -> Id MPSCNNMultiaryKernel

instance IsMPSCNNMultiaryKernel (Id MPSCNNMultiaryKernel) where
  toMPSCNNMultiaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNMultiaryKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNMultiaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSImageCopyToMatrix ----------

-- | MPSImageCopyToMatrix
--
-- The MPSImageCopyToMatrix copies image data to a MPSMatrix.              The image data is stored in a row of a matrix.  The dataLayout              specifies the order in which the feature channels in the MPSImage              get stored in the matrix.  If MPSImage stores a batch of images,              the images are copied into multiple rows, one row per image.
--
-- The number of elements in a row in the matrix must be >= image width *               image height * number of featureChannels in the image.
-- 
-- Phantom type for @MPSImageCopyToMatrix@.
data MPSImageCopyToMatrix

instance IsObjCObject (Id MPSImageCopyToMatrix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageCopyToMatrix"

class IsMPSKernel a => IsMPSImageCopyToMatrix a where
  toMPSImageCopyToMatrix :: a -> Id MPSImageCopyToMatrix

instance IsMPSImageCopyToMatrix (Id MPSImageCopyToMatrix) where
  toMPSImageCopyToMatrix = unsafeCastId

instance IsMPSKernel (Id MPSImageCopyToMatrix) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageCopyToMatrix) where
  toNSObject = unsafeCastId

-- ---------- MPSImageEDLines ----------

-- | MPSImageEDLines
--
-- The MPSImageEDLInes class implements the EDLines line segmenting algorithm using edge-drawing (ED)              described here              https://ieeexplore.ieee.org/document/6116138
--
-- The EDLInes algorithm consists of 5 steps, the first 4 of which describe the ED algorithm:              1. Blur the source image using a Gaussian blur with a sigma parameter              2. Use horizontal and vertical Sobel filters to find a gradient magnitude and                direction.                  G = sqrt(Sx^2 + Sy^2)                  G_ang = arctan(Sy / Sx)              3. Compute anchor points, points with a high probability of being edge pixels.                Anchor points are local maxima, in the gradient image that lie on row and column                multiples of the detailRatio. This parameter effectively downsamples the gradient                image, and directly influences the density of anchor points. A larger detailRatio results                in fewer fine grained details, leaving long, main lines.              4. Anchor points are traced in a forward and backward direction along the gradient direction, until                the gradient falls below some gradientThreshold parameter or the edge of the image is reached.                The paths traced become an edge map of the image.              5. Points in the edges are fit to a line), and extended along the edge until the line error crosses a                lineErrorThreshold. Lines which are beyond a minimum length are labelled line segments and                will be outputs of the algorithm.
-- 
-- Phantom type for @MPSImageEDLines@.
data MPSImageEDLines

instance IsObjCObject (Id MPSImageEDLines) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageEDLines"

class IsMPSKernel a => IsMPSImageEDLines a where
  toMPSImageEDLines :: a -> Id MPSImageEDLines

instance IsMPSImageEDLines (Id MPSImageEDLines) where
  toMPSImageEDLines = unsafeCastId

instance IsMPSKernel (Id MPSImageEDLines) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageEDLines) where
  toNSObject = unsafeCastId

-- ---------- MPSImageFindKeypoints ----------

-- | MPSImageFindKeypoints
--
-- The MPSImageFindKeypoints kernel is used to find a list of keypoints whose values are >= minimumPixelThresholdValue              in MPSImageKeypointRangeInfo. The keypoints are generated for a specified region in the image.                The pixel format of the source image must be MTLPixelFormatR8Unorm.
-- 
-- Phantom type for @MPSImageFindKeypoints@.
data MPSImageFindKeypoints

instance IsObjCObject (Id MPSImageFindKeypoints) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageFindKeypoints"

class IsMPSKernel a => IsMPSImageFindKeypoints a where
  toMPSImageFindKeypoints :: a -> Id MPSImageFindKeypoints

instance IsMPSImageFindKeypoints (Id MPSImageFindKeypoints) where
  toMPSImageFindKeypoints = unsafeCastId

instance IsMPSKernel (Id MPSImageFindKeypoints) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageFindKeypoints) where
  toNSObject = unsafeCastId

-- ---------- MPSImageGuidedFilter ----------

-- | MPSImageGuidedFilter
--
-- Perform Guided Filter to produce a coefficients image              The filter is broken into two stages:                  - Regression                  - Reconstruction
--
-- The regression stage learns a 4-channel "coefficient" texture (typically at a very low resolution),              and represents the per-pixel linear regression of the source texture to the guidance texture.
--
-- The reconstruction stage upsamples the coefficeints to the same size as the final output and              then at each pixel computes the inner product to produce the output.
--
-- The filter is broken into two stages to allow coefficients to be filtered (such as for example - temporally filtering for video to prevent flicker).
--
-- There is also support for an optional weight texture that can be used to discard values in the source data.
--
-- Guided Filter is described at https://arxiv.org/pdf/1505.00996.pdf.
-- 
-- Phantom type for @MPSImageGuidedFilter@.
data MPSImageGuidedFilter

instance IsObjCObject (Id MPSImageGuidedFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageGuidedFilter"

class IsMPSKernel a => IsMPSImageGuidedFilter a where
  toMPSImageGuidedFilter :: a -> Id MPSImageGuidedFilter

instance IsMPSImageGuidedFilter (Id MPSImageGuidedFilter) where
  toMPSImageGuidedFilter = unsafeCastId

instance IsMPSKernel (Id MPSImageGuidedFilter) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageGuidedFilter) where
  toNSObject = unsafeCastId

-- ---------- MPSImageHistogram ----------

-- | MPSImageHistogram
--
-- The MPSImageHistogram computes the histogram of an image.
-- 
-- Phantom type for @MPSImageHistogram@.
data MPSImageHistogram

instance IsObjCObject (Id MPSImageHistogram) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageHistogram"

class IsMPSKernel a => IsMPSImageHistogram a where
  toMPSImageHistogram :: a -> Id MPSImageHistogram

instance IsMPSImageHistogram (Id MPSImageHistogram) where
  toMPSImageHistogram = unsafeCastId

instance IsMPSKernel (Id MPSImageHistogram) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageHistogram) where
  toNSObject = unsafeCastId

-- ---------- MPSImageNormalizedHistogram ----------

-- | MPSImageNormalizedHistogram
--
-- The MPSImageNormalizedHistogram computes the normalized histogram of an image.              The minimum and maximum pixel values for a given region of an image are first computed.              The max(computed minimum pixel value, MPSImageHistogramInfo.minPixelValue) and the              min(computed maximum pixel value, MPSImageHistogramInfo.maxPixelValue) are used to              compute the normalized histogram.
-- 
-- Phantom type for @MPSImageNormalizedHistogram@.
data MPSImageNormalizedHistogram

instance IsObjCObject (Id MPSImageNormalizedHistogram) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageNormalizedHistogram"

class IsMPSKernel a => IsMPSImageNormalizedHistogram a where
  toMPSImageNormalizedHistogram :: a -> Id MPSImageNormalizedHistogram

instance IsMPSImageNormalizedHistogram (Id MPSImageNormalizedHistogram) where
  toMPSImageNormalizedHistogram = unsafeCastId

instance IsMPSKernel (Id MPSImageNormalizedHistogram) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageNormalizedHistogram) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixBinaryKernel ----------

-- | MPSMatrixBinaryKernel
--
-- This depends on Metal.framework
--
-- A MPSMatrixBinaryKernel consumes two MPSMatrix objects and produces              one MPSMatrix object.
-- 
-- Phantom type for @MPSMatrixBinaryKernel@.
data MPSMatrixBinaryKernel

instance IsObjCObject (Id MPSMatrixBinaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixBinaryKernel"

class IsMPSKernel a => IsMPSMatrixBinaryKernel a where
  toMPSMatrixBinaryKernel :: a -> Id MPSMatrixBinaryKernel

instance IsMPSMatrixBinaryKernel (Id MPSMatrixBinaryKernel) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSMatrixBinaryKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixBinaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixCopy ----------

-- | Phantom type for @MPSMatrixCopy@.
data MPSMatrixCopy

instance IsObjCObject (Id MPSMatrixCopy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixCopy"

class IsMPSKernel a => IsMPSMatrixCopy a where
  toMPSMatrixCopy :: a -> Id MPSMatrixCopy

instance IsMPSMatrixCopy (Id MPSMatrixCopy) where
  toMPSMatrixCopy = unsafeCastId

instance IsMPSKernel (Id MPSMatrixCopy) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixCopy) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixCopyToImage ----------

-- | MPSMatrixCopyToImage
--
-- The MPSMatrixCopyToImage copies matrix data to a MPSImage.              The operation is the reverse of MPSImageCopyToMatrix.
-- 
-- Phantom type for @MPSMatrixCopyToImage@.
data MPSMatrixCopyToImage

instance IsObjCObject (Id MPSMatrixCopyToImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixCopyToImage"

class IsMPSKernel a => IsMPSMatrixCopyToImage a where
  toMPSMatrixCopyToImage :: a -> Id MPSMatrixCopyToImage

instance IsMPSMatrixCopyToImage (Id MPSMatrixCopyToImage) where
  toMPSMatrixCopyToImage = unsafeCastId

instance IsMPSKernel (Id MPSMatrixCopyToImage) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixCopyToImage) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixMultiplication ----------

-- | MPSMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix multiplication kernel.
--
-- A MPSMatrixMultiplication object computes:
--
-- C = alpha * op(A) * op(B) + beta * C
--
-- A, B, and C are matrices which are represented by MPSMatrix              objects. alpha and beta are scalar values (of the same data type              as values of C) which are applied as shown above.  A and B may              each have an optional transposition operation applied.
--
-- A, B, and C (also referred to in later discussions as the left input              matrix, the right input matrix, and the result matrix respectively).
--
-- A MPSMatrixMultiplication object is initialized with the transpose              operators to apply to A and B, sizes for the operation to perform,              and the scalar values alpha and beta.
-- 
-- Phantom type for @MPSMatrixMultiplication@.
data MPSMatrixMultiplication

instance IsObjCObject (Id MPSMatrixMultiplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixMultiplication"

class IsMPSKernel a => IsMPSMatrixMultiplication a where
  toMPSMatrixMultiplication :: a -> Id MPSMatrixMultiplication

instance IsMPSMatrixMultiplication (Id MPSMatrixMultiplication) where
  toMPSMatrixMultiplication = unsafeCastId

instance IsMPSKernel (Id MPSMatrixMultiplication) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixMultiplication) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixRandom ----------

-- | MPSMatrixRandom
--
-- Kernels that implement random number generation.
-- 
-- Phantom type for @MPSMatrixRandom@.
data MPSMatrixRandom

instance IsObjCObject (Id MPSMatrixRandom) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixRandom"

class IsMPSKernel a => IsMPSMatrixRandom a where
  toMPSMatrixRandom :: a -> Id MPSMatrixRandom

instance IsMPSMatrixRandom (Id MPSMatrixRandom) where
  toMPSMatrixRandom = unsafeCastId

instance IsMPSKernel (Id MPSMatrixRandom) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixRandom) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSum ----------

-- | MPSMatrixSum
--
-- This depends on Metal.framework
--
-- MPSMatrixSum performs a pointwise summation of N MPSMatrix              objects and applies an optional bias term and neuron activation              function.
--
-- MPSMatrix A = empty matrix;              for (i = 0; i < N; ++i)                  A += alpha[i]*B[i];
--
-- if (bias)                  A += broadcast(bias);
--
-- if (neuron)                  A = applyNeuron(A);
--
-- Where B is the array of MPSMatrix objects, A is the destination              MPSMatrix, alpha is an array of scalar values, bias is a vector              which is broadcast and accumulated across each row of the intermediate              result, and applyNeuron is a neuron activation function.
--
-- Each matrix in the array may have an independent origin.
-- 
-- Phantom type for @MPSMatrixSum@.
data MPSMatrixSum

instance IsObjCObject (Id MPSMatrixSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSum"

class IsMPSKernel a => IsMPSMatrixSum a where
  toMPSMatrixSum :: a -> Id MPSMatrixSum

instance IsMPSMatrixSum (Id MPSMatrixSum) where
  toMPSMatrixSum = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSum) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSum) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixUnaryKernel ----------

-- | MPSMatrixUnaryKernel
--
-- This depends on Metal.framework
--
-- A MPSMatrixUnaryKernel consumes one MPSMatrix and produces one MPSMatrix.
-- 
-- Phantom type for @MPSMatrixUnaryKernel@.
data MPSMatrixUnaryKernel

instance IsObjCObject (Id MPSMatrixUnaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixUnaryKernel"

class IsMPSKernel a => IsMPSMatrixUnaryKernel a where
  toMPSMatrixUnaryKernel :: a -> Id MPSMatrixUnaryKernel

instance IsMPSMatrixUnaryKernel (Id MPSMatrixUnaryKernel) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSMatrixUnaryKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixUnaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayMultiaryBase ----------

-- | Phantom type for @MPSNDArrayMultiaryBase@.
data MPSNDArrayMultiaryBase

instance IsObjCObject (Id MPSNDArrayMultiaryBase) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayMultiaryBase"

class IsMPSKernel a => IsMPSNDArrayMultiaryBase a where
  toMPSNDArrayMultiaryBase :: a -> Id MPSNDArrayMultiaryBase

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayMultiaryBase) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayMultiaryBase) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayMultiaryBase) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGraph ----------

-- | MPSNNGraph
--
-- Optimized representation of a graph of MPSNNImageNodes and MPSNNFilterNodes
--
-- Once you have prepared a graph of MPSNNImageNodes and MPSNNFilterNodes              (and if needed MPSNNStateNodes), you may initialize a MPSNNGraph using               the MPSNNImageNode that you wish to appear as the result. The MPSNNGraph               object will introspect the graph representation and determine which nodes              are needed for inputs, and which nodes are produced as output state (if any).              Nodes which are not needed to calculate the result image node are ignored.              Some nodes may be internally concatenated with other nodes for better               performance.
--
-- Note: the MPSNNImageNode that you choose as the result node may be interior                    to a graph. This feature is provided as a means to examine intermediate                    computations in the full graph for debugging purposes.
--
-- During MPSNNGraph construction, the graph attached to the result node will               be parsed and reduced to an optimized representation. This representation may              be saved using the NSSecureCoding protocol for later recall.
--
-- When decoding a MPSNNGraph using a NSCoder, it will be created against               the system default MTLDevice. If you would like to set the MTLDevice,              your NSCoder should conform to the <MPSDeviceProvider> protocol.
--
-- You may find it helpful to set MPSKernelOptionsVerbose on the graph when              debugging. To turn this on during MPSKernel initialization (including              MPSNNGraph initialization) set the MPS_LOG_INFO environment variable.              There is a lot of information about what optimizations are done to your              graph, including some information on why certain optimizations were not              made.
-- 
-- Phantom type for @MPSNNGraph@.
data MPSNNGraph

instance IsObjCObject (Id MPSNNGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGraph"

class IsMPSKernel a => IsMPSNNGraph a where
  toMPSNNGraph :: a -> Id MPSNNGraph

instance IsMPSNNGraph (Id MPSNNGraph) where
  toMPSNNGraph = unsafeCastId

instance IsMPSKernel (Id MPSNNGraph) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNGraph) where
  toNSObject = unsafeCastId

-- ---------- MPSNNOptimizer ----------

-- | MPSNNOptimizer
--
-- The MPSNNOptimizer base class, use one of the child classes, not to be directly used. Optimizers are generally used to update trainable neural network parameters.              Users are usually expected to call these MPSKernels from the update methods on their Convolution or BatchNormalization data sources.
--
-- Before the gradient is used to update the original value, some preprocessing occurs on each gradient where it is scaled or clipped              If regularization is chosen the appropriate regularization loss gradient is added to the value gradient.
-- 
-- Phantom type for @MPSNNOptimizer@.
data MPSNNOptimizer

instance IsObjCObject (Id MPSNNOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNOptimizer"

class IsMPSKernel a => IsMPSNNOptimizer a where
  toMPSNNOptimizer :: a -> Id MPSNNOptimizer

instance IsMPSNNOptimizer (Id MPSNNOptimizer) where
  toMPSNNOptimizer = unsafeCastId

instance IsMPSKernel (Id MPSNNOptimizer) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNMatrixInferenceLayer ----------

-- | MPSRNNMatrixInferenceLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNMatrixInferenceLayer specifies a recurrent neural network layer for inference on MPSMatrices.              Currently two types of recurrent layers are supported: ones that operate with convolutions on              images: MPSRNNImageInferenceLayer and one that operates on matrices: MPSRNNMatrixInferenceLayer.              The former can be often used to implement the latter by using 1x1-matrices, but due to              image size restrictions and performance, it is advisable to use MPSRNNMatrixInferenceLayer for              linear recurrent layers.              A MPSRNNMatrixInferenceLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer, or an array of MPSRNNLayerDescriptors, which specifies a stack              of recurrent layers, that can operate in parallel a subset of the inputs in a sequence of inputs and              recurrent outputs. Note that currently stacks with bidirectionally traversing encode functions do not support starting              from a previous set of recurrent states, but this can be achieved quite easily by defining two separate              unidirectional stacks of layers, and running the same input sequence on them separately (one forwards and one backwards)              and ultimately combining the two result sequences as desired with auxiliary functions.              The input and output vectors in encode calls are stored as rows of the input and output matrices and              MPSRNNMatrixInferenceLayer supports matrices with decreasing number of rows: The row-indices identify the different              sequences that may be of different lengths - for example if we have three sequences:                  ( x1, x2, x3 ), ( y1, y2, y3, y4 ) and ( z1, z2 )              of vectors xi, yi and zi, then these can be inserted together as a batch to the sequence encoding kernel by              using the matrices:
--
-- ( y1 )        ( y2 )        ( y3 )        ( y4 )
-- m1 = ( x1 ),  m2 = ( x2 ),  m3 = ( x3 ),  m4 =
-- ( z1 )        ( z2 )
--
-- If a recurrent output state is requested then it will contain the state corresponding to last inputs to each              sequence and if all the intermediate states are requested (see storeAllIntermediateStates),              then the shorter sequences will be propagated by copying the state of the previous output if the              input vector is not present in the sequence - in the example above the output states would be:
--
-- ( s_y1 )        ( s_y2 )        ( s_y3 )        ( s_y4 )
-- s1 = ( s_x1 ),  s2 = ( s_x2 ),  s3 = ( s_x3 ),  s4 = ( s_x3 )
-- ( s_z1 )        ( s_z2 )        ( s_z2 )        ( s_z2 )
--
-- The mathematical operation described in the linear transformations of MPSRNNSingleGateDescriptor              MPSLSTMDescriptor and MPSGRUDescriptor are y^T = W x^T  <=> y = x W^T, where x is the matrix containing              the input vectors as rows, y is the matrix containing the output vectors as rows and W is the weight matrix.
-- 
-- Phantom type for @MPSRNNMatrixInferenceLayer@.
data MPSRNNMatrixInferenceLayer

instance IsObjCObject (Id MPSRNNMatrixInferenceLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNMatrixInferenceLayer"

class IsMPSKernel a => IsMPSRNNMatrixInferenceLayer a where
  toMPSRNNMatrixInferenceLayer :: a -> Id MPSRNNMatrixInferenceLayer

instance IsMPSRNNMatrixInferenceLayer (Id MPSRNNMatrixInferenceLayer) where
  toMPSRNNMatrixInferenceLayer = unsafeCastId

instance IsMPSKernel (Id MPSRNNMatrixInferenceLayer) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSRNNMatrixInferenceLayer) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNMatrixTrainingLayer ----------

-- | MPSRNNMatrixTrainingLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNMatrixTrainingLayer specifies a recurrent neural network layer for training on MPSMatrices.
--
-- A MPSRNNMatrixTrainingLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer.              The input and output vectors in encode calls are stored as rows of the input and output matrices and              MPSRNNMatrixTrainingLayer supports matrices with decreasing number of rows: The row-indices identify the different              sequences that may be of different lengths - for example if we have three sequences:                  ( x1, x2, x3 ), ( y1, y2, y3, y4 ) and ( z1, z2 )              of vectors xi, yi and zi, then these can be inserted together as a batch to the sequence encoding kernel by              using the matrices:
--
-- ( y1 )        ( y2 )        ( y3 )        ( y4 )
-- m1 = ( x1 ),  m2 = ( x2 ),  m3 = ( x3 ),  m4 =
-- ( z1 )        ( z2 )
--
-- The gradient computation pass is then achieved by passing the corresponding gradient sequence from the              previous layer ( dx1, dx2, dx3 ), ( dy1, dy2, dy3, dy4 ) and ( dz1, dz2 ) as matrices
--
-- ( dy1 )         ( dy2 )         ( dy3 )         ( dy4 )
-- dm1 = ( dx1 ),  dm2 = ( dx2 ),  dm3 = ( dx3 ),  dm4 =
-- ( dz1 )         ( dz2 )
--
-- The mathematical operation described in the linear transformations of MPSRNNSingleGateDescriptor              MPSLSTMDescriptor and MPSGRUDescriptor are y^T = W x^T  <=> y = x W^T, where x is the matrix containing              the input vectors as rows, y is the matrix containing the output vectors as rows and W is the weight matrix.
-- 
-- Phantom type for @MPSRNNMatrixTrainingLayer@.
data MPSRNNMatrixTrainingLayer

instance IsObjCObject (Id MPSRNNMatrixTrainingLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNMatrixTrainingLayer"

class IsMPSKernel a => IsMPSRNNMatrixTrainingLayer a where
  toMPSRNNMatrixTrainingLayer :: a -> Id MPSRNNMatrixTrainingLayer

instance IsMPSRNNMatrixTrainingLayer (Id MPSRNNMatrixTrainingLayer) where
  toMPSRNNMatrixTrainingLayer = unsafeCastId

instance IsMPSKernel (Id MPSRNNMatrixTrainingLayer) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSRNNMatrixTrainingLayer) where
  toNSObject = unsafeCastId

-- ---------- MPSRayIntersector ----------

-- | MPSRayIntersector
--
-- Performs intersection tests between rays and the geometry in an MPSAccelerationStructure
--
-- An MPSRayIntersector is used to schedule intersection tests between rays and geometry into an MTLCommandBuffer. First, create a raytracer with a Metal device. Then, configure the properties of the raytracer:
--
-- id <MTLDevice> device = MTLCreateSystemDefaultDevice();
-- id <MTLCommandQueue> commandQueue = [device newCommandQueue];
--
-- MPSRayIntersector *raytracer = [[MPSRayIntersector alloc] initWithDevice:device];
--
-- // Configure raytracer properties
--
-- Before scheduling intersection tests, an MPSAccelerationStructure must be created. The acceleration structure is built over geometry and is used to accelerate intersection testing. For example, to create a triangle acceleration structure, allocate an MPSTriangleAccelerationStructure object. Then, configure the properties of the acceleration structure. For example, triangle acceleration structures require a vertex buffer and a triangle count:
--
-- MPSTriangleAccelerationStructure *accelerationStructure =
-- [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- accelerationStructure.vertexBuffer = vertexBuffer;
-- accelerationStructure.triangleCount = triangleCount;
--
-- Acceleration structures must be built at least once before they are used for intersection testing, and must be rebuilt when the geometry changes. Rebuilding an acceleration structure is a time consuming operation, so an asynchronous version of this method is also available.
--
-- [accelerationStructure rebuild];
--
-- The raytracer is then used to schedule intersection tests into an MTLCommandBuffer. Rays are provided in batches through a Metal buffer, and intersection results are returned through another Metal buffer in the same order, one intersection per ray.
--
-- There are several choices of ray data type controlled by the rayDataType property. The default ray data type is MPSRayOriginDirection, which includes just the ray origin direction. The other data types add support for minimum and maximum intersection distances and ray masks. These data types are available in the Metal Shading Language by including the MetalPerformanceShaders/MetalPerformanceShaders.h header. Additional application specific per-ray data can also be appended to the end of the ray data type using the rayStride property. This data will be ignored by the intersector.
--
-- If the rays were generated on the CPU:
--
-- typedef MPSRayOriginDirection Ray;
--
-- // Create a buffer to hold the rays
-- id <MTLBuffer> rayBuffer = [device newBufferWithLength:sizeof(Ray) * rayCount options:0];
--
-- // Copy the rays into the ray buffer
-- memcpy(rayBuffer.contents, rays, sizeof(Ray) * rayCount);
--
-- // Create a buffer to hold the intersections
-- id <MTLBuffer> intersectionBuffer = [device newBufferWithLength:sizeof(Intersection) * rayCount
-- options:0];
--
-- It can be useful to prevent certain rays from participating in intersection testing. For example: rays which have bounced out of the scene in previous intersection tests. It may be more efficient to do this by compacting the ray buffer so that threads with invalid rays are not left idle during intersection testing. However, it can be more convenient to disable the ray in place. This can be done by setting most fields to invalid values. For example, setting the maximum distance to a negative value, setting the mask to zero, setting the direction to the zero vector, etc.
--
-- Finally, the intersection testing is encoded into an MTLCommandBuffer. There are two intersection types. The "nearest" intersection type returns the closest intersection along each ray. The "any" intersection type returns immediately when the first intersection is found. The "any" intersection type is useful for determining whether a point is visible from another point for, e.g., shadow rays or ambient occlusion rays and is typically much faster than the "nearest" intersection type.
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- [commandBuffer commit];
--
-- The intersection results are not available until the command buffer has finished executing on the GPU. It is not safe for the CPU to write or read the contents of the ray buffer, intersection buffer, vertex buffer, etc. until the command buffer has finished executing. Use the waitUntilCompleted or addCompletedHandler methods of the MTLCommandBuffer to block the CPU until the GPU has finished executing. Then retrieve the intersection results from the intersection buffer:
--
-- typedef MPSIntersectionDistancePrimitiveIndexCoordinates Intersection;
--
-- [commandBuffer waitUntilCompleted];
--
-- Intersection *intersections = (Intersection *)intersectionBuffer.contents;
--
-- There are also several choices of intersection data type controlled by the intersectionDataType property. The default intersection data type is MPSIntersectionDistancePrimitiveIndexCoordinates, which includes the intersection distance, primitive index, and barycentric coordinates. The other data types remove the primitive index or barycentric coordinates, which can be used to reduce the memory and memory bandwidth usage of the intersection buffer. These data types are available in the Metal Shading Language by including the MetalPerformanceShaders/MetalPerformanceShaders.h header.
--
-- The intersection distance field is positive when an intersection has been found and negative when there is no intersection. When using the "nearest" intersection type, the intersection point is the ray origin plus the ray direction multiplied by the intersection distance. The other fields are not valid if there is no intersection. Only the intersection distance field is valid for the "any" intersection type, and the distance is either a negative or positive value to indicate an intersection or miss. It does not necessarily contain the actual intersection distance when using the "any" intersection type.
--
-- Asynchronous Raytracing: Copying rays and intersections to and from the CPU is expensive. Furthermore, generating rays and consuming intersections on the CPU causes the CPU and GPU to block each other. If the CPU must generate rays and consume intersections, it is better to add an asynchronous completion handler to the MTLCommandBuffer. The CPU can then proceed to do other useful work and will be notified when the GPU has finished executing. Use double or triple buffered ray and intersection buffers to avoid race conditions such as the CPU overwriting data the GPU may be reading. Then the CPU can safely write to one range of the buffer while the GPU reads from another range of the buffer. Once the GPU is done  executing, the CPU and GPU can advance to the next range of the buffer. This method can be implemented using a completion handler and a semaphore:
--
-- #define MAX_ASYNC_OPERATIONS 3
--
-- // Initialization:
--
-- // Create a semaphore with the maximum number of asynchronous operations in flight
-- dispatch_semaphore_t asyncOperationSemaphore = dispatch_semaphore_create(MAX_ASYNC_OPERATIONS);
--
-- // Create a ray and intersection buffer large enough for the maximum number of operations
-- id <MTLBuffer> rayBuffer =
-- [device newBufferWithLength:sizeof(Ray) * rayCount * MAX_ASYNC_OPERATIONS
-- options:0];
--
-- id <MTLBuffer> intersectionBuffer =
-- [device newBufferWithLength:sizeof(Intersection) * rayCount * MAX_ASYNC_OPERATIONS
-- options:0];
--
-- NSUInteger asyncOperationIndex = 0;
--
-- // Encode intersection testing:
--
-- // Wait until there is a free buffer range
-- dispatch_semaphore_wait(asyncOperationSemaphore, DISPATCH_TIME_FOREVER);
--
-- // Copy rays into ray buffer
-- NSUInteger rayBufferOffset = sizeof(Ray) * rayCount * asyncOperationIndex;
-- NSUInteger intersectionBufferOffset = sizeof(Intersection) * rayCount * asyncOperationIndex;
--
-- memcpy((uint8_t *)rayBuffer.contents + rayBufferOffset, rays, sizeof(Ray) * rayCount);
--
-- // Advance the async operation index
-- asyncOperationIndex = (asyncOperationIndex + 1) % MAX_ASYNC_OPERATIONS;
--
-- // Create a command buffer
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- // Encode actual intersection work
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:rayBufferOffset
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:intersectionBufferOffset
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- // Register a completion handler to run when the GPU finishes executing
-- [commandBuffer addCompletedHandler:^(id <MTLCommandBuffer> commandBuffer) {
-- Intersection *intersections = (Intersection *)((uint8_t *)intersectionBuffer.contents +
-- intersectionBufferOffset);
--
-- // Process intersections
--
-- // Signal that the ray and intersection buffer ranges are now available for reuse
-- dispatch_semaphore_signal(asyncOperationSemaphore);
-- }];
--
-- // Commit the command buffer to allow the GPU to start executing
-- [commandBuffer commit];
--
-- GPU Driven Raytracing: Pipelining CPU and GPU work with asynchronous raytracing is better than allowing the CPU and GPU block each other, but it is even better to produce rays and consume intersections entirely on the GPU. This avoids the need to copy rays and intersections to and from the GPU and avoids any kind of CPU/GPU synchronization. To do this, encode compute kernels before and after intersection testing. By processing rays in parallel, the compute kernels may also be able to generate and consume rays faster than the CPU. The ray generation kernel typically produces rays according to some camera model, and the intersection consumption kernel typically updates the output buffer or texture according to some shading model.
--
-- Since the rays and intersections will never leave the GPU, store them in private Metal buffers that are allocated in GPU memory rather than system memory. Because the ray generation, intersection testing, and intersection consumption kernels are pipelined on the GPU, there is no need to double or triple buffer the ray or intersection buffers, which saves memory.
--
-- id <MTLBuffer> rayBuffer =
-- [device newBufferWithLength:sizeof(Ray) * rayCount
-- options:MTLResourceStorageModePrivate];
-- id <MTLBuffer> intersectionBuffer =
-- [device newBufferWithLength:sizeof(Intersection) * rayCount
-- options:MTLResourceStorageModePrivate];
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- // Generate rays
-- id <MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:rayBuffer offset:0 atIndex:0];
-- [encoder setBytes:&uniformData length:sizeof(uniformData) atIndex:1];
--
-- [encoder setComputePipelineState:cameraPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(rayCount, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- // Perform shading at intersections and update framebuffer texture
-- encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:rayBuffer offset:0 atIndex:0];
-- [encoder setBuffer:intersectionBuffer offset:0 atIndex:1];
-- [encoder setBytes:&uniformData length:sizeof(uniformData) atIndex:2];
--
-- [encoder setTexture:framebufferTexture atIndex:0];
--
-- [encoder setComputePipelineState:shadingPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(rayCount, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
--
-- [commandBuffer commit];
--
-- Note that the intersection consumption kernel can in turn produce new rays that can be passed back to the MPSRayIntersector. This technique can be used to implement iterative techniques such as progressive path tracing without leaving the GPU. For example, the shading kernel in the example above could produce both a secondary ray that will be passed back to the raytracer in the next iteration as well as a shadow ray that will be used to sample the direct lighting. A final kernel can consume the shadow ray intersections to accumulate lighting contributions into the framebuffer.
--
-- There is an alternative version of the intersection test encoding method that does not accept a literal ray count. The ray count is instead fetched indirectly by the GPU. For example, this can be combined with a parallel reduction on the GPU to compact the ray buffer after each iteration as rays bounce out of the scene or are absorbed. Alternatively, setting the maximum distance of a ray to a negative number indicates that the ray has become inactive and causes the raytracer to ignore the ray.
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCountBuffer:rayCountBuffer
-- rayCountBufferOffset:0
-- accelerationStructure:accelerationStructure];
--
-- Multi-GPU Raytracing: to implement multi-GPU raytracing, create the MPSRayIntersector and MPSAccelerationStructure objects first with one Metal device and copy them to the other Metal device(s). The raytracing process can then proceed independently on each GPU. For example, divide the output image into tiles or slices that are rendered independently. Then composite finished tiles or slices back together on one GPU and present the output image to the screen. The workload should be distributed across GPUs according to their performance to avoid a more powerful GPU idly waiting for a less powerful GPU to finish.
--
-- Acceleration Structure Serialization: MPSAccelerationStructure objects can be serialized and deserialized using the NSSecureCoding protocol. This can be used to build acceleration structures offline and reload them at runtime rather than building them from scratch.
--
-- Performance Guidelines:
--
-- - For vertex buffers, ray buffers, intersection buffers, etc., use private or managed       buffers rather than shared buffers when possible on discrete memory GPU architectures as       they are much faster than fetching data over the PCIe bus. If the CPU only writes once       to a ray buffer once and reads once from the intersection buffer, then a shared buffer may       be acceptable and avoids extra copies to and from the GPU. However, it is generally       preferable to generate and consume rays and intersections on the GPU instead, in which       case a private buffer should be used. Vertex data is typically static and reused many       times so it should be stored in private or managed buffers.
--
-- - If the CPU must generate and consume rays and intersections, use double or triple       buffering as described above. This avoids the CPU and GPU mutually blocking each other.
--
-- - In general, disable any unused features such as ray masks, backface culling,       etc. Enabling extra features increases the number of instructions and register usage of       the ray intersection kernel(s), reducing intersection performance. For example, it may be       more efficient to compute barycentric coordinates in your intersection consumption       kernel rather getting them from the raytracer. Use of an index buffer may also reduce       performance, so consider disabling the index buffer if there is enough memory available.
--
-- - Try to submit rays in large batches. This amortizes the costs involved in dispatching       work to the GPU and also allows the GPU to perform more effective latency hiding.       Use the recommendedMinimumRayBatchSizeForRayCount method to get an estimate of the       minimum recommended ray batch size. For this reason, small images or sample counts       may not perform as well as large images or sample counts. Note, however, that submitting       rays in very large batches can reduce the responsiveness of the system because the GPU       will be busy for long periods. Experiment to find a balance between raytracing throughput       and system responsiveness.
--
-- - When possible, organize rays within a batch for spatial locality. Rays that originate       at nearby points or are oriented in similar directions tend to access the same       locations in memory and can therefore make more effective use of the GPU's caches.       For example, the camera rays associated with nearby pixels in the output image will likely       originate at the same point and travel in very similar directions. Therefore, divide the       output image into small tiles (e.g., 8x8). Rather than laying out all of the rays in the       ray buffer in scanline order, first lay out the ray in scanline order within each tile,       then lay out the tiles in scanline order or according to some space filling curve.
--
-- - If CPU encode time is an issue, disable Metal API validation and enable       MPSKernelOptionsSkipAPIValidation.
--
-- - Choose the minimal ray and intersection data types for your use case. Loading and storing       extra values such as ray masks or primitive indices can reduce raytracing performance, so       use a simpler data type if they are not needed. For example, camera rays typically have no       need for a maximum distance field, while shadow rays do.
--
-- - Use MPSIntersectionTestTypeAny when possible: this is typically much faster than       MPSIntersectionTestTypeNearest and can be used when you only need to check for       binary visibility between two points such as shadow and ambient occlusion rays. Combine       this with MPSRayDataTypeDistance to minimize memory bandwidth usage.
--
-- - Try to keep the geometry, textures, ray buffers, etc. within the Metal device's       recommended working set size. Paging data into GPU memory can significantly reduce       raytracing performance.
--
-- - Changes to MPSRayIntersector properties can trigger internal pipeline compilations when       intersection tests are next encoded. If you need to avoid hitches due to pipeline       compilation, encode a small ray intersection with each raytracer configuration you will       use at encode-time. This creates and caches the corresponding pipelines.
--
-- - Disable rays which should not participate in intersection testing. This can be done either       by compacting the ray buffer such that it only contains valid rays, or by setting fields       of the ray struct to invalid values. For example, setting the maximum distance to a       negative value, setting the mask to zero, setting the direction to the zero vector, etc.       In particular, rays should NOT be disabled using schemes such as moving their origin       outside the scene. These rays will still partially traverse the acceleration structure,       potentially evicting data from the cache which could have been used by valid rays. Note       that it is preferable to provide only valid rays so that threads are not left idle if       their rays are found to be invalid, but it can be convenient to disable rays in place in       the ray buffer.
--
-- See MPSAccelerationStructure and MPSInstanceAccelerationStructure for more performance guidelines.
--
-- Thread Safety: MPSRayIntersectors are generally not thread safe: changing properties and encoding intersection tests from multiple threads result in undefined behavior. Instead, multiple threads should copy or create their own MPSRayIntersectors.
-- 
-- Phantom type for @MPSRayIntersector@.
data MPSRayIntersector

instance IsObjCObject (Id MPSRayIntersector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRayIntersector"

class IsMPSKernel a => IsMPSRayIntersector a where
  toMPSRayIntersector :: a -> Id MPSRayIntersector

instance IsMPSRayIntersector (Id MPSRayIntersector) where
  toMPSRayIntersector = unsafeCastId

instance IsMPSKernel (Id MPSRayIntersector) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSRayIntersector) where
  toNSObject = unsafeCastId

-- ---------- MPSSVGF ----------

-- | Reduces noise in images rendered with Monte Carlo ray tracing methods
--
-- This filter uses temporal reprojection to accumulate samples over time, followed by an edge-avoiding blur to smooth out the noise. It uses depth and surface normal textures to detect edges in the image(s) to be denoised. The filter also computes an estimate of the luminance variance of the accumulated samples for each pixel to reject neighboring pixels whose luminance is too dissimilar while blurring.
--
-- This filter requires noise-free depth and normal textures, so it is not compatible with stochastic visibility effects such as depth of field, motion blur, or pixel subsampling. These effects need to be applied as a post-process instead. Furthermore, because the depth and normal textures can only represent directly visible geometry, the filter may over-blur reflections. The use of temporal reprojection may introduce artifacts such as ghosting or streaking, as well as a temporal lag for changes in luminance such as moving shadows. However, the filter is relatively fast as it is intended for realtime use. Slower but higher quality filters are available in the literature.
--
-- This filter can process up to two images simultaneously assuming they share the same depth and normal textures. This is typically faster than processing the two images independently because memory bandwidth spent fetching depth and normal values and ALU time spent computing various weighting functions can be shared by both images. This is useful if e.g. you want to denoise direct and indirect lighting terms separately to avoid mixing the two terms. The filter is also optimized for processing single-channel images for effects such as shadows and ambient occlusion. Denoising these images can be much faster than denoising a full RGB image, so it may be useful to separate out these terms and denoise them specifically.
--
-- This filter operates in three stages: temporal reprojection, variance estimation, and finally a series of edge-avoiding bilateral blurs. The temporal reprojection stage accepts the image to be denoised for the current frame and the denoised image from the previous frame, the depth and normal textures from the current and previous frame and, finally, a motion vector texture. It uses the motion vector texture to look up the accumulated samples from the previous frame. It then compares the depth and normals to determine if those samples are consistent with the current frame. If so, the previous frame is blended with the current frame. This stage also accumulates the first and second moments of the sample luminance which is used to compute the luminance variance in the next stage.
--
-- The variance estimation stage computes an estimate of the variance of the luminance of the accumulated samples for each pixel. This stage may fall back to a spatial estimate if not enough samples have been accumulated. The luminance variance is used in the final stage to reject outlying neighboring pixels while blurring to avoid blurring across luminance discontinuities such as shadow boundaries.
--
-- The final stage performs consecutive edge-avoiding bilateral blurs to smooth out noise in the image. The blurs are dilated with increasing power of two step distances starting from 1, which cheaply approximates a very large radius bilateral blur. Each iteration blurs both the input image and the variance image as variance is reduced after each iteration. It is recommended that the output of the first iteration be used as the input to the next frame's reprojection stage to further reduce noise.
--
-- Tips:
--
-- - It may be helpful to further divide out texture details such as surface albedo before   denoising to avoid blurring texture detail and to preserve any careful texture filtering that   may have been performed. The albedo can be reapplied after denoising. - High frequency geometry and normal maps may cause excessive disocclusions during reprojection   manifesting as noise. - Jittering sample positions from frame to frame for temporal antialiasing may also cause   disocclusions. However, this can be partially hidden by the temporal antialiasing algorithm   itself. - This kernel, like many convolutions, requires quite a bit of bandwidth. Use the texture pixel   formats with the smallest number of bits-per-pixel and the lowest resolution possible for the   required quality level. Lower resolution images can be combined with a bilateral upsampling   filter, especially if the image being denoised is mostly low frequency lighting or ambient   occlusion. - The increasing dilation during the bilateral blurring stage can introduce ringing artifacts   around geometric discontinuities. These can be partially hidden at the cost of potentially   increased noise by reducing the bilateral blur's sigma value slightly after each iteration. - Use lower precision pixel formats if possible to reduce memory bandwidth.
--
-- Refer to "Spatiotemporal Variance-Guided Filtering: Real-Time Reconstruction for Path-Traced Global Illumination" for more information.
-- 
-- Phantom type for @MPSSVGF@.
data MPSSVGF

instance IsObjCObject (Id MPSSVGF) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSSVGF"

class IsMPSKernel a => IsMPSSVGF a where
  toMPSSVGF :: a -> Id MPSSVGF

instance IsMPSSVGF (Id MPSSVGF) where
  toMPSSVGF = unsafeCastId

instance IsMPSKernel (Id MPSSVGF) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSSVGF) where
  toNSObject = unsafeCastId

-- ---------- MPSTemporalAA ----------

-- | Reduces aliasing in an image by accumulating samples over multiple frames
--
-- The color for the previous frame will be sampled using the provided motion vector texture and blended with the current frame according to the blendFactor property. The colors from the previous frame will be clamped to the color-space bounding box formed by the center pixel's neighbors to avoid reprojection artifacts, and the motion vector texture will be dilated to avoid aliased silhouette edges under motion.
--
-- For the best result, the sample positions produced by the renderer should be jittered every frame, ideally using a low discrepency sequence. This will ensure that different sample positions along edges will be visited over time even if the camera is not moving. This will also reduce aliasing due to textures and high-frequency shading.
--
-- For reference, see "High-Quality Temporal Supersampling" by Karis.
-- 
-- Phantom type for @MPSTemporalAA@.
data MPSTemporalAA

instance IsObjCObject (Id MPSTemporalAA) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTemporalAA"

class IsMPSKernel a => IsMPSTemporalAA a where
  toMPSTemporalAA :: a -> Id MPSTemporalAA

instance IsMPSTemporalAA (Id MPSTemporalAA) where
  toMPSTemporalAA = unsafeCastId

instance IsMPSKernel (Id MPSTemporalAA) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSTemporalAA) where
  toNSObject = unsafeCastId

-- ---------- MPSUnaryImageKernel ----------

-- | MPSUnaryImageKernel
--
-- This depends on Metal.framework
--
-- A MPSUnaryImageKernel consumes one MTLTexture and produces one MTLTexture.
-- 
-- Phantom type for @MPSUnaryImageKernel@.
data MPSUnaryImageKernel

instance IsObjCObject (Id MPSUnaryImageKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSUnaryImageKernel"

class IsMPSKernel a => IsMPSUnaryImageKernel a where
  toMPSUnaryImageKernel :: a -> Id MPSUnaryImageKernel

instance IsMPSUnaryImageKernel (Id MPSUnaryImageKernel) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsMPSKernel (Id MPSUnaryImageKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSUnaryImageKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSTemporaryMatrix ----------

-- | A MPSMatrix allocated on GPU private memory.
--
-- It may alias one or more other MPSTemporaryMatrices. Undesired data destruction              due to aliasing is avoided using the readCount property.
-- 
-- Phantom type for @MPSTemporaryMatrix@.
data MPSTemporaryMatrix

instance IsObjCObject (Id MPSTemporaryMatrix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTemporaryMatrix"

class IsMPSMatrix a => IsMPSTemporaryMatrix a where
  toMPSTemporaryMatrix :: a -> Id MPSTemporaryMatrix

instance IsMPSTemporaryMatrix (Id MPSTemporaryMatrix) where
  toMPSTemporaryMatrix = unsafeCastId

instance IsMPSMatrix (Id MPSTemporaryMatrix) where
  toMPSMatrix = unsafeCastId

instance IsNSObject (Id MPSTemporaryMatrix) where
  toNSObject = unsafeCastId

-- ---------- MPSTemporaryNDArray ----------

-- | MPSTemporaryNDArray
--
-- A MPSNDArray that uses command buffer specific memory to store the array data
--
-- Temporary memory is command buffer specific memory, and is useful for MPSNDArray allocations              with limited lifetime within a single command buffer. Typically, most MPSNDArrays that              are not read or written to by the CPU or needed in other command buffers should be              MPSTemporaryNDArray. This will greatly reduce time spent allocating new memory, reduce memory usage              and help improve memory locality.
-- 
-- Phantom type for @MPSTemporaryNDArray@.
data MPSTemporaryNDArray

instance IsObjCObject (Id MPSTemporaryNDArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTemporaryNDArray"

class IsMPSNDArray a => IsMPSTemporaryNDArray a where
  toMPSTemporaryNDArray :: a -> Id MPSTemporaryNDArray

instance IsMPSTemporaryNDArray (Id MPSTemporaryNDArray) where
  toMPSTemporaryNDArray = unsafeCastId

instance IsMPSNDArray (Id MPSTemporaryNDArray) where
  toMPSNDArray = unsafeCastId

instance IsNSObject (Id MPSTemporaryNDArray) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayAffineQuantizationDescriptor ----------

-- | MPSNDArrayAffineQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Describes an affine quantization scheme
-- 
-- Phantom type for @MPSNDArrayAffineQuantizationDescriptor@.
data MPSNDArrayAffineQuantizationDescriptor

instance IsObjCObject (Id MPSNDArrayAffineQuantizationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayAffineQuantizationDescriptor"

class IsMPSNDArrayQuantizationDescriptor a => IsMPSNDArrayAffineQuantizationDescriptor a where
  toMPSNDArrayAffineQuantizationDescriptor :: a -> Id MPSNDArrayAffineQuantizationDescriptor

instance IsMPSNDArrayAffineQuantizationDescriptor (Id MPSNDArrayAffineQuantizationDescriptor) where
  toMPSNDArrayAffineQuantizationDescriptor = unsafeCastId

instance IsMPSNDArrayQuantizationDescriptor (Id MPSNDArrayAffineQuantizationDescriptor) where
  toMPSNDArrayQuantizationDescriptor = unsafeCastId

instance IsNSObject (Id MPSNDArrayAffineQuantizationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayLUTQuantizationDescriptor ----------

-- | MPSNDArrayLUTQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Describes a lookup-table based quantization scheme
-- 
-- Phantom type for @MPSNDArrayLUTQuantizationDescriptor@.
data MPSNDArrayLUTQuantizationDescriptor

instance IsObjCObject (Id MPSNDArrayLUTQuantizationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayLUTQuantizationDescriptor"

class IsMPSNDArrayQuantizationDescriptor a => IsMPSNDArrayLUTQuantizationDescriptor a where
  toMPSNDArrayLUTQuantizationDescriptor :: a -> Id MPSNDArrayLUTQuantizationDescriptor

instance IsMPSNDArrayLUTQuantizationDescriptor (Id MPSNDArrayLUTQuantizationDescriptor) where
  toMPSNDArrayLUTQuantizationDescriptor = unsafeCastId

instance IsMPSNDArrayQuantizationDescriptor (Id MPSNDArrayLUTQuantizationDescriptor) where
  toMPSNDArrayQuantizationDescriptor = unsafeCastId

instance IsNSObject (Id MPSNDArrayLUTQuantizationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationNode ----------

-- | MPSCNNBatchNormalizationNode
--
-- A node representing batch normalization for inference or training
--
-- Batch normalization operates differently for inference and training.              For inference, the normalization is done according to a static statistical              representation of data saved during training. For training, this representation              is ever evolving.  In the low level MPS batch normalization interface,              during training, the batch normalization is broken up into two steps:              calculation of the statistical representation of input data, followed              by normalization once the statistics are known for the entire batch.              These are MPSCNNBatchNormalizationStatistics and MPSCNNBatchNormalization,              respectively.
--
-- When this node appears in a graph and is not required to produce a              MPSCNNBatchNormalizationState -- that is, MPSCNNBatchNormalizationNode.resultState              is not used within the graph -- then it operates in inference mode              and new batch-only statistics are not calculated. When this state node              is consumed, then the node is assumed to be in training mode and              new statistics will be calculated and written to the MPSCNNBatchNormalizationState              and passed along to the MPSCNNBatchNormalizationGradient and              MPSCNNBatchNormalizationStatisticsGradient as necessary. This should              allow you to construct an identical sequence of nodes for inference              and training and expect the right thing to happen.
-- 
-- Phantom type for @MPSCNNBatchNormalizationNode@.
data MPSCNNBatchNormalizationNode

instance IsObjCObject (Id MPSCNNBatchNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationNode"

class IsMPSNNFilterNode a => IsMPSCNNBatchNormalizationNode a where
  toMPSCNNBatchNormalizationNode :: a -> Id MPSCNNBatchNormalizationNode

instance IsMPSCNNBatchNormalizationNode (Id MPSCNNBatchNormalizationNode) where
  toMPSCNNBatchNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNBatchNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionNode ----------

-- | A MPSNNFilterNode representing a MPSCNNConvolution kernel
-- 
-- Phantom type for @MPSCNNConvolutionNode@.
data MPSCNNConvolutionNode

instance IsObjCObject (Id MPSCNNConvolutionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionNode"

class IsMPSNNFilterNode a => IsMPSCNNConvolutionNode a where
  toMPSCNNConvolutionNode :: a -> Id MPSCNNConvolutionNode

instance IsMPSCNNConvolutionNode (Id MPSCNNConvolutionNode) where
  toMPSCNNConvolutionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNConvolutionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDilatedPoolingMaxNode ----------

-- | A node for a MPSCNNDilatedPooling kernel
--
-- This class corresponds to the MPSCNNDilatedPooling class.
-- 
-- Phantom type for @MPSCNNDilatedPoolingMaxNode@.
data MPSCNNDilatedPoolingMaxNode

instance IsObjCObject (Id MPSCNNDilatedPoolingMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDilatedPoolingMaxNode"

class IsMPSNNFilterNode a => IsMPSCNNDilatedPoolingMaxNode a where
  toMPSCNNDilatedPoolingMaxNode :: a -> Id MPSCNNDilatedPoolingMaxNode

instance IsMPSCNNDilatedPoolingMaxNode (Id MPSCNNDilatedPoolingMaxNode) where
  toMPSCNNDilatedPoolingMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNDilatedPoolingMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNDilatedPoolingMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDropoutNode ----------

-- | Phantom type for @MPSCNNDropoutNode@.
data MPSCNNDropoutNode

instance IsObjCObject (Id MPSCNNDropoutNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDropoutNode"

class IsMPSNNFilterNode a => IsMPSCNNDropoutNode a where
  toMPSCNNDropoutNode :: a -> Id MPSCNNDropoutNode

instance IsMPSCNNDropoutNode (Id MPSCNNDropoutNode) where
  toMPSCNNDropoutNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNDropoutNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNDropoutNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGroupNormalizationNode ----------

-- | Phantom type for @MPSCNNGroupNormalizationNode@.
data MPSCNNGroupNormalizationNode

instance IsObjCObject (Id MPSCNNGroupNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGroupNormalizationNode"

class IsMPSNNFilterNode a => IsMPSCNNGroupNormalizationNode a where
  toMPSCNNGroupNormalizationNode :: a -> Id MPSCNNGroupNormalizationNode

instance IsMPSCNNGroupNormalizationNode (Id MPSCNNGroupNormalizationNode) where
  toMPSCNNGroupNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNGroupNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNGroupNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNInstanceNormalizationNode ----------

-- | Phantom type for @MPSCNNInstanceNormalizationNode@.
data MPSCNNInstanceNormalizationNode

instance IsObjCObject (Id MPSCNNInstanceNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNInstanceNormalizationNode"

class IsMPSNNFilterNode a => IsMPSCNNInstanceNormalizationNode a where
  toMPSCNNInstanceNormalizationNode :: a -> Id MPSCNNInstanceNormalizationNode

instance IsMPSCNNInstanceNormalizationNode (Id MPSCNNInstanceNormalizationNode) where
  toMPSCNNInstanceNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNInstanceNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNInstanceNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLogSoftMaxNode ----------

-- | Node representing a MPSCNNLogSoftMax kernel
-- 
-- Phantom type for @MPSCNNLogSoftMaxNode@.
data MPSCNNLogSoftMaxNode

instance IsObjCObject (Id MPSCNNLogSoftMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLogSoftMaxNode"

class IsMPSNNFilterNode a => IsMPSCNNLogSoftMaxNode a where
  toMPSCNNLogSoftMaxNode :: a -> Id MPSCNNLogSoftMaxNode

instance IsMPSCNNLogSoftMaxNode (Id MPSCNNLogSoftMaxNode) where
  toMPSCNNLogSoftMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNLogSoftMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNLogSoftMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLossNode ----------

-- | MPSCNNLossNode
--
-- This node calculates loss information during training               typically immediately after the inference portion               of network evaluation is performed. The result image               of the loss operations is typically the first gradient               image to be comsumed by the gradient passes that work               their way back up the graph. In addition, the node will               update the loss image in the MPSNNLabels with the               desired estimate of correctness.
-- 
-- Phantom type for @MPSCNNLossNode@.
data MPSCNNLossNode

instance IsObjCObject (Id MPSCNNLossNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLossNode"

class IsMPSNNFilterNode a => IsMPSCNNLossNode a where
  toMPSCNNLossNode :: a -> Id MPSCNNLossNode

instance IsMPSCNNLossNode (Id MPSCNNLossNode) where
  toMPSCNNLossNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNLossNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNLossNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronNode ----------

-- | virtual base class for MPSCNNNeuron nodes
--
-- This is a virtual base class only. Please create a              subclass using +newNeuronNodeWithSouce:descriptor or              by making one of the subclasses directly. Better yet, skip              the node entirely and specify the neuron function directly in              your MPSCNNConvolutionDataSource.descriptor.neuronDescriptor.
--
-- MPSCNNNeuronNodes are provided as a representational convenience.              However, you are usually better off incorporating your neuron              into the MPSCNNConvolutionDataSource when possible. The MPSNNGraph              will attempt to optimize away the neuron pass by fusing it with a               preceeding convolution, but it might be prevented from doing so               if the neuron pass has a custom padding method or more than one               node reads from the convolution result. The graph -debugDescription              should reveal what happened.
-- 
-- Phantom type for @MPSCNNNeuronNode@.
data MPSCNNNeuronNode

instance IsObjCObject (Id MPSCNNNeuronNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronNode"

class IsMPSNNFilterNode a => IsMPSCNNNeuronNode a where
  toMPSCNNNeuronNode :: a -> Id MPSCNNNeuronNode

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNormalizationNode ----------

-- | virtual base class for CNN normalization nodes
-- 
-- Phantom type for @MPSCNNNormalizationNode@.
data MPSCNNNormalizationNode

instance IsObjCObject (Id MPSCNNNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNormalizationNode"

class IsMPSNNFilterNode a => IsMPSCNNNormalizationNode a where
  toMPSCNNNormalizationNode :: a -> Id MPSCNNNormalizationNode

instance IsMPSCNNNormalizationNode (Id MPSCNNNormalizationNode) where
  toMPSCNNNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingNode ----------

-- | A node for a MPSCNNPooling kernel
--
-- This is an abstract base class that does not correspond with any              particular MPSCNNKernel. Please make one of the MPSCNNPooling              subclasses instead.
-- 
-- Phantom type for @MPSCNNPoolingNode@.
data MPSCNNPoolingNode

instance IsObjCObject (Id MPSCNNPoolingNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingNode"

class IsMPSNNFilterNode a => IsMPSCNNPoolingNode a where
  toMPSCNNPoolingNode :: a -> Id MPSCNNPoolingNode

instance IsMPSCNNPoolingNode (Id MPSCNNPoolingNode) where
  toMPSCNNPoolingNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSoftMaxNode ----------

-- | Node representing a MPSCNNSoftMax kernel
-- 
-- Phantom type for @MPSCNNSoftMaxNode@.
data MPSCNNSoftMaxNode

instance IsObjCObject (Id MPSCNNSoftMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSoftMaxNode"

class IsMPSNNFilterNode a => IsMPSCNNSoftMaxNode a where
  toMPSCNNSoftMaxNode :: a -> Id MPSCNNSoftMaxNode

instance IsMPSCNNSoftMaxNode (Id MPSCNNSoftMaxNode) where
  toMPSCNNSoftMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNSoftMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNSoftMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingBilinearNode ----------

-- | Node representing a MPSCNNUpsamplingBilinear kernel
-- 
-- Phantom type for @MPSCNNUpsamplingBilinearNode@.
data MPSCNNUpsamplingBilinearNode

instance IsObjCObject (Id MPSCNNUpsamplingBilinearNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingBilinearNode"

class IsMPSNNFilterNode a => IsMPSCNNUpsamplingBilinearNode a where
  toMPSCNNUpsamplingBilinearNode :: a -> Id MPSCNNUpsamplingBilinearNode

instance IsMPSCNNUpsamplingBilinearNode (Id MPSCNNUpsamplingBilinearNode) where
  toMPSCNNUpsamplingBilinearNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNUpsamplingBilinearNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingBilinearNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingNearestNode ----------

-- | Node representing a MPSCNNUpsamplingNearest kernel
-- 
-- Phantom type for @MPSCNNUpsamplingNearestNode@.
data MPSCNNUpsamplingNearestNode

instance IsObjCObject (Id MPSCNNUpsamplingNearestNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingNearestNode"

class IsMPSNNFilterNode a => IsMPSCNNUpsamplingNearestNode a where
  toMPSCNNUpsamplingNearestNode :: a -> Id MPSCNNUpsamplingNearestNode

instance IsMPSCNNUpsamplingNearestNode (Id MPSCNNUpsamplingNearestNode) where
  toMPSCNNUpsamplingNearestNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNUpsamplingNearestNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingNearestNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNYOLOLossNode ----------

-- | MPSCNNYOLOLossNode
--
-- This node calculates loss information during training               typically immediately after the inference portion               of network evaluation is performed. The result image               of the loss operations is typically the first gradient               image to be comsumed by the gradient passes that work               their way back up the graph. In addition, the node will               update the loss image in the MPSNNLabels with the               desired estimate of correctness.
-- 
-- Phantom type for @MPSCNNYOLOLossNode@.
data MPSCNNYOLOLossNode

instance IsObjCObject (Id MPSCNNYOLOLossNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNYOLOLossNode"

class IsMPSNNFilterNode a => IsMPSCNNYOLOLossNode a where
  toMPSCNNYOLOLossNode :: a -> Id MPSCNNYOLOLossNode

instance IsMPSCNNYOLOLossNode (Id MPSCNNYOLOLossNode) where
  toMPSCNNYOLOLossNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNYOLOLossNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNYOLOLossNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNBinaryArithmeticNode ----------

-- | virtual base class for basic arithmetic nodes
-- 
-- Phantom type for @MPSNNBinaryArithmeticNode@.
data MPSNNBinaryArithmeticNode

instance IsObjCObject (Id MPSNNBinaryArithmeticNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNBinaryArithmeticNode"

class IsMPSNNFilterNode a => IsMPSNNBinaryArithmeticNode a where
  toMPSNNBinaryArithmeticNode :: a -> Id MPSNNBinaryArithmeticNode

instance IsMPSNNBinaryArithmeticNode (Id MPSNNBinaryArithmeticNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNBinaryArithmeticNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNBinaryArithmeticNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNConcatenationNode ----------

-- | Node representing a the concatenation (in the feature channel dimension) of the results from one or more kernels
-- 
-- Phantom type for @MPSNNConcatenationNode@.
data MPSNNConcatenationNode

instance IsObjCObject (Id MPSNNConcatenationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNConcatenationNode"

class IsMPSNNFilterNode a => IsMPSNNConcatenationNode a where
  toMPSNNConcatenationNode :: a -> Id MPSNNConcatenationNode

instance IsMPSNNConcatenationNode (Id MPSNNConcatenationNode) where
  toMPSNNConcatenationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNConcatenationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNConcatenationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNForwardLossNode ----------

-- | Node representing a MPSNNForwardLoss kernel
-- 
-- Phantom type for @MPSNNForwardLossNode@.
data MPSNNForwardLossNode

instance IsObjCObject (Id MPSNNForwardLossNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNForwardLossNode"

class IsMPSNNFilterNode a => IsMPSNNForwardLossNode a where
  toMPSNNForwardLossNode :: a -> Id MPSNNForwardLossNode

instance IsMPSNNForwardLossNode (Id MPSNNForwardLossNode) where
  toMPSNNForwardLossNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNForwardLossNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNForwardLossNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGradientFilterNode ----------

-- | MPSNNGradientFilterNode
--
-- For each MPSNNFilterNode, there is a corresponding MPSNNGradientFilterNode              used for training that back propagates image gradients to refine the              various parameters in each node. Generally, it takes as input a gradient              corresponding to the result image from the MPSNNFilterNode and returns              a gradient image corresponding to the source image of the MPSNNFilterNode.              In addition, there is generally a MPSNNState produced by the MPSNNFilterNode              that is consumed by the MPSNNGradientNode and the MPSNNGradientNode generally              needs to look at the MPSNNFilterNode source image.
--
-- If you have a simple method to traverse your inference graph backwards, then              -[MPSNNFilterNode gradientFilterWithSource:] is a simple way to construct              these.
-- 
-- Phantom type for @MPSNNGradientFilterNode@.
data MPSNNGradientFilterNode

instance IsObjCObject (Id MPSNNGradientFilterNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGradientFilterNode"

class IsMPSNNFilterNode a => IsMPSNNGradientFilterNode a where
  toMPSNNGradientFilterNode :: a -> Id MPSNNGradientFilterNode

instance IsMPSNNGradientFilterNode (Id MPSNNGradientFilterNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNGradientFilterNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNGradientFilterNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGramMatrixCalculationNode ----------

-- | Node representing a MPSNNGramMatrixCalculation kernel
-- 
-- Phantom type for @MPSNNGramMatrixCalculationNode@.
data MPSNNGramMatrixCalculationNode

instance IsObjCObject (Id MPSNNGramMatrixCalculationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGramMatrixCalculationNode"

class IsMPSNNFilterNode a => IsMPSNNGramMatrixCalculationNode a where
  toMPSNNGramMatrixCalculationNode :: a -> Id MPSNNGramMatrixCalculationNode

instance IsMPSNNGramMatrixCalculationNode (Id MPSNNGramMatrixCalculationNode) where
  toMPSNNGramMatrixCalculationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNGramMatrixCalculationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNGramMatrixCalculationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNInitialGradientNode ----------

-- | MPSNNInitialGradientNode
--
-- A node for a MPSNNInitialGradient kernel
--
-- This node can be used to generate a starting point for an arbitrary gradient computation.                  Simply add this node after the node for which you want to compute gradients and then                  call the function trainingGraphWithSourceGradient: of this node to automatically                  generate the nodes needed for gradient computations or add the desired nodes manually.                  This is generally used with MPSNNLossGradientNode and MPSNNForwardLossNode
-- 
-- Phantom type for @MPSNNInitialGradientNode@.
data MPSNNInitialGradientNode

instance IsObjCObject (Id MPSNNInitialGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNInitialGradientNode"

class IsMPSNNFilterNode a => IsMPSNNInitialGradientNode a where
  toMPSNNInitialGradientNode :: a -> Id MPSNNInitialGradientNode

instance IsMPSNNInitialGradientNode (Id MPSNNInitialGradientNode) where
  toMPSNNInitialGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNInitialGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNInitialGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNPadNode ----------

-- | MPSNNPadNode
--
-- A node for a MPSNNPad kernel
--
-- You should not use this node to zero pad your data in the XY-plane.                  This node copies the input image and therefore should only be used in                  special circumstances where the normal padding operation, defined for most                  filters and nodes through MPSNNPadding, cannot achieve the necessary padding.                  Therefore use this node only when you need one of the special edge modes:                  MPSImageEdgeModeConstant, MPSImageEdgeModeMirror,                  MPSImageEdgeModeMirrorWithEdge or, if you need padding in the                  feature-channel dimesion.                  In other cases use to MPSNNPadding to get best performance.
-- 
-- Phantom type for @MPSNNPadNode@.
data MPSNNPadNode

instance IsObjCObject (Id MPSNNPadNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNPadNode"

class IsMPSNNFilterNode a => IsMPSNNPadNode a where
  toMPSNNPadNode :: a -> Id MPSNNPadNode

instance IsMPSNNPadNode (Id MPSNNPadNode) where
  toMPSNNPadNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNPadNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNPadNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReshapeNode ----------

-- | A node for a MPSNNReshape kernel
-- 
-- Phantom type for @MPSNNReshapeNode@.
data MPSNNReshapeNode

instance IsObjCObject (Id MPSNNReshapeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReshapeNode"

class IsMPSNNFilterNode a => IsMPSNNReshapeNode a where
  toMPSNNReshapeNode :: a -> Id MPSNNReshapeNode

instance IsMPSNNReshapeNode (Id MPSNNReshapeNode) where
  toMPSNNReshapeNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReshapeNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNReshapeNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNScaleNode ----------

-- | Abstract Node representing a image resampling operation
--
-- Please make a MPSNNBilinearScale or MPSNNLanczosScale object instead
-- 
-- Phantom type for @MPSNNScaleNode@.
data MPSNNScaleNode

instance IsObjCObject (Id MPSNNScaleNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNScaleNode"

class IsMPSNNFilterNode a => IsMPSNNScaleNode a where
  toMPSNNScaleNode :: a -> Id MPSNNScaleNode

instance IsMPSNNScaleNode (Id MPSNNScaleNode) where
  toMPSNNScaleNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNScaleNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNScaleNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNUnaryReductionNode ----------

-- | A node for a unary MPSNNReduce node.
--
-- This is an abstract base class that does not correspond with any              particular MPSCNNKernel. Please make one of the MPSNNReduction              subclasses instead.
-- 
-- Phantom type for @MPSNNUnaryReductionNode@.
data MPSNNUnaryReductionNode

instance IsObjCObject (Id MPSNNUnaryReductionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNUnaryReductionNode"

class IsMPSNNFilterNode a => IsMPSNNUnaryReductionNode a where
  toMPSNNUnaryReductionNode :: a -> Id MPSNNUnaryReductionNode

instance IsMPSNNUnaryReductionNode (Id MPSNNUnaryReductionNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNUnaryReductionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNUnaryReductionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNBinaryGradientStateNode ----------

-- | MPSNNBinaryGradientStateNode
-- 
-- Phantom type for @MPSNNBinaryGradientStateNode@.
data MPSNNBinaryGradientStateNode

instance IsObjCObject (Id MPSNNBinaryGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNBinaryGradientStateNode"

class IsMPSNNStateNode a => IsMPSNNBinaryGradientStateNode a where
  toMPSNNBinaryGradientStateNode :: a -> Id MPSNNBinaryGradientStateNode

instance IsMPSNNBinaryGradientStateNode (Id MPSNNBinaryGradientStateNode) where
  toMPSNNBinaryGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSNNBinaryGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNBinaryGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGradientStateNode ----------

-- | MPSNNGradientStateNode
--
-- During training, each MPSNNFilterNode has a corresponding              MPSNNGradientFilterNode for the gradient computation for              trainable parameter update. The two communicate through a              MPSNNGradientStateNode or subclass which carries information              about the inference pass settings to the gradient pass.              You can avoid managing these -- there will be many! -- by              using -[MPSNNFilterNode gradientFilterWithSources:] to make              the MPSNNGradientFilterNodes. That method will append              the necessary extra information like MPSNNGradientState              nodes and inference filter source image nodes to the object as              needed.
-- 
-- Phantom type for @MPSNNGradientStateNode@.
data MPSNNGradientStateNode

instance IsObjCObject (Id MPSNNGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGradientStateNode"

class IsMPSNNStateNode a => IsMPSNNGradientStateNode a where
  toMPSNNGradientStateNode :: a -> Id MPSNNGradientStateNode

instance IsMPSNNGradientStateNode (Id MPSNNGradientStateNode) where
  toMPSNNGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSNNGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNLabelsNode ----------

-- | MPSNNLabelsNode
--
-- The labels and weights for each MPSImage are passed in               separately to the graph in a MPSNNLabels object. If               the batch interface is used then there will be a               MPSStateBatch of these of the same size as the MPSImageBatch               that holds the images.  The MPSNNLabelsNode is a place               holder in the graph for these nodes. The MPSNNLabels node               is taken as an input to the Loss node
-- 
-- Phantom type for @MPSNNLabelsNode@.
data MPSNNLabelsNode

instance IsObjCObject (Id MPSNNLabelsNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNLabelsNode"

class IsMPSNNStateNode a => IsMPSNNLabelsNode a where
  toMPSNNLabelsNode :: a -> Id MPSNNLabelsNode

instance IsMPSNNLabelsNode (Id MPSNNLabelsNode) where
  toMPSNNLabelsNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSNNLabelsNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNLabelsNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNMultiaryGradientStateNode ----------

-- | Phantom type for @MPSNNMultiaryGradientStateNode@.
data MPSNNMultiaryGradientStateNode

instance IsObjCObject (Id MPSNNMultiaryGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNMultiaryGradientStateNode"

class IsMPSNNStateNode a => IsMPSNNMultiaryGradientStateNode a where
  toMPSNNMultiaryGradientStateNode :: a -> Id MPSNNMultiaryGradientStateNode

instance IsMPSNNMultiaryGradientStateNode (Id MPSNNMultiaryGradientStateNode) where
  toMPSNNMultiaryGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSNNMultiaryGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNMultiaryGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSGRUDescriptor ----------

-- | MPSGRUDescriptor
--
-- This depends on Metal.framework
--
-- The MPSGRUDescriptor specifies a GRU (Gated Recurrent Unit) block/layer descriptor.              The RNN layer initialized with a MPSGRUDescriptor transforms the input data (image or matrix),              and previous output with a set of filters, each producing one feature map in              the output data according to the Gated unit formulae detailed below.              The user may provide the GRU unit a single input or a sequence of inputs. The layer also supports              p-norm gating (Detailed in: https://arxiv.org/abs/1608.03639 ).
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h_i be the proposed new output.              Let h1_i be the output data produced at this time step.
--
-- Let Wz_ij, Uz_ij, be the input gate weights for input and recurrent input data respectively              Let bi_i be the bias for the input gate
--
-- Let Wr_ij, Ur_ij be the recurrent gate weights for input and recurrent input data respectively              Let br_i be the bias for the recurrent gate
--
-- Let Wh_ij, Uh_ij, Vh_ij, be the output gate weights for input, recurrent gate and input gate respectively              Let bh_i be the bias for the output gate
--
-- Let gz(x), gr(x), gh(x) be the neuron activation function for the input, recurrent and output gates              Let p > 0 be a scalar variable (typicall p >= 1.0) that defines the p-norm gating norm value.
--
-- Then the output of the Gated Recurrent Unit layer is computed as follows:
--
-- z_i = gz(  Wz_ij * x_j  +  Uz_ij * h0_j  +  bz_i  )                      r_i = gr(  Wr_ij * x_j  +  Ur_ij * h0_j  +  br_i  )                      c_i =      Uh_ij * (r_j h0_j)  +  Vh_ij * (z_j h0_j)                      h_i = gh(  Wh_ij * x_j  + c_i + bh_i  )
--
-- h1_i = ( 1 - z_i ^ p)^(1/p) h_i + z_i h0_i
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and all U and V matrices have to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.              The conventional GRU block is achieved by setting Vh = 0 (nil) and the so-called Minimal Gated Unit is achieved with Uh = 0.              (The Minimal Gated Unit is detailed in: https://arxiv.org/abs/1603.09420 and there they call z_i the value of the forget gate).
-- 
-- Phantom type for @MPSGRUDescriptor@.
data MPSGRUDescriptor

instance IsObjCObject (Id MPSGRUDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGRUDescriptor"

class IsMPSRNNDescriptor a => IsMPSGRUDescriptor a where
  toMPSGRUDescriptor :: a -> Id MPSGRUDescriptor

instance IsMPSGRUDescriptor (Id MPSGRUDescriptor) where
  toMPSGRUDescriptor = unsafeCastId

instance IsMPSRNNDescriptor (Id MPSGRUDescriptor) where
  toMPSRNNDescriptor = unsafeCastId

instance IsNSObject (Id MPSGRUDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSLSTMDescriptor ----------

-- | MPSLSTMDescriptor
--
-- This depends on Metal.framework
--
-- The MPSLSTMDescriptor specifies a LSTM block/layer descriptor.              The RNN layer initialized with a MPSLSTMDescriptor transforms the input data (image or matrix),              the memory cell data and previous output with a set of filters, each producing one feature map in              the output data and memory cell, according to the LSTM formulae detailed below.              The user may provide the LSTM unit a single input or a sequence of inputs.
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h1_i be the output data produced at this time step.              Let c0_j be the previous memory cell data (at time index t-1 of sequence).              Let c1_i be the new memory cell data (at time index t-1 of sequence).
--
-- Let Wi_ij, Ui_ij, Vi_ij, be the input gate weights for input, recurrent input and memory cell (peephole) data respectively              Let bi_i be the bias for the input gate
--
-- Let Wf_ij, Uf_ij, Vf_ij, be the forget gate weights for input, recurrent input and memory cell data respectively              Let bf_i be the bias for the forget gate
--
-- Let Wo_ij, Uo_ij, Vo_ij, be the output gate weights for input, recurrent input and memory cell data respectively              Let bo_i be the bias for the output gate
--
-- Let Wc_ij, Uc_ij, Vc_ij, be the memory cell gate weights for input, recurrent input and memory cell data respectively              Let bc_i be the bias for the memory cell gate
--
-- Let gi(x), gf(x), go(x), gc(x) be neuron activation function for the input, forget, output gate and memory cell gate              Let gh(x) be the activation function applied to result memory cell data
--
-- Then the new memory cell data c1_j and output image h1_i are computed as follows:
--
-- I_i = gi(  Wi_ij * x_j  +  Ui_ij * h0_j  +  Vi_ij * c0_j  + bi_i  )                      F_i = gf(  Wf_ij * x_j  +  Uf_ij * h0_j  +  Vf_ij * c0_j  + bf_i  )                      C_i = gc(  Wc_ij * x_j  +  Uc_ij * h0_j  +  Vc_ij * c0_j  + bc_i  )
--
-- c1_i = F_i c0_i  +  I_i C_i
--
-- O_i = go(  Wo_ij * x_j  +  Uo_ij * h0_j  +  Vo_ij * c1_j  + bo_i  )
--
-- h1_i = O_i gh( c1_i )
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and all U and V matrices have to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.
-- 
-- Phantom type for @MPSLSTMDescriptor@.
data MPSLSTMDescriptor

instance IsObjCObject (Id MPSLSTMDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSLSTMDescriptor"

class IsMPSRNNDescriptor a => IsMPSLSTMDescriptor a where
  toMPSLSTMDescriptor :: a -> Id MPSLSTMDescriptor

instance IsMPSLSTMDescriptor (Id MPSLSTMDescriptor) where
  toMPSLSTMDescriptor = unsafeCastId

instance IsMPSRNNDescriptor (Id MPSLSTMDescriptor) where
  toMPSRNNDescriptor = unsafeCastId

instance IsNSObject (Id MPSLSTMDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNSingleGateDescriptor ----------

-- | MPSRNNSingleGateDescriptor
--
-- This depends on Metal.framework
--
-- The MPSRNNSingleGateDescriptor specifies a simple recurrent block/layer descriptor.              The RNN layer initialized with a MPSRNNSingleGateDescriptor transforms the input data (image or matrix),              and previous output with a set of filters, each producing one feature map in the new output data.              The user may provide the RNN unit a single input or a sequence of inputs.
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h1_i be the output data produced at this time step.
--
-- Let W_ij, U_ij be the weights for input and recurrent input data respectively              Let b_i be a bias term
--
-- Let gi(x) be a neuron activation function
--
-- Then the new output image h1_i data is computed as follows:
--
-- h1_i = gi( W_ij * x_j + U_ij * h0_j  + b_i )
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and the U matrix has to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.
-- 
-- Phantom type for @MPSRNNSingleGateDescriptor@.
data MPSRNNSingleGateDescriptor

instance IsObjCObject (Id MPSRNNSingleGateDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNSingleGateDescriptor"

class IsMPSRNNDescriptor a => IsMPSRNNSingleGateDescriptor a where
  toMPSRNNSingleGateDescriptor :: a -> Id MPSRNNSingleGateDescriptor

instance IsMPSRNNSingleGateDescriptor (Id MPSRNNSingleGateDescriptor) where
  toMPSRNNSingleGateDescriptor = unsafeCastId

instance IsMPSRNNDescriptor (Id MPSRNNSingleGateDescriptor) where
  toMPSRNNDescriptor = unsafeCastId

instance IsNSObject (Id MPSRNNSingleGateDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionWeightsAndBiasesState ----------

-- | MPSCNNConvolutionWeightsAndBiasesState
--
-- The MPSCNNConvolutionWeightsAndBiasesState is returned by exportWeightsAndBiasesWithCommandBuffer: method on MPSCNNConvolution object.              This is mainly used for GPU side weights/biases update process.              During training, application can keep a copy of weights, velocity, momentum MTLBuffers in its data source, update the weights (in-place or out of place)              with gradients obtained from MPSCNNConvolutionGradientState and call [MPSCNNConvolution reloadWeightsAndBiasesWithCommandBuffer] with resulting updated              MTLBuffer. If application does not want to keep a copy of weights/biases, it can call [MPSCNNConvolution exportWeightsAndBiasesWithCommandBuffer:] to get              the current weights from convolution itself, do the updated and call reloadWithCommandBuffer.
-- 
-- Phantom type for @MPSCNNConvolutionWeightsAndBiasesState@.
data MPSCNNConvolutionWeightsAndBiasesState

instance IsObjCObject (Id MPSCNNConvolutionWeightsAndBiasesState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionWeightsAndBiasesState"

class IsMPSState a => IsMPSCNNConvolutionWeightsAndBiasesState a where
  toMPSCNNConvolutionWeightsAndBiasesState :: a -> Id MPSCNNConvolutionWeightsAndBiasesState

instance IsMPSCNNConvolutionWeightsAndBiasesState (Id MPSCNNConvolutionWeightsAndBiasesState) where
  toMPSCNNConvolutionWeightsAndBiasesState = unsafeCastId

instance IsMPSState (Id MPSCNNConvolutionWeightsAndBiasesState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionWeightsAndBiasesState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLossLabels ----------

-- | MPSCNNLossLabels
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossLabels is used to hold the per-element weights buffer              used by both MPSCNNLoss forward filter and MPSNNLossGradient backward filter.              The MPSCNNLoss forward filter populates the MPSCNNLossLabels object              and the MPSNNLossGradient backward filter consumes the state object.
-- 
-- Phantom type for @MPSCNNLossLabels@.
data MPSCNNLossLabels

instance IsObjCObject (Id MPSCNNLossLabels) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLossLabels"

class IsMPSState a => IsMPSCNNLossLabels a where
  toMPSCNNLossLabels :: a -> Id MPSCNNLossLabels

instance IsMPSCNNLossLabels (Id MPSCNNLossLabels) where
  toMPSCNNLossLabels = unsafeCastId

instance IsMPSState (Id MPSCNNLossLabels) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNLossLabels) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNormalizationGammaAndBetaState ----------

-- | MPSCNNNormalizationGammaAndBetaState
--
-- A state which contains gamma and beta terms used to apply a scale               and bias in either an MPSCNNInstanceNormalization or MPSCNNBatchNormalization               operation.
-- 
-- Phantom type for @MPSCNNNormalizationGammaAndBetaState@.
data MPSCNNNormalizationGammaAndBetaState

instance IsObjCObject (Id MPSCNNNormalizationGammaAndBetaState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNormalizationGammaAndBetaState"

class IsMPSState a => IsMPSCNNNormalizationGammaAndBetaState a where
  toMPSCNNNormalizationGammaAndBetaState :: a -> Id MPSCNNNormalizationGammaAndBetaState

instance IsMPSCNNNormalizationGammaAndBetaState (Id MPSCNNNormalizationGammaAndBetaState) where
  toMPSCNNNormalizationGammaAndBetaState = unsafeCastId

instance IsMPSState (Id MPSCNNNormalizationGammaAndBetaState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNNormalizationGammaAndBetaState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNormalizationMeanAndVarianceState ----------

-- | MPSCNNNormalizationMeanAndVarianceState
--
-- A state which contains mean and variance terms used to apply a               normalization in a MPSCNNBatchNormalization operation.
-- 
-- Phantom type for @MPSCNNNormalizationMeanAndVarianceState@.
data MPSCNNNormalizationMeanAndVarianceState

instance IsObjCObject (Id MPSCNNNormalizationMeanAndVarianceState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNormalizationMeanAndVarianceState"

class IsMPSState a => IsMPSCNNNormalizationMeanAndVarianceState a where
  toMPSCNNNormalizationMeanAndVarianceState :: a -> Id MPSCNNNormalizationMeanAndVarianceState

instance IsMPSCNNNormalizationMeanAndVarianceState (Id MPSCNNNormalizationMeanAndVarianceState) where
  toMPSCNNNormalizationMeanAndVarianceState = unsafeCastId

instance IsMPSState (Id MPSCNNNormalizationMeanAndVarianceState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNNormalizationMeanAndVarianceState) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayGradientState ----------

-- | A state created to record a MPSCNNKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage:]          or analogous interfaces.
-- 
-- Phantom type for @MPSNDArrayGradientState@.
data MPSNDArrayGradientState

instance IsObjCObject (Id MPSNDArrayGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayGradientState"

class IsMPSState a => IsMPSNDArrayGradientState a where
  toMPSNDArrayGradientState :: a -> Id MPSNDArrayGradientState

instance IsMPSNDArrayGradientState (Id MPSNDArrayGradientState) where
  toMPSNDArrayGradientState = unsafeCastId

instance IsMPSState (Id MPSNDArrayGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSNDArrayGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSNNBinaryGradientState ----------

-- | A state created to record MPSCNNBinaryKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNBinaryKernel resultStateForPrimaryImage:secondaryImage:sourceStates:destinationImage:]          or analogous interfaces.
-- 
-- Phantom type for @MPSNNBinaryGradientState@.
data MPSNNBinaryGradientState

instance IsObjCObject (Id MPSNNBinaryGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNBinaryGradientState"

class IsMPSState a => IsMPSNNBinaryGradientState a where
  toMPSNNBinaryGradientState :: a -> Id MPSNNBinaryGradientState

instance IsMPSNNBinaryGradientState (Id MPSNNBinaryGradientState) where
  toMPSNNBinaryGradientState = unsafeCastId

instance IsMPSState (Id MPSNNBinaryGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSNNBinaryGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGradientState ----------

-- | A state created to record a MPSCNNKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage:]          or analogous interfaces.
-- 
-- Phantom type for @MPSNNGradientState@.
data MPSNNGradientState

instance IsObjCObject (Id MPSNNGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGradientState"

class IsMPSState a => IsMPSNNGradientState a where
  toMPSNNGradientState :: a -> Id MPSNNGradientState

instance IsMPSNNGradientState (Id MPSNNGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSNNGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSNNGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSNNMultiaryGradientState ----------

-- | Phantom type for @MPSNNMultiaryGradientState@.
data MPSNNMultiaryGradientState

instance IsObjCObject (Id MPSNNMultiaryGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNMultiaryGradientState"

class IsMPSState a => IsMPSNNMultiaryGradientState a where
  toMPSNNMultiaryGradientState :: a -> Id MPSNNMultiaryGradientState

instance IsMPSNNMultiaryGradientState (Id MPSNNMultiaryGradientState) where
  toMPSNNMultiaryGradientState = unsafeCastId

instance IsMPSState (Id MPSNNMultiaryGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSNNMultiaryGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNMatrixTrainingState ----------

-- | MPSRNNMatrixTrainingState
--
-- This depends on Metal.framework
--
-- This class holds the data that is passed from the forward pass needed in the backward pass.
-- 
-- Phantom type for @MPSRNNMatrixTrainingState@.
data MPSRNNMatrixTrainingState

instance IsObjCObject (Id MPSRNNMatrixTrainingState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNMatrixTrainingState"

class IsMPSState a => IsMPSRNNMatrixTrainingState a where
  toMPSRNNMatrixTrainingState :: a -> Id MPSRNNMatrixTrainingState

instance IsMPSRNNMatrixTrainingState (Id MPSRNNMatrixTrainingState) where
  toMPSRNNMatrixTrainingState = unsafeCastId

instance IsMPSState (Id MPSRNNMatrixTrainingState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSRNNMatrixTrainingState) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNRecurrentImageState ----------

-- | MPSRNNRecurrentImageState
--
-- This depends on Metal.framework
--
-- This class holds all the data that is passed from one sequence iteration of the image-based RNN layer (stack) to the next.
-- 
-- Phantom type for @MPSRNNRecurrentImageState@.
data MPSRNNRecurrentImageState

instance IsObjCObject (Id MPSRNNRecurrentImageState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNRecurrentImageState"

class IsMPSState a => IsMPSRNNRecurrentImageState a where
  toMPSRNNRecurrentImageState :: a -> Id MPSRNNRecurrentImageState

instance IsMPSRNNRecurrentImageState (Id MPSRNNRecurrentImageState) where
  toMPSRNNRecurrentImageState = unsafeCastId

instance IsMPSState (Id MPSRNNRecurrentImageState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSRNNRecurrentImageState) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNRecurrentMatrixState ----------

-- | MPSRNNRecurrentMatrixState
--
-- This depends on Metal.framework
--
-- This class holds all the data that is passed from one sequence iteration of the matrix-based RNN layer to the next.
-- 
-- Phantom type for @MPSRNNRecurrentMatrixState@.
data MPSRNNRecurrentMatrixState

instance IsObjCObject (Id MPSRNNRecurrentMatrixState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNRecurrentMatrixState"

class IsMPSState a => IsMPSRNNRecurrentMatrixState a where
  toMPSRNNRecurrentMatrixState :: a -> Id MPSRNNRecurrentMatrixState

instance IsMPSRNNRecurrentMatrixState (Id MPSRNNRecurrentMatrixState) where
  toMPSRNNRecurrentMatrixState = unsafeCastId

instance IsMPSState (Id MPSRNNRecurrentMatrixState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSRNNRecurrentMatrixState) where
  toNSObject = unsafeCastId

-- ---------- MPSTemporaryVector ----------

-- | A MPSVector allocated on GPU private memory.
--
-- It may alias one or more other MPSTemporaryVector objects. Undesired data destruction              due to aliasing is avoided using the readCount property.
-- 
-- Phantom type for @MPSTemporaryVector@.
data MPSTemporaryVector

instance IsObjCObject (Id MPSTemporaryVector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTemporaryVector"

class IsMPSVector a => IsMPSTemporaryVector a where
  toMPSTemporaryVector :: a -> Id MPSTemporaryVector

instance IsMPSTemporaryVector (Id MPSTemporaryVector) where
  toMPSTemporaryVector = unsafeCastId

instance IsMPSVector (Id MPSTemporaryVector) where
  toMPSVector = unsafeCastId

instance IsNSObject (Id MPSTemporaryVector) where
  toNSObject = unsafeCastId

-- ---------- MPSInstanceAccelerationStructure ----------

-- | An acceleration structure built over instances of other acceleration structures
--
-- Instancing can be used to reduce memory usage in scenes that contain many copies of the same object(s) or to combine multiple acceleration structures such as a static and dynamic acceleration structure into a two-level instance hierarchy.
--
-- The typical pattern for creating an instance acceleration structure is as follows. First, create individual bottom-level acceleration structures. Then assign these acceleration structures to the accelerationStructures property of an instance acceleration structure.
--
-- All of the acceleration structures in the instance hierarchy must share the same MPSAccelerationStructureGroup. Furthermore, all of the bottom-level acceleration structures must share the same vertex buffer, index buffer, etc. although they may have different offsets within those buffers.
--
-- MPSAccelerationStructureGroup *group = nil;
-- group = [[MPSAccelerationStructureGroup alloc] initWithDevice:device];
--
-- MPSInstanceAccelerationStructure *instanceAccel = nil;
-- instanceAccel = [[MPSInstanceAccelerationStructure alloc] initWithGroup:group];
--
-- NSMutableArray *accelerationStructures = [NSMutableArray array];
-- instanceAccel.accelerationStructures = accelerationStructures;
--
-- instanceAccel.instanceCount = instanceCount;
--
-- for (ObjectType *objectType in objectTypes) {
-- MPSTriangleAccelerationStructure *triAccel = nil;
-- triAccel = [[MPSTriangleAccelerationStructure alloc] initWithGroup:group];
--
-- triAccel.vertexBuffer = objectType.vertexBuffer;
-- triAccel.vertexBufferOffset = objectType.vertexBufferOffset;
-- triAccel.triangleCount = objectType.triangleCount;
--
-- [triAccel rebuild];
--
-- [accelerationStructures addObject:triAccel];
-- }
--
-- Next, create a buffer containing the acceleration structure index for each instance, and another acceleration structure containing the transformation matrix for each instance:
--
-- NSUInteger instanceBufferLength = sizeof(uint32_t) * instanceCount;
--
-- id <MTLBuffer> instanceBuffer =
-- [device newBufferWithLength:instanceBufferLength
-- options:MTLResourceStorageModeManaged];
--
-- memcpy(instanceBuffer.contents, instances,
-- instanceBufferLength);
-- [instanceBuffer
-- didModifyRange:NSMakeRange(0, instanceBufferLength)];
--
-- instanceAccel.instanceBuffer = instanceBuffer;
--
-- // Similar for transformation matrix buffer
--
-- Finally, rebuild the instance acceleration structure:
--
-- [instanceAccel rebuild];
--
-- Refitting and Rebuilding Bottom-Level Acceleration Structures: when a bottom level acceleration structure is rebuild or refit, its' bounding box may change. Therefore, the instance acceleration structure also needs to be rebuilt or refit.
--
-- Copying and Serializing Instance Acceleration Structures: When an instance acceleration structure is copied or serialized, the bottom level acceleration structures are not copied or serialized. These must be copied or serialized along with the instance acceleration structure and assigned to the new instance acceleration structure. This also applies to buffer properties such as the instance buffer, transformation buffer, etc.
--
-- Performance Guidelines:
--
-- - Use instancing to reduce memory usage: if there are many copies of the same object(s) in       a scene, using instances of the same object can reduce memory usage and acceleration       structure build time. Rebuilding or refitting the top level acceleration structure can       also be much faster than rebuilding a large single level acceleration structure.
--
-- - Consider flattening your instance hierarchy into a single acceleration structure if the       increased memory usage and acceleration structure build time are not a concern.       Intersecting a two level acceleration structure can have a significant performance cost so       only use it when necessary. Which technique to use depends on the scene and use case. For       example, in a rendering application, it may be best to use an instance hierarchy for       interactive scene editing and preview and flattening the instance hierarchy for the final       render. For smaller scenes, it may also be sufficient to refit a flattened acceleration       structure rather than rebuilding an instance hierarchy.
--
-- - If there is only a single object in the scene, intersect its acceleration structure       directly instead of using an instance hierarchy.
--
-- - Consider dividing objects into static and dynamic acceleration structures. If dynamic       objects require the acceleration structure to be rebuilt frequently, create a high quality       static acceleration structure and a lower quality but faster to build dynamic acceleration       structure. These two acceleration structures can then be combined with a two level       acceleration structure. Use MPSTransformTypeIdentity to reduce the overhead of this       technique. Whether this technique is more efficient than rebuilding the entire       acceleration structure depends on the scene.
--
-- See MPSAccelerationStructure for more information
-- 
-- Phantom type for @MPSInstanceAccelerationStructure@.
data MPSInstanceAccelerationStructure

instance IsObjCObject (Id MPSInstanceAccelerationStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSInstanceAccelerationStructure"

class IsMPSAccelerationStructure a => IsMPSInstanceAccelerationStructure a where
  toMPSInstanceAccelerationStructure :: a -> Id MPSInstanceAccelerationStructure

instance IsMPSInstanceAccelerationStructure (Id MPSInstanceAccelerationStructure) where
  toMPSInstanceAccelerationStructure = unsafeCastId

instance IsMPSAccelerationStructure (Id MPSInstanceAccelerationStructure) where
  toMPSAccelerationStructure = unsafeCastId

instance IsMPSKernel (Id MPSInstanceAccelerationStructure) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSInstanceAccelerationStructure) where
  toNSObject = unsafeCastId

-- ---------- MPSPolygonAccelerationStructure ----------

-- | An acceleration structure built over polygonal shapes
--
-- See MPSAccelerationStructure for more information
-- 
-- Phantom type for @MPSPolygonAccelerationStructure@.
data MPSPolygonAccelerationStructure

instance IsObjCObject (Id MPSPolygonAccelerationStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSPolygonAccelerationStructure"

class IsMPSAccelerationStructure a => IsMPSPolygonAccelerationStructure a where
  toMPSPolygonAccelerationStructure :: a -> Id MPSPolygonAccelerationStructure

instance IsMPSPolygonAccelerationStructure (Id MPSPolygonAccelerationStructure) where
  toMPSPolygonAccelerationStructure = unsafeCastId

instance IsMPSAccelerationStructure (Id MPSPolygonAccelerationStructure) where
  toMPSAccelerationStructure = unsafeCastId

instance IsMPSKernel (Id MPSPolygonAccelerationStructure) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSPolygonAccelerationStructure) where
  toNSObject = unsafeCastId

-- ---------- MPSImageArithmetic ----------

-- | MPSImageArithmetic
--
-- This depends on Metal.framework.
--
-- This filter takes two source images, a primary source image and a secondary source image,              and outputs a single destination image. It applies an element-wise arithmetic operator to              each pixel in a primary source image and a corresponding pixel in a secondary source image              over a specified region.
--
-- The supported arithmetic operators are the following:              - Addition              - Subtraction              - Multiplication              - Division
--
-- This filter takes additional parameters: primaryScale, secondaryScale, and bias. The default              value for primaryScale and secondaryScale is 1.0f. The default value for bias is 0.0f. This              filter applies primaryScale, secondaryScale, and bias to the primary source pixel (x) and              secondary source pixel (y) in the following way:              - Addition:         result = ((primaryScale * x) + (secondaryScale * y)) + bias              - Subtraction:      result = ((primaryScale * x) - (secondaryScale * y)) + bias              - Multiplicaton:    result = ((primaryScale * x) * (secondaryScale * y)) + bias              - Division:         result = ((primaryScale * x) / (secondaryScale * y)) + bias
--
-- To clamp the result of an arithmetic operation, where              result = clamp(result, minimumValue, maximumValue),              set the minimumValue and maximumValue appropriately. The default value of minimumValue              is -FLT_MAX. The default value of maximumValue is FLT_MAX.
--
-- This filter also takes the following additional parameters:              - primaryStrideInPixels              - secondaryStrideInPixels              These parameters can be used to control broadcasting for the data stored in the primary and              secondary source images. For example, setting all strides for the primary source image to 0              will result in the primarySource image being treated as a scalar value. The only supported              values are 0 or 1. The default value of these parameters is 1.
--
-- This filter accepts uint and int data in addition to unorm and floating-point data.
--
-- You must use one of the sub-classes of MPSImageArithmetic.
-- 
-- Phantom type for @MPSImageArithmetic@.
data MPSImageArithmetic

instance IsObjCObject (Id MPSImageArithmetic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageArithmetic"

class IsMPSBinaryImageKernel a => IsMPSImageArithmetic a where
  toMPSImageArithmetic :: a -> Id MPSImageArithmetic

instance IsMPSImageArithmetic (Id MPSImageArithmetic) where
  toMPSImageArithmetic = unsafeCastId

instance IsMPSBinaryImageKernel (Id MPSImageArithmetic) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSKernel (Id MPSImageArithmetic) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageArithmetic) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNArithmetic ----------

-- | MPSCNNArithmetic
--
-- This depends on Metal.framework
--
-- The MPSCNNArithmetic filter takes two source images, a primary source image and a              secondary source image, and outputs a single destination image. It applies an              element-wise arithmetic operator to each pixel in a primary source image and a              corresponding pixel in a secondary source image over a specified region.
--
-- The supported arithmetic operators are the following:              - Addition              - Subtraction              - Multiplication              - Division              - Comparison
--
-- This filter takes additional parameters: primaryScale, secondaryScale, and bias. The default              value for primaryScale and secondaryScale is 1.0f. The default value for bias is 0.0f. This              filter applies primaryScale, secondaryScale, and bias to the primary source pixel (x) and              secondary source pixel (y) in the following way:              - Addition:         result = ((primaryScale * x) + (secondaryScale * y)) + bias              - Subtraction:      result = ((primaryScale * x) - (secondaryScale * y)) + bias              - Multiplicaton:    result = ((primaryScale * x) * (secondaryScale * y)) + bias              - Division:         result = ((primaryScale * x) / (secondaryScale * y)) + bias              - Comparison:       Unused.
--
-- To clamp the result of an arithmetic operation, where              result = clamp(result, minimumValue, maximumValue),              set the minimumValue and maximumValue appropriately. The default value of minimumValue              is -FLT_MAX. The default value of maximumValue is FLT_MAX.
--
-- This filter also takes the following additional parameters:              - primaryStrideInPixelsX, primaryStrideInPixelsY, primaryStrideInFeatureChannels              - secondaryStrideInPixelsX, secondaryStrideInPixelsY, secondaryStrideInFeatureChannels              These parameters can be used to control broadcasting for the data stored in the primary and              secondary source images. For example, setting all strides for the primary source image to 0              will result in the primarySource image being treated as a scalar value. The only supported              values are 0 or 1. The default value of these parameters is 1.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- You must use one of the sub-classes of MPSImageArithmetic.
-- 
-- Phantom type for @MPSCNNArithmetic@.
data MPSCNNArithmetic

instance IsObjCObject (Id MPSCNNArithmetic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNArithmetic"

class IsMPSCNNBinaryKernel a => IsMPSCNNArithmetic a where
  toMPSCNNArithmetic :: a -> Id MPSCNNArithmetic

instance IsMPSCNNArithmetic (Id MPSCNNArithmetic) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNArithmetic) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNArithmetic) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNArithmetic) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGradientKernel ----------

-- | MPSCNNGradientKernel
--
-- Gradient kernels are the backwards pass of a MPSCNNKernel              used during training to calculate gradient back propagation.              These take as arguments the gradient result from the next filter              and the source image for the forward version of the filter.              There is also a MPSNNGradientState passed from MPSCNNKernel              to MPSCNNGradientKernel that contains information about the              MPSCNNKernel parameters at the time it encoded and possibly              also additional MTLResources to enable it to do its job.
--
-- Training graph (partial):
--
-- ---> input image ---------> MPSCNNKernel ------>  resultImage ------>-->-->-->.
-- \                  |                                           |
-- '------.    MPSNNGradientState                         loss estimation
-- \         |                                           |
-- V        V                                           V
-- <--- result gradient <- MPSCNNGradientKernel <---  input gradient <--<--<--<---'
--
-- In general operation, starting with the input image, the sequence of events is:
-- 1a)  Invoke padding policy to find result size for MPSCNNKernel.  This
-- also configures some MPSCNNKernel parameters such as offset.
-- 1b)  Use the MPSImageDescriptor from 1a to make resultImage.
-- 1c)  Call MPSCNNKernel -encode...
-- 2) stages 1a-c are repeated for other forward passes in the inference portion of the graph
-- 3) We estimate the loss resulting from the whole inference computation so far (see MPSCNNLoss.h>
-- 4) stages 5a-c are repeated for corresponding backward gradient passes in the graph
-- 5a) Invoke padding policy on the MPSCNNGradientKernel shown above. This sets the
-- MPSCNNGradientKernel parameters to correspond with those in the forward pass
-- 5b) The result gradient for the MPSCNNGradientKernel is created from the MPSImageDescriptor from 5a
-- 5c) Call MPSCNNGradientKernel -encode with the input image, input gradient, result gradient and MPSNNGradientState
-- 6) pass the result gradient on to leftward gradient passes.
--
-- For MPSCNNKernels that are trained, there may be other accompanying training kernels that              need to be called in addition to the gradient kernel to update convolution weights or batch              normalization parameters, for example. Steps 1a-c and 5a-c can be combined in a single -encode              call. These return the result image or gradient out the left hand side.
--
-- For purposes of inheritance the gradient image is the MPSCNNBinaryKernel primary image              and the source image is the MPSCNNBinaryKernel secondary image. Various secondary properties              such as kernel size are copies of the forward inference pass parameters of similar name              are set automatically when -[MPSCNNGradientKernel destinationImageDescriptorForSourceImages:sourceStates:]              is called.
-- 
-- Phantom type for @MPSCNNGradientKernel@.
data MPSCNNGradientKernel

instance IsObjCObject (Id MPSCNNGradientKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGradientKernel"

class IsMPSCNNBinaryKernel a => IsMPSCNNGradientKernel a where
  toMPSCNNGradientKernel :: a -> Id MPSCNNGradientKernel

instance IsMPSCNNGradientKernel (Id MPSCNNGradientKernel) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNGradientKernel) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNGradientKernel) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNGradientKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGridSample ----------

-- | Phantom type for @MPSNNGridSample@.
data MPSNNGridSample

instance IsObjCObject (Id MPSNNGridSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGridSample"

class IsMPSCNNBinaryKernel a => IsMPSNNGridSample a where
  toMPSNNGridSample :: a -> Id MPSNNGridSample

instance IsMPSNNGridSample (Id MPSNNGridSample) where
  toMPSNNGridSample = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNGridSample) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNGridSample) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNGridSample) where
  toNSObject = unsafeCastId

-- ---------- MPSNNLossGradient ----------

-- | MPSNNLossGradient
--
-- This depends on Metal.framework.
--
-- The MPSNNLossGradient filter specifies the gradient filter for MPSNNForwardLoss.
-- 
-- Phantom type for @MPSNNLossGradient@.
data MPSNNLossGradient

instance IsObjCObject (Id MPSNNLossGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNLossGradient"

class IsMPSCNNBinaryKernel a => IsMPSNNLossGradient a where
  toMPSNNLossGradient :: a -> Id MPSNNLossGradient

instance IsMPSNNLossGradient (Id MPSNNLossGradient) where
  toMPSNNLossGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNLossGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNLossGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNLossGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceBinary ----------

-- | MPSNNReduceBinary
--
-- The MPSNNReduce performs a reduction operation              The reduction operations supported are:                   - Reduce feature channels mean
-- 
-- Phantom type for @MPSNNReduceBinary@.
data MPSNNReduceBinary

instance IsObjCObject (Id MPSNNReduceBinary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceBinary"

class IsMPSCNNBinaryKernel a => IsMPSNNReduceBinary a where
  toMPSNNReduceBinary :: a -> Id MPSNNReduceBinary

instance IsMPSNNReduceBinary (Id MPSNNReduceBinary) where
  toMPSNNReduceBinary = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNReduceBinary) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceBinary) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNReduceBinary) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalization ----------

-- | MPSCNNBatchNormalization
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalization normalizes input images using per-channel              means and variances.
--
-- for (c = 0; c < numberOfFeatureChannels; ++c)              {                  input_image = in(:,:,c,:);                  output_image = (input_image - mean[c]) * gamma[c] / sqrt(variance[c] + epsilon) + beta[c];                  out(:,:,c,:) = output_image;              }
-- 
-- Phantom type for @MPSCNNBatchNormalization@.
data MPSCNNBatchNormalization

instance IsObjCObject (Id MPSCNNBatchNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalization"

class IsMPSCNNKernel a => IsMPSCNNBatchNormalization a where
  toMPSCNNBatchNormalization :: a -> Id MPSCNNBatchNormalization

instance IsMPSCNNBatchNormalization (Id MPSCNNBatchNormalization) where
  toMPSCNNBatchNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNBatchNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBatchNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationStatistics ----------

-- | MPSCNNBatchNormalizationStatistics
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationStatistics updates a MPSCNNBatchNormalizationState              with the batch statistics necessary to perform a batch normalization.              MPSCNNBatchNormalizationStatistics may be executed multiple times with              multiple images to accumulate all the statistics necessary to perform              a batch normalization as described in  https://arxiv.org/pdf/1502.03167v3.pdf.
-- 
-- Phantom type for @MPSCNNBatchNormalizationStatistics@.
data MPSCNNBatchNormalizationStatistics

instance IsObjCObject (Id MPSCNNBatchNormalizationStatistics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationStatistics"

class IsMPSCNNKernel a => IsMPSCNNBatchNormalizationStatistics a where
  toMPSCNNBatchNormalizationStatistics :: a -> Id MPSCNNBatchNormalizationStatistics

instance IsMPSCNNBatchNormalizationStatistics (Id MPSCNNBatchNormalizationStatistics) where
  toMPSCNNBatchNormalizationStatistics = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNBatchNormalizationStatistics) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBatchNormalizationStatistics) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationStatistics) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBinaryConvolution ----------

-- | MPSCNNBinaryConvolution
--
-- This depends on Metal.framework
--
-- The MPSCNNBinaryConvolution specifies a convolution with binary weights and an input image using binary approximations.              The MPSCNNBinaryConvolution optionally first binarizes the input image and then convolves the result with a set of              binary-valued filters, each producing one feature map in the output image (which is a normal image)
--
-- The output is computed as follows:
--
-- out[i, x, y, c] = ( sum_{dx,dy,f} in[i,x+dx, y+dy, f] x B[c,dx,dy,f] )                                      * scale[c] * beta[i,x,y] + bias[c], where
--
-- the sum over dx,dy is over the spatial filter kernel window defined by 'kernelWidth' and 'KernelHeight',              sum over 'f' is over the input feature channel indices within group, 'B' contains the binary weights, interpreted as              {-1,1} or { 0, 1 } and scale[c] is the 'outputScaleTerms' array and bias is the 'outputBiasTerms' array. Above 'i' is              the image index in batch the sum over input channels 'f' runs through the group indices.
--
-- The convolution operator 'x' is defined by MPSCNNBinaryConvolutionType passed in at initialization time of the filter              (
--
-- See: initWithDevice).              In case 'type' = MPSCNNBinaryConvolutionTypeBinaryWeights, the input image is not binarized at all                  and the convolution is computed interpreting the weights as [ 0, 1 ] -> { -1, 1 } with the given scaling terms.              In case 'type' = MPSCNNBinaryConvolutionTypeXNOR the convolution is computed by first binarizing the input image                  using the sign function 'bin(x) = x < 0 ? -1 : 1' and the convolution multiplication is done with the                  XNOR-operator !(x ^ y) = delta_xy = { (x==y) ? 1 : 0 },                  and scaled according to the optional scaling operations. Note that we output the values of the bitwise convolutions                  to interval { -1, 1 }, which means that the output of the XNOR-operator is scaled implicitly as follows:                      r = 2 * ( !(x ^ y) ) - 1 = { -1, 1 }.                  This means that for a dot-product of two 32-bit words the result is:                      r = 2 * popcount(!(x ^ y) ) - 32 = 32 - 2 * popcount( x ^ y ) = { -32, -30, ..., 30, 32 }.              In case 'type' = MPSCNNBinaryConvolutionTypeAND the convolution is computed by first binarizing the input image                  using the sign function 'bin(x) = x < 0 ? -1 : 1' and the convolution multiplication is done with the                  AND-operator (x & y) = delta_xy * delta_x1 = { (x==y==1) ? 1 : 0 }.                  and scaled according to the optional scaling operations. Note that we output the values of the AND-operation is                  assumed to lie in { 0, 1 } interval and hence no more implicit scaling takes place.                  This means that for a dot-product of two 32-bit words the result is:                      r = popcount(x & y) = { 0, ..., 31, 32 }.
--
-- The input data can be pre-offset and scaled by providing the 'inputBiasTerms' and 'inputScaleTerms' parameters for the              initialization functions and this can be used for example to accomplish batch normalization of the data. The scaling of              input values happens before possible beta-image computation.
--
-- The parameter 'beta' above is an optional image which is used to compute scaling factors for each spatial position and image index.              For the XNOR-Net based networks this is computed as follows: beta[i,x,y] = sum_{dx,dy} A[i, x+dx, y+dy] / (kx * ky), where              (dx,dy) are summed over the convolution filter window [ -kx/2, (kx-1)/2], [ -ky/2, (ky-1)/2 ] and              A[i,x,y] = sum_{c} abs( in[i,x,y,c] ) / Nc, where 'in' is the original input image (in full precision) and Nc is the              number of input channels in the input image. Parameter 'beta' is not passed as input and to enable beta-scaling the user can              provide 'MPSCNNBinaryConvolutionFlagsUseBetaScaling' in the flags parameter in the initialization functions.
--
-- Finally the normal activation neuron is applied and the result is written to the output image.
--
-- NOTE: MPSCNNBinaryConvolution does not currently support groups > 1.
-- 
-- Phantom type for @MPSCNNBinaryConvolution@.
data MPSCNNBinaryConvolution

instance IsObjCObject (Id MPSCNNBinaryConvolution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBinaryConvolution"

class IsMPSCNNKernel a => IsMPSCNNBinaryConvolution a where
  toMPSCNNBinaryConvolution :: a -> Id MPSCNNBinaryConvolution

instance IsMPSCNNBinaryConvolution (Id MPSCNNBinaryConvolution) where
  toMPSCNNBinaryConvolution = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNBinaryConvolution) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBinaryConvolution) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBinaryConvolution) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolution ----------

-- | MPSCNNConvolution
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolution specifies a convolution.              The MPSCNNConvolution convolves the input image with a set of filters, each producing one feature map in the output image.
-- 
-- Phantom type for @MPSCNNConvolution@.
data MPSCNNConvolution

instance IsObjCObject (Id MPSCNNConvolution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolution"

class IsMPSCNNKernel a => IsMPSCNNConvolution a where
  toMPSCNNConvolution :: a -> Id MPSCNNConvolution

instance IsMPSCNNConvolution (Id MPSCNNConvolution) where
  toMPSCNNConvolution = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNConvolution) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNConvolution) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNConvolution) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTranspose ----------

-- | MPSCNNConvolutionTranspose
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionTranspose specifies a transposed convolution.              The MPSCNNConvolutionTranspose convolves the input image with a set of filters, each producing one feature map in the output image.
--
-- Some third-party frameworks may rotate the weights spatially by 180 degrees for Convolution Transpose. MPS uses the weights              specified by the developer as-is and does not perform any rotation. The developer may need to rotate the weights appropriately              in case this rotation is needed before the convolution transpose is applied.
--
-- When the stride in any dimension is greater than 1, the convolution transpose puts (stride - 1) zeroes in-between the source               image pixels to create an expanded image. Then a convolution is done over the expanded image to generate the output of the               convolution transpose.
--
-- Intermediate image size = (srcSize - 1) * Stride + 1
--
-- Examples:
--
-- So in case of sride == 2 (this behaves same in both dimensions)
--
-- Source image:
-- _______________
-- |   |   |   |   |
-- | 1 | 2 | 3 | 4 |
-- |   |   |   |   |
-- ---------------
--
-- Intermediate Image:
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   |   |   |   |
-- ---------------------------
--
-- NOTE on Offset:
-- There are 2 types of offsets defined:
-- 1) The Offset defined in MPSCNNKernel from which MPSCNNConvolutionTranspose inherits. This offset is applied to from where
-- the kernel will be applied on the source.
-- 2) The kernelOffsetX and kernelOffsetY which is the offset applied to the kernel when it is finally applied on the intermediate
-- image.
--
-- So totalOffset = Offset * stride + kernelOffset
--
-- The offset defined by user refers to the coordinate frame of the expanded image
-- (we are showing only 1 dimension X it can be extended to Y dimension as well) :
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = 0
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- | X |   |   |   |   |   |   |
-- ---------------------------
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = 1
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   | X |   |   |   |   |   |
-- ---------------------------
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = -1
-- ___________________________
-- |   |   |   |   |   |   |   |
-- X | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   |   |   |   |
-- ---------------------------
--
-- So if the user wanted to apply an offset of 2 on the source image of convolution transpose:
--
-- Source image:
-- _______________
-- |   |   |   |   |
-- | 1 | 2 | 3 | 4 |
-- |   |   | X |   |
-- ---------------
--
-- offset = 2, kernelOffset = 0
--
-- Intermediate Image:
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   | X |   |   |
-- ---------------------------
--
-- Note that if your application is not using MPSCNNConvolutionGradientState to configure the convolution transpose with respect to convolution,      your application may do this using padding policy. In such case if convolution uses valid padding policy, than convolution transpose should use      full padding policy and vice vera. Full padding remains full.
-- 
-- Phantom type for @MPSCNNConvolutionTranspose@.
data MPSCNNConvolutionTranspose

instance IsObjCObject (Id MPSCNNConvolutionTranspose) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTranspose"

class IsMPSCNNKernel a => IsMPSCNNConvolutionTranspose a where
  toMPSCNNConvolutionTranspose :: a -> Id MPSCNNConvolutionTranspose

instance IsMPSCNNConvolutionTranspose (Id MPSCNNConvolutionTranspose) where
  toMPSCNNConvolutionTranspose = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNConvolutionTranspose) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNConvolutionTranspose) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTranspose) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNCrossChannelNormalization ----------

-- | MPSCNNCrossChannelNormalization
--
-- This depends on Metal.framework
--
-- Specifies the normalization filter across feature channels.               This normalization filter applies the filter to a local region across nearby feature channels,              but with no spatial extent (i.e., they have shape kernelSize x 1 x 1).              The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,              where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where              N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
-- 
-- Phantom type for @MPSCNNCrossChannelNormalization@.
data MPSCNNCrossChannelNormalization

instance IsObjCObject (Id MPSCNNCrossChannelNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNCrossChannelNormalization"

class IsMPSCNNKernel a => IsMPSCNNCrossChannelNormalization a where
  toMPSCNNCrossChannelNormalization :: a -> Id MPSCNNCrossChannelNormalization

instance IsMPSCNNCrossChannelNormalization (Id MPSCNNCrossChannelNormalization) where
  toMPSCNNCrossChannelNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNCrossChannelNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNCrossChannelNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNCrossChannelNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDropout ----------

-- | MPSCNNDropout
--
-- This depends on Metal.framework
--
-- Dropout is a regularization technique used to prevent neural networks from              overfitting during training. With probability keepProbability, this filter              outputs the input element scaled by 1 / keepProbability. Otherwise, it              outputs 0. Each input element is kept or dropped independently. The scaling              is performed to keep the energy of the output unchanged.
-- 
-- Phantom type for @MPSCNNDropout@.
data MPSCNNDropout

instance IsObjCObject (Id MPSCNNDropout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDropout"

class IsMPSCNNKernel a => IsMPSCNNDropout a where
  toMPSCNNDropout :: a -> Id MPSCNNDropout

instance IsMPSCNNDropout (Id MPSCNNDropout) where
  toMPSCNNDropout = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNDropout) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNDropout) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNDropout) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGroupNormalization ----------

-- | MPSCNNGroupNormalization
--
-- This depends on Metal.framework
--
-- This kernel normalizes each image, on a per-group basis, to              have zero mean and unit variance:
--
-- for each image:                  for each channel:                      y = (x - mean) * gamma / sqrt(variance + epsilon) + beta;
--
-- The mean and variance are computed per group of channels, as given by the dataSource.
-- 
-- Phantom type for @MPSCNNGroupNormalization@.
data MPSCNNGroupNormalization

instance IsObjCObject (Id MPSCNNGroupNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGroupNormalization"

class IsMPSCNNKernel a => IsMPSCNNGroupNormalization a where
  toMPSCNNGroupNormalization :: a -> Id MPSCNNGroupNormalization

instance IsMPSCNNGroupNormalization (Id MPSCNNGroupNormalization) where
  toMPSCNNGroupNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNGroupNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNGroupNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNGroupNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNInstanceNormalization ----------

-- | MPSCNNInstanceNormalization
--
-- This depends on Metal.framework
--
-- This kernel normalizes each image, on a per-channel basis, to              have zero mean and unit variance:
--
-- for each image:                  for each channel:                      y = (x - mean) * gamma / sqrt(variance + epsilon) + beta;
-- 
-- Phantom type for @MPSCNNInstanceNormalization@.
data MPSCNNInstanceNormalization

instance IsObjCObject (Id MPSCNNInstanceNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNInstanceNormalization"

class IsMPSCNNKernel a => IsMPSCNNInstanceNormalization a where
  toMPSCNNInstanceNormalization :: a -> Id MPSCNNInstanceNormalization

instance IsMPSCNNInstanceNormalization (Id MPSCNNInstanceNormalization) where
  toMPSCNNInstanceNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNInstanceNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNInstanceNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNInstanceNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLocalContrastNormalization ----------

-- | MPSCNNLocalContrastNormalization
--
-- This depends on Metal.framework
--
-- Specifies the local contrast normalization filter.              The local contrast normalization is quite similar to spatial normalization              (see MPSCNNSpatialNormalization) in that it applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight),              but instead of dividing by the local "energy" of the feature, the denominator uses the local variance              of the feature - effectively the mean value of the feature is subtracted from the signal.              For each feature channel, the function computes the variance VAR(i,j) and              mean M(i,j) of X(i,j) inside each rectangle around the spatial point (i,j).
--
-- Then the result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / (delta + alpha * VAR(i,j))^beta,
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. For example setting              pm=0, ps=1, p0=1, delta=0, alpha=1.0 and beta=0.5 scales input data so that the result has              unit variance and zero mean, provided that input variance is positive.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined. A good way to guard              against tiny variances is to regulate the expression with a small value for delta, for example              delta = 1/1024 = 0.0009765625.
-- 
-- Phantom type for @MPSCNNLocalContrastNormalization@.
data MPSCNNLocalContrastNormalization

instance IsObjCObject (Id MPSCNNLocalContrastNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLocalContrastNormalization"

class IsMPSCNNKernel a => IsMPSCNNLocalContrastNormalization a where
  toMPSCNNLocalContrastNormalization :: a -> Id MPSCNNLocalContrastNormalization

instance IsMPSCNNLocalContrastNormalization (Id MPSCNNLocalContrastNormalization) where
  toMPSCNNLocalContrastNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNLocalContrastNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNLocalContrastNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNLocalContrastNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLogSoftMax ----------

-- | MPSCNNLogSoftMax
--
-- This depends on Metal.framework
--
-- The logarithmic softMax filter can be achieved by taking the natural logarithm of the              the result of the softMax filter. The results are often used to construct a loss function to be              minimized when training neural networks.              For each feature channel per pixel in an image in a feature map, the logarithmic softMax filter              computes the following:                  result channel in pixel = pixel(x,y,k)) - ln{sum(exp(pixel(x,y,0)) ... exp(pixel(x,y,N-1))}                      where N is the number of feature channels and y = ln{x} satisfies e^y = x.
-- 
-- Phantom type for @MPSCNNLogSoftMax@.
data MPSCNNLogSoftMax

instance IsObjCObject (Id MPSCNNLogSoftMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLogSoftMax"

class IsMPSCNNKernel a => IsMPSCNNLogSoftMax a where
  toMPSCNNLogSoftMax :: a -> Id MPSCNNLogSoftMax

instance IsMPSCNNLogSoftMax (Id MPSCNNLogSoftMax) where
  toMPSCNNLogSoftMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNLogSoftMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNLogSoftMax) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNLogSoftMax) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLoss ----------

-- | MPSCNNLoss
--
-- This depends on Metal.framework.
--
-- The MPSCNNLoss filter is only used for training. This filter performs both the forward and              backward pass computations. Specifically, it computes the loss between the input (predictions)              and target data (labels) and the loss gradient. The loss value can be a 1 x 1 x 1 image containing              a scalar loss value or an image (of the same size as the input source image) with per feature              channel losses. The loss value is used to determine whether to continue the training operation or              to terminate it, once satisfactory results are achieved. The loss gradient is the first gradient              computed for the backward pass and serves as input to the next gradient filter (in the backward              direction).
--
-- The MPSCNNLoss filter is created with a MPSCNNLossDescriptor describing the type of a loss filter              and the type of a reduction to use for computing the overall loss.
--
-- The MPSCNNLoss filter takes the output of the inference pass (predictions) as input. It also              requires the target data (labels) and optionally, weights for the labels. If per-label weights              are not supplied, there is an option to use a single weight value by setting the 'weight' properly              on the MPSCNNLossDescriptor object. The labels and optional weights need to be supplied by the user              using the MPSCNNLossLabels object. The labels and weights are described via the MPSCNNLossDataDescriptor              objects, which are in turn used to initialize the MPSCNNLossLabels object.
--
-- If the specified reduction operation is MPSCNNReductionTypeNone, the destinationImage should be              at least as large as the specified clipRect. The destinationImage will then contain per-element              losses. Otherse, a reduction operation will be performed, according to the specified reduction              type, and the filter will return a scalar value containing the overall loss. For more information              on the available reduction types, see MPSCNNTypes.h. Also see MPSCNNLossDescriptor for the              description of optional parameters.
--
-- Here is a code example:
--
-- // Setup              MPSCNNLossDataDescriptor* labelsDescriptor =                  [MPSCNNLossDataDescriptor cnnLossDataDescriptorWithData: labelsData                                                                   layout: MPSDataLayoutHeightxWidthxFeatureChannels                                                                     size: labelsDataSize];              MPSCNNLossLabels* labels = [[MPSCNNLossLabels alloc] initWithDevice: device                                                                 labelsDescriptor: labelsDescriptor];              MPSCNNLossDescriptor *lossDescriptor =                  [MPSCNNLossDescriptor cnnLossDescriptorWithType: (MPSCNNLossType)MPSCNNLossTypeMeanAbsoluteError                                                    reductionType: (MPSCNNReductionType)MPSCNNReductionTypeSum];              MPSCNNLoss* lossFilter = [[MPSCNNLoss alloc] initWithDevice: device lossDescriptor: lossDescriptor];
--
-- // Encode loss filter.              // The sourceImage is the output of a previous layer, for example, the SoftMax layer. The lossGradientsImage              // is the sourceGradient input image to the first gradient layer (in the backward direction), for example,              // the SoftMax gradient filter.              [lossFilter encodeToCommandBuffer: commandBuffer sourceImage: sourceImage                                                                    labels: labels                                                          destinationImage: lossGradientsImage];
--
-- // In order to guarantee that the loss image data is correctly synchronized for CPU side access,              // it is the application's responsibility to call the [labels synchronizeOnCommandBuffer:]              // method before accessing the loss image data.              [labels synchronizeOnCommandBuffer:commandBuffer];              MPSImage* lossImage = [labels lossImage];
--
-- For predictions (y) and labels (t), the available loss filter types are the following:
--
-- Mean Absolute Error loss filter. This filter measures the absolute error of the element-wise              difference between the predictions and labels.              This loss function is computed according to the following formulas:                  Compute losses:          losses = |y - t|                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Mean Squared Error loss filter. This filter measures the squared error of the element-wise              difference between the predictions and labels.              This loss function is computed according to the following formulas:                  Compute losses:          losses = (y - t)^2                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- SoftMax Cross Entropy loss filter. This loss filter is applied element-wise.              This loss filter combines the LogSoftMax and Negative Log Likelihood operations in a              single filter. It is useful for training a classification problem with multiple classes.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -t * LogSoftMax(y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)                                           If reductionType is MPSCNNReductionTypeMean, the accumulated                                           loss value is divided by width * height instead of                                           width * height * featureChannels.
--
-- Sigmoid Cross Entropy loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = max(y, 0) - y * t + log(1 + exp(-|y|))                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Categorical Cross Entropy loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -t * log(y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Hinge loss filter. This loss filter is applied element-wise.              The labels are expected to be 0.0 or 1.0.              This loss function is computed according to the following formulas:                  Compute losses:          losses = max(1 - (t * y), 0.0f)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Huber loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          if (|y - t| <= delta, losses = 0.5 * y^2                                           if (|y - t| >  delta, losses = 0.5 * delta^2 + delta * (|y - t| - delta)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Cosine Distance loss filter. This loss filter is applied element-wise.              The only valid reduction type for this loss filter is MPSCNNReductionTypeSum.              This loss function is computed according to the following formulas:                  Compute losses:          loss = 1 - reduce_sum(y * t)                  Compute overall loss:    weighted_loss = weight * loss
--
-- Log loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -(t * log(y + epsilon)) - ((1 - t) * log(1 - y + epsilon))                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Kullback-Leibler Divergence loss filter. This loss filter is applied element-wise.              The input (predictions) is expected to contain log-probabilities.                  This loss function is computed according to the following formulas:                  Compute losses:          losses = t * (log(t) - y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- For predictions (y) and labels (t), the loss gradient for each available loss filter type              is computed as follows:
--
-- Mean Absolute Error loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = (y - t) / |y - t|                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Mean Squared Error loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = 2 * (y - t)                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- SoftMax Cross Entropy loss.              The loss gradient is computed according to the following formulas:                  First, apply the same label smoothing as in the MPSCNNLoss filter.                  Compute gradient:          d/dy = y - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Sigmoid Cross Entropy loss.              The loss gradient is computed according to the following formulas:              First, apply the same label smoothing as in the MPSCNNLoss filter.                  Compute gradient:          d/dy = (1 / (1 + exp(-y)) - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Categorical Cross Entropy loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t / y                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Hinge loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = ((1 + ((1 - (2 * t)) * y)) > 0) ? 1 - (2 * t) : 0                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Huber loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = |y - t| > delta ? delta : y - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Cosine Distance loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Log loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = (-2 * epsilon * t - t + y + epsilon) / (y * (1 - y) + epsilon * (epsilon + 1))                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Kullback-Leibler Divergence loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t / y                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- The number of output feature channels remains the same as the number of input feature              channels.
-- 
-- Phantom type for @MPSCNNLoss@.
data MPSCNNLoss

instance IsObjCObject (Id MPSCNNLoss) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLoss"

class IsMPSCNNKernel a => IsMPSCNNLoss a where
  toMPSCNNLoss :: a -> Id MPSCNNLoss

instance IsMPSCNNLoss (Id MPSCNNLoss) where
  toMPSCNNLoss = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNLoss) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNLoss) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNLoss) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuron ----------

-- | MPSCNNNeuron
--
-- This depends on Metal.framework
--
-- This filter applies a neuron activation function.              You must use one of the sub-classes of MPSCNNNeuron.
--
-- The following filter types are supported:  MPSCNNNeuronTypeNone            ///< f(x) = x  MPSCNNNeuronTypeLinear          ///< f(x) = a * x + b  MPSCNNNeuronTypeReLU            ///< f(x) = x >= 0 ? x : a * x  MPSCNNNeuronTypeSigmoid         ///< f(x) = 1 / (1 + e^-x)  MPSCNNNeuronTypeHardSigmoid     ///< f(x) = clamp((x * a) + b, 0, 1)  MPSCNNNeuronTypeTanH            ///< f(x) = a * tanh(b * x)  MPSCNNNeuronTypeAbsolute        ///< f(x) = fabs(x)  MPSCNNNeuronTypeSoftPlus        ///< f(x) = a * log(1 + e^(b * x))  MPSCNNNeuronTypeSoftSign        ///< f(x) = x / (1 + abs(x))  MPSCNNNeuronTypeELU             ///< f(x) = x >= 0 ? x : a * (exp(x) - 1)  MPSCNNNeuronTypePReLU           ///< Same as ReLU except parameter a is per channel  MPSCNNNeuronTypeReLUN           ///< f(x) = min((x >= 0 ? x : a * x), b)  MPSCNNNeuronTypePower           ///< f(x) = (a * x + b) ^ c  MPSCNNNeuronTypeExponential     ///< f(x) = c ^ (a * x + b)  MPSCNNNeuronTypeLogarithm       ///< f(x) = log_c(a * x + b)  MPSCNNNeuronTypeGeLU            ///< f(x) = (1.0 + erf(x * sqrt(0.5))) * 0.5 * x
-- 
-- Phantom type for @MPSCNNNeuron@.
data MPSCNNNeuron

instance IsObjCObject (Id MPSCNNNeuron) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuron"

class IsMPSCNNKernel a => IsMPSCNNNeuron a where
  toMPSCNNNeuron :: a -> Id MPSCNNNeuron

instance IsMPSCNNNeuron (Id MPSCNNNeuron) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuron) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuron) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuron) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPooling ----------

-- | MPSCNNPooling
--
-- This depends on Metal.framework
--
-- Pooling is a form of non-linear sub-sampling. Pooling partitions the input image into a set of              rectangles (overlapping or non-overlapping) and, for each such sub-region, outputs a value.              The pooling operation is used in computer vision to reduce the dimensionality of intermediate representations.
-- 
-- Phantom type for @MPSCNNPooling@.
data MPSCNNPooling

instance IsObjCObject (Id MPSCNNPooling) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPooling"

class IsMPSCNNKernel a => IsMPSCNNPooling a where
  toMPSCNNPooling :: a -> Id MPSCNNPooling

instance IsMPSCNNPooling (Id MPSCNNPooling) where
  toMPSCNNPooling = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNPooling) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNPooling) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPooling) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSoftMax ----------

-- | MPSCNNSoftMax
--
-- This depends on Metal.framework
--
-- The softMax filter is a neural transfer function and is useful for classification tasks.              The softMax filter is applied across feature channels and in a convolutional manner at all              spatial locations. The softMax filter can be seen as the combination of an              activation function (exponential) and a normalization operator.              For each feature channel per pixel in an image in a feature map, the softMax filter computes the following:                  result channel in pixel = exp(pixel(x,y,k))/sum(exp(pixel(x,y,0)) ... exp(pixel(x,y,N-1))                      where N is the number of feature channels
-- 
-- Phantom type for @MPSCNNSoftMax@.
data MPSCNNSoftMax

instance IsObjCObject (Id MPSCNNSoftMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSoftMax"

class IsMPSCNNKernel a => IsMPSCNNSoftMax a where
  toMPSCNNSoftMax :: a -> Id MPSCNNSoftMax

instance IsMPSCNNSoftMax (Id MPSCNNSoftMax) where
  toMPSCNNSoftMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNSoftMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSoftMax) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSoftMax) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSpatialNormalization ----------

-- | MPSCNNSpatialNormalization
--
-- This depends on Metal.framework
--
-- Specifies the spatial normalization filter.              The spatial normalization for a feature channel applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight).              For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).              It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,              where kw and kh are the kernelWidth and the kernelHeight.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
-- 
-- Phantom type for @MPSCNNSpatialNormalization@.
data MPSCNNSpatialNormalization

instance IsObjCObject (Id MPSCNNSpatialNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSpatialNormalization"

class IsMPSCNNKernel a => IsMPSCNNSpatialNormalization a where
  toMPSCNNSpatialNormalization :: a -> Id MPSCNNSpatialNormalization

instance IsMPSCNNSpatialNormalization (Id MPSCNNSpatialNormalization) where
  toMPSCNNSpatialNormalization = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNSpatialNormalization) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSpatialNormalization) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSpatialNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsampling ----------

-- | MPSCNNUpsampling
--
-- This depends on Metal.framework
--
-- The MPSCNNUpsampling filter can be used to resample an existing MPSImage              using a different sampling frequency for the x and y dimensions with the purpose of              enlarging the size of an image.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- The scaleFactor must be an integer value >= 1. The default value is 1.              If scaleFactor == 1, the filter acts as a copy kernel.
--
-- Nearest and bilinear variants are supported.
-- 
-- Phantom type for @MPSCNNUpsampling@.
data MPSCNNUpsampling

instance IsObjCObject (Id MPSCNNUpsampling) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsampling"

class IsMPSCNNKernel a => IsMPSCNNUpsampling a where
  toMPSCNNUpsampling :: a -> Id MPSCNNUpsampling

instance IsMPSCNNUpsampling (Id MPSCNNUpsampling) where
  toMPSCNNUpsampling = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNUpsampling) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsampling) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsampling) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNYOLOLoss ----------

-- | Phantom type for @MPSCNNYOLOLoss@.
data MPSCNNYOLOLoss

instance IsObjCObject (Id MPSCNNYOLOLoss) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNYOLOLoss"

class IsMPSCNNKernel a => IsMPSCNNYOLOLoss a where
  toMPSCNNYOLOLoss :: a -> Id MPSCNNYOLOLoss

instance IsMPSCNNYOLOLoss (Id MPSCNNYOLOLoss) where
  toMPSCNNYOLOLoss = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNYOLOLoss) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNYOLOLoss) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNYOLOLoss) where
  toNSObject = unsafeCastId

-- ---------- MPSNNCropAndResizeBilinear ----------

-- | MPSNNCropAndResizeBilinear
--
-- This depends on Metal.framework
--
-- The MPSNNCropAndResizeBilinear filter resizes the source image  using bilinear interpolation to              a destination whose dimensions are given by resizeWidth and resizeHeight
--
-- The number of output feature channels remains the same as the number of input feature              channels.
-- 
-- Phantom type for @MPSNNCropAndResizeBilinear@.
data MPSNNCropAndResizeBilinear

instance IsObjCObject (Id MPSNNCropAndResizeBilinear) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNCropAndResizeBilinear"

class IsMPSCNNKernel a => IsMPSNNCropAndResizeBilinear a where
  toMPSNNCropAndResizeBilinear :: a -> Id MPSNNCropAndResizeBilinear

instance IsMPSNNCropAndResizeBilinear (Id MPSNNCropAndResizeBilinear) where
  toMPSNNCropAndResizeBilinear = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNCropAndResizeBilinear) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNCropAndResizeBilinear) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNCropAndResizeBilinear) where
  toNSObject = unsafeCastId

-- ---------- MPSNNForwardLoss ----------

-- | MPSNNForwardLoss
--
-- This depends on Metal.framework.
--
-- The MPSNNForwardLoss filter specifies a version of the loss filter which separates the forward              computation from the gradient computation. In order to compute gradients for the loss filter              use MPSNNLossGradient filter and in order to start the gradient computation of an arbitrary              image use the MPSNNInitialGradient filter.              NOTE: This filter does not support non-default offset or cliprects and setting them to other              than default values will result in undefined results.
-- 
-- Phantom type for @MPSNNForwardLoss@.
data MPSNNForwardLoss

instance IsObjCObject (Id MPSNNForwardLoss) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNForwardLoss"

class IsMPSCNNKernel a => IsMPSNNForwardLoss a where
  toMPSNNForwardLoss :: a -> Id MPSNNForwardLoss

instance IsMPSNNForwardLoss (Id MPSNNForwardLoss) where
  toMPSNNForwardLoss = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNForwardLoss) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNForwardLoss) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNForwardLoss) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGramMatrixCalculation ----------

-- | MPSNNGramMatrixCalculation
--
-- This depends on Metal.framework
--
-- The MPSNNGramMatrixCalculation filter specifies a layer which computes the uncentered cross-correlation              values between the image planes of each feature channel of an image. If the input image batch is              x = x[b, y, x, c], where 'b' is batch index, 'y' and 'x' are the image coordinate and              'c' is the feature channel index then this filter computes the values:
--
-- y = y[b, 1, f, c] = alpha * sum_{x,y} x[b,y,x,f] * x[b,y,x,c], where
--
-- 'alpha' is a scaling factor. This operation can be interpreted to be computing all combinations              of fully connected layers between the different image planes of the input image. The results              are stored in the feature channel and 'x'-coordinate indices of the output batch.              The operation is performed independently on different images in the batch.
--
-- NOTE: Due to the nature of the operation this filter specifies a special padding policy              and hence does not support non-default offset or cliprect properties.
-- 
-- Phantom type for @MPSNNGramMatrixCalculation@.
data MPSNNGramMatrixCalculation

instance IsObjCObject (Id MPSNNGramMatrixCalculation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGramMatrixCalculation"

class IsMPSCNNKernel a => IsMPSNNGramMatrixCalculation a where
  toMPSNNGramMatrixCalculation :: a -> Id MPSNNGramMatrixCalculation

instance IsMPSNNGramMatrixCalculation (Id MPSNNGramMatrixCalculation) where
  toMPSNNGramMatrixCalculation = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNGramMatrixCalculation) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNGramMatrixCalculation) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNGramMatrixCalculation) where
  toNSObject = unsafeCastId

-- ---------- MPSNNInitialGradient ----------

-- | MPSNNInitialGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNInitialGradient filter specifies a layer which computes the initial gradient for              an aribitrary input image. The operation itself is extremely simple: it computes the gradient of the input image              with itself, resulting in an output image which is filled with '1's for all the inputs that were used.              This serves as the starting point for the computation of gradients between arbitrary images in a network.              Example:                  Suppose that we want to compute gradients for a function that multiplies together two losses:                      f = f(L1, L2) = L1 * L2                  The losses themselves are computed from some inputs x1,x2:                      L1 = L1(x1),                      L2 = L2(x2)                  The filters to compute f, L1, L2 are:                      f = MPSCNNMultiply(L1, L2), where                      L1 = MPSNNForwardLoss1(x1) and                      L2 = MPSNNForwardLoss1(x2)
--
-- To compute df/dx1 we apply the chain rule:
--
-- df/dx1 = d(L1 * L2)/dx1 = d(L1 * L2)/dL1 * dL1/dx1 + d(L1 * L2)/dL2 * dL2/dx1                             = d(L1 * L2)/dL1 * dL1/dx1 = L2 * dL1/dx1
--
-- The MPSCNNMultiplyGradient filter computes for f = L1 * L2 forward op:                      dL/dL1 = dL/df * df/dL1 = dL/df * L2 and                      dL/dL2 = dL/df * df/dL2 = dL/df * L1 where                  dL/df is the input gradient of the chain rule / backpropagation algorithm.                  But in our case we want MPSCNNMultiplyGradient to compute the gradient:                      df/dL1 = d(L1 * L2)/dL1 = L2,                  which shows that                      L = f, which means that dL/dL1 = df/df * df/dL1 = 1 * L2, which                  shows that we get the correct gradient by providing unit input as input gradient to                  the MPSCNNMultiplyGradient.
-- 
-- Phantom type for @MPSNNInitialGradient@.
data MPSNNInitialGradient

instance IsObjCObject (Id MPSNNInitialGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNInitialGradient"

class IsMPSCNNKernel a => IsMPSNNInitialGradient a where
  toMPSNNInitialGradient :: a -> Id MPSNNInitialGradient

instance IsMPSNNInitialGradient (Id MPSNNInitialGradient) where
  toMPSNNInitialGradient = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNInitialGradient) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNInitialGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNInitialGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNPad ----------

-- | Phantom type for @MPSNNPad@.
data MPSNNPad

instance IsObjCObject (Id MPSNNPad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNPad"

class IsMPSCNNKernel a => IsMPSNNPad a where
  toMPSNNPad :: a -> Id MPSNNPad

instance IsMPSNNPad (Id MPSNNPad) where
  toMPSNNPad = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNPad) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNPad) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNPad) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceUnary ----------

-- | MPSNNReduceUnary
--
-- The MPSNNReduce performs a reduction operation              The reduction operations supported are:                   - Reduce row min                   - Reduce column min                   - Reduce feature channels min                   - Reduce row max                   - Reduce column max                   - Reduce feature channels max                   - Reduce row mean                   - Reduce column mean                   - Reduce feature channels mean                   - Reduce row sum                   - Reduce column sum                   - Reduce feature channels sum
-- 
-- Phantom type for @MPSNNReduceUnary@.
data MPSNNReduceUnary

instance IsObjCObject (Id MPSNNReduceUnary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceUnary"

class IsMPSCNNKernel a => IsMPSNNReduceUnary a where
  toMPSNNReduceUnary :: a -> Id MPSNNReduceUnary

instance IsMPSNNReduceUnary (Id MPSNNReduceUnary) where
  toMPSNNReduceUnary = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceUnary) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceUnary) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNReduceUnary) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReshape ----------

-- | Phantom type for @MPSNNReshape@.
data MPSNNReshape

instance IsObjCObject (Id MPSNNReshape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReshape"

class IsMPSCNNKernel a => IsMPSNNReshape a where
  toMPSNNReshape :: a -> Id MPSNNReshape

instance IsMPSNNReshape (Id MPSNNReshape) where
  toMPSNNReshape = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReshape) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReshape) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNReshape) where
  toNSObject = unsafeCastId

-- ---------- MPSNNResizeBilinear ----------

-- | MPSNNResizeBilinear
--
-- This depends on Metal.framework
--
-- The MPSNNResizeBilinear filter resizes the source image  using bilinear interpolation to              a destination whose dimensions are given by resizeWidth and resizeHeight
--
-- The number of output feature channels remains the same as the number of input feature              channels.
-- 
-- Phantom type for @MPSNNResizeBilinear@.
data MPSNNResizeBilinear

instance IsObjCObject (Id MPSNNResizeBilinear) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNResizeBilinear"

class IsMPSCNNKernel a => IsMPSNNResizeBilinear a where
  toMPSNNResizeBilinear :: a -> Id MPSNNResizeBilinear

instance IsMPSNNResizeBilinear (Id MPSNNResizeBilinear) where
  toMPSNNResizeBilinear = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNResizeBilinear) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNResizeBilinear) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNResizeBilinear) where
  toNSObject = unsafeCastId

-- ---------- MPSNNSlice ----------

-- | Phantom type for @MPSNNSlice@.
data MPSNNSlice

instance IsObjCObject (Id MPSNNSlice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNSlice"

class IsMPSCNNKernel a => IsMPSNNSlice a where
  toMPSNNSlice :: a -> Id MPSNNSlice

instance IsMPSNNSlice (Id MPSNNSlice) where
  toMPSNNSlice = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNSlice) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNSlice) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNSlice) where
  toNSObject = unsafeCastId

-- ---------- MPSRNNImageInferenceLayer ----------

-- | MPSRNNImageInferenceLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNImageInferenceLayer specifies a recurrent neural network layer for inference on MPSImages.              Currently two types of recurrent layers are supported: ones that operate with convolutions on              images: MPSRNNImageInferenceLayer and one that operates on matrices: MPSRNNMatrixInferenceLayer.              The former can be often used to implement the latter by using 1x1-images, but due to              image size restrictions and performance, it is advisable to use MPSRNNMatrixInferenceLayer for              linear recurrent layers.              A MPSRNNImageInferenceLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer, or an array of MPSRNNLayerDescriptors, which specifies a stack              of recurrent layers, that can operate in parallel a subset of the inputs in a sequence of inputs and              recurrent outputs. Note that currently stacks with bidirectionally traversing encode functions do not support starting              from a previous set of recurrent states, but this can be achieved quite easily by defining two separate              unidirectional stacks of layers, and running the same input sequence on them separately (one forwards and one backwards)              and ultimately combining the two result sequences as desired with auxiliary functions.
-- 
-- Phantom type for @MPSRNNImageInferenceLayer@.
data MPSRNNImageInferenceLayer

instance IsObjCObject (Id MPSRNNImageInferenceLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSRNNImageInferenceLayer"

class IsMPSCNNKernel a => IsMPSRNNImageInferenceLayer a where
  toMPSRNNImageInferenceLayer :: a -> Id MPSRNNImageInferenceLayer

instance IsMPSRNNImageInferenceLayer (Id MPSRNNImageInferenceLayer) where
  toMPSRNNImageInferenceLayer = unsafeCastId

instance IsMPSCNNKernel (Id MPSRNNImageInferenceLayer) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSRNNImageInferenceLayer) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSRNNImageInferenceLayer) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixBatchNormalizationGradient ----------

-- | MPSMatrixBatchNormalizationGradient
--
-- This depends on Metal.framework.
--
-- A kernel to compute the gradient of the batch normalization operation.
--
-- A MPSMatrixBatchNormalizationGradient object computes the results of backpropagating              the gradients of a loss function with respect to the outputs of an              MPSMatrixBatchNormalization object.  The corresponding properties and data used by              the MPSMatrixBatchNormalizationGradient object should correspond to those used by              the forward MPSMatrixBatchNormalization object for which the gradient is being computed.
-- 
-- Phantom type for @MPSMatrixBatchNormalizationGradient@.
data MPSMatrixBatchNormalizationGradient

instance IsObjCObject (Id MPSMatrixBatchNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixBatchNormalizationGradient"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixBatchNormalizationGradient a where
  toMPSMatrixBatchNormalizationGradient :: a -> Id MPSMatrixBatchNormalizationGradient

instance IsMPSMatrixBatchNormalizationGradient (Id MPSMatrixBatchNormalizationGradient) where
  toMPSMatrixBatchNormalizationGradient = unsafeCastId

instance IsMPSKernel (Id MPSMatrixBatchNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixBatchNormalizationGradient) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixBatchNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixFullyConnected ----------

-- | MPSMatrixFullyConnected
--
-- This depends on Metal.framework.
--
-- Applies a fully connected neural network layer by performing a              a matrix multiplication, adding a bias vector, scaling, and applying a              neuron activation function.
--
-- A MPSMatrixFullyConnected object computes:
--
-- y = neuron(alpha * x * W + bias)
--
-- y is the output matrix, x and W are input matrices corresponding              to a collection of input vectors and weights respectively, and bias              is a vector which is broadcast and accumulated to each row              of the product.  alpha is a scale factor applied to the product.
--
-- neuron() is a pointwise function applied to the intermediate result.
-- 
-- Phantom type for @MPSMatrixFullyConnected@.
data MPSMatrixFullyConnected

instance IsObjCObject (Id MPSMatrixFullyConnected) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixFullyConnected"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixFullyConnected a where
  toMPSMatrixFullyConnected :: a -> Id MPSMatrixFullyConnected

instance IsMPSMatrixFullyConnected (Id MPSMatrixFullyConnected) where
  toMPSMatrixFullyConnected = unsafeCastId

instance IsMPSKernel (Id MPSMatrixFullyConnected) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixFullyConnected) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixFullyConnected) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixFullyConnectedGradient ----------

-- | MPSMatrixFullyConnectedGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient of the fully connected layer with respect              to either the weights and bias terms or the input feature vectors.
--
-- An MPSMatrixFullyConnectedGradient kernel may be used to compute              the gradients corresponding to a MPSMatrixFullyConnected kernel.              The properties, input, and weight data must match those values              used in the forward computation.              This kernel does not compute the gradient of any non-identity              activation function which may have been applied in the forward              kernel.  Such a kernel must be expressed using both MPSMatrixFullyConnected              and MPSMatrixNeuron if a gradient is to be computed.
-- 
-- Phantom type for @MPSMatrixFullyConnectedGradient@.
data MPSMatrixFullyConnectedGradient

instance IsObjCObject (Id MPSMatrixFullyConnectedGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixFullyConnectedGradient"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixFullyConnectedGradient a where
  toMPSMatrixFullyConnectedGradient :: a -> Id MPSMatrixFullyConnectedGradient

instance IsMPSMatrixFullyConnectedGradient (Id MPSMatrixFullyConnectedGradient) where
  toMPSMatrixFullyConnectedGradient = unsafeCastId

instance IsMPSKernel (Id MPSMatrixFullyConnectedGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixFullyConnectedGradient) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixFullyConnectedGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixNeuronGradient ----------

-- | MPSMatrixNeuronGradient
--
-- This depends on Metal.framework.
--
-- A neuron gradient activation kernel that operates on matrices.
--
-- A MPSMatrixNeuronGradient object computes the results of backpropagating              the gradients of a loss function with respect to the outputs of an              MPSMatrixNeuron object.  The corresponding properties and data used by              the MPSMatrixNeuronGradient object should correspond to those used by              the forward MPSMatrixNeuron object for which the gradient is being computed.
-- 
-- Phantom type for @MPSMatrixNeuronGradient@.
data MPSMatrixNeuronGradient

instance IsObjCObject (Id MPSMatrixNeuronGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixNeuronGradient"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixNeuronGradient a where
  toMPSMatrixNeuronGradient :: a -> Id MPSMatrixNeuronGradient

instance IsMPSMatrixNeuronGradient (Id MPSMatrixNeuronGradient) where
  toMPSMatrixNeuronGradient = unsafeCastId

instance IsMPSKernel (Id MPSMatrixNeuronGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixNeuronGradient) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixNeuronGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSoftMaxGradient ----------

-- | MPSMatrixSoftMaxGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient corresponding to a forward MPSMatrixSoftMax object.
--
-- A MPSMatrixSoftMaxGradient object computes:
--
-- dL_dX_ij = Y_ij * (dL_dY_ij - sum_k(dL_dY_ik * Y_ik)
--
-- Where dL_dX is the resulting gradient of the loss function with respect to              the original input to the forward MPSMatrixSoftMax operation, Y is              the output of the forward MPSMatrixSoftMax operation, and dL_dY is the              gradient of the loss function with respect to Y.
-- 
-- Phantom type for @MPSMatrixSoftMaxGradient@.
data MPSMatrixSoftMaxGradient

instance IsObjCObject (Id MPSMatrixSoftMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSoftMaxGradient"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixSoftMaxGradient a where
  toMPSMatrixSoftMaxGradient :: a -> Id MPSMatrixSoftMaxGradient

instance IsMPSMatrixSoftMaxGradient (Id MPSMatrixSoftMaxGradient) where
  toMPSMatrixSoftMaxGradient = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSoftMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixSoftMaxGradient) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSoftMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSolveCholesky ----------

-- | MPSMatrixSolveCholesky
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of equations              using the Cholesky factorization resulting from a              MPSMatrixDecompositionCholesky kernel.
--
-- A MPSMatrixSolveCholesky finds the solution matrix to the system:
--
-- A * X = B
--
-- Where A is symmetric positive definite.  B is the array of              right hand sides for which the equations are to be solved.              X is the resulting matrix of solutions.
-- 
-- Phantom type for @MPSMatrixSolveCholesky@.
data MPSMatrixSolveCholesky

instance IsObjCObject (Id MPSMatrixSolveCholesky) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSolveCholesky"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixSolveCholesky a where
  toMPSMatrixSolveCholesky :: a -> Id MPSMatrixSolveCholesky

instance IsMPSMatrixSolveCholesky (Id MPSMatrixSolveCholesky) where
  toMPSMatrixSolveCholesky = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSolveCholesky) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixSolveCholesky) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSolveCholesky) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSolveLU ----------

-- | MPSMatrixSolveLU
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of equations              using the LU factorization resulting from a MPSMatrixDecompositionLU              kernel.
--
-- A MPSMatrixSolveLU finds the solution matrix to the system:
--
-- op(A) * X = B
--
-- Where op(A) is A**T or A.  B is the array of right hand sides for which              the equations are to be solved.  X is the resulting matrix of solutions.
-- 
-- Phantom type for @MPSMatrixSolveLU@.
data MPSMatrixSolveLU

instance IsObjCObject (Id MPSMatrixSolveLU) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSolveLU"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixSolveLU a where
  toMPSMatrixSolveLU :: a -> Id MPSMatrixSolveLU

instance IsMPSMatrixSolveLU (Id MPSMatrixSolveLU) where
  toMPSMatrixSolveLU = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSolveLU) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixSolveLU) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSolveLU) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSolveTriangular ----------

-- | MPSMatrixSolveTriangular
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of              equations using a triangular coefficient matrix.
--
-- A MPSMatrixSolveTriangular finds the solution matrix to the              triangular system:
--
-- op(A) * X = alpha * B    or    X * op(A) = alpha * B
--
-- Where A is either upper or lower triangular and op(A) is A**T              or A.  B is the array of right hand sides for which the              equations are to be solved.  X is the resulting matrix of              solutions.
-- 
-- Phantom type for @MPSMatrixSolveTriangular@.
data MPSMatrixSolveTriangular

instance IsObjCObject (Id MPSMatrixSolveTriangular) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSolveTriangular"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixSolveTriangular a where
  toMPSMatrixSolveTriangular :: a -> Id MPSMatrixSolveTriangular

instance IsMPSMatrixSolveTriangular (Id MPSMatrixSolveTriangular) where
  toMPSMatrixSolveTriangular = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSolveTriangular) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixSolveTriangular) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSolveTriangular) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixVectorMultiplication ----------

-- | MPSMatrixVectorMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix-vector multiplication kernel.
--
-- A MPSMatrixVectorMultiplication object computes:
--
-- y = alpha * op(A) * x + beta * y
--
-- A is a matrix represented by a MPSMatrix object. alpha and beta              are scalar values (of the same data type as values of y) which are              applied as shown above.  A may have an optional transposition              operation applied.
--
-- A MPSMatrixVectorMultiplication object is initialized with the transpose              operator to apply to A, sizes for the operation to perform,              and the scalar values alpha and beta.
-- 
-- Phantom type for @MPSMatrixVectorMultiplication@.
data MPSMatrixVectorMultiplication

instance IsObjCObject (Id MPSMatrixVectorMultiplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixVectorMultiplication"

class IsMPSMatrixBinaryKernel a => IsMPSMatrixVectorMultiplication a where
  toMPSMatrixVectorMultiplication :: a -> Id MPSMatrixVectorMultiplication

instance IsMPSMatrixVectorMultiplication (Id MPSMatrixVectorMultiplication) where
  toMPSMatrixVectorMultiplication = unsafeCastId

instance IsMPSKernel (Id MPSMatrixVectorMultiplication) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixVectorMultiplication) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixVectorMultiplication) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixRandomMTGP32 ----------

-- | MPSMatrixRandomMTGP32
--
-- Generates random numbers using a Mersenne Twister algorithm              suitable for GPU execution.  It uses a period of 2**11214.              For further details see:          Mutsuo Saito. A Variant of Mersenne Twister Suitable for Graphic Processors. arXiv:1005.4973
-- 
-- Phantom type for @MPSMatrixRandomMTGP32@.
data MPSMatrixRandomMTGP32

instance IsObjCObject (Id MPSMatrixRandomMTGP32) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixRandomMTGP32"

class IsMPSMatrixRandom a => IsMPSMatrixRandomMTGP32 a where
  toMPSMatrixRandomMTGP32 :: a -> Id MPSMatrixRandomMTGP32

instance IsMPSMatrixRandomMTGP32 (Id MPSMatrixRandomMTGP32) where
  toMPSMatrixRandomMTGP32 = unsafeCastId

instance IsMPSKernel (Id MPSMatrixRandomMTGP32) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixRandom (Id MPSMatrixRandomMTGP32) where
  toMPSMatrixRandom = unsafeCastId

instance IsNSObject (Id MPSMatrixRandomMTGP32) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixRandomPhilox ----------

-- | MPSMatrixRandomPhilox
--
-- Generates random numbers using a counter based algorithm.              For further details see:          John K. Salmon, Mark A. Moraes, Ron O. Dror, and David E. Shaw. Parallel Random Numbers: As Easy as 1, 2, 3.
-- 
-- Phantom type for @MPSMatrixRandomPhilox@.
data MPSMatrixRandomPhilox

instance IsObjCObject (Id MPSMatrixRandomPhilox) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixRandomPhilox"

class IsMPSMatrixRandom a => IsMPSMatrixRandomPhilox a where
  toMPSMatrixRandomPhilox :: a -> Id MPSMatrixRandomPhilox

instance IsMPSMatrixRandomPhilox (Id MPSMatrixRandomPhilox) where
  toMPSMatrixRandomPhilox = unsafeCastId

instance IsMPSKernel (Id MPSMatrixRandomPhilox) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixRandom (Id MPSMatrixRandomPhilox) where
  toMPSMatrixRandom = unsafeCastId

instance IsNSObject (Id MPSMatrixRandomPhilox) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixBatchNormalization ----------

-- | MPSMatrixBatchNormalization
--
-- This depends on Metal.framework.
--
-- Applies a batch normalization to a matrix.
--
-- A MPSMatrixBatchNormalization object computes the batch normalization              of a collection of feature vectors stored in an MPSMatrix.
--
-- Feature vectors are stored in a row of the supplied input matrix and the              normalization is performed along columns:
--
-- y[i,j] = gamma[j] * (x[i,j] - mean(x[:,j])) / (variance(x[:,j]) + epsilon) + beta[j]
--
-- where gamma and beta are supplied weight and bias factors and epsilon is a small value added              to the variance.
--
-- Optionally a neuron activation function may be applied to the result.
-- 
-- Phantom type for @MPSMatrixBatchNormalization@.
data MPSMatrixBatchNormalization

instance IsObjCObject (Id MPSMatrixBatchNormalization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixBatchNormalization"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixBatchNormalization a where
  toMPSMatrixBatchNormalization :: a -> Id MPSMatrixBatchNormalization

instance IsMPSMatrixBatchNormalization (Id MPSMatrixBatchNormalization) where
  toMPSMatrixBatchNormalization = unsafeCastId

instance IsMPSKernel (Id MPSMatrixBatchNormalization) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixBatchNormalization) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixBatchNormalization) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixDecompositionCholesky ----------

-- | MPSMatrixDecompositionCholesky
--
-- This depends on Metal.framework.
--
-- A kernel for computing the Cholesky factorization of a matrix.
--
-- A MPSMatrixDecompositionLU object computes one of the following              factorizations of a matrix A:
--
-- A = L * L**T                  A = U**T * U
--
-- A is a symmetric positive-definite matrix for which the              factorization is to be computed. L and U are lower and upper              triangular matrices respectively.
-- 
-- Phantom type for @MPSMatrixDecompositionCholesky@.
data MPSMatrixDecompositionCholesky

instance IsObjCObject (Id MPSMatrixDecompositionCholesky) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixDecompositionCholesky"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixDecompositionCholesky a where
  toMPSMatrixDecompositionCholesky :: a -> Id MPSMatrixDecompositionCholesky

instance IsMPSMatrixDecompositionCholesky (Id MPSMatrixDecompositionCholesky) where
  toMPSMatrixDecompositionCholesky = unsafeCastId

instance IsMPSKernel (Id MPSMatrixDecompositionCholesky) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixDecompositionCholesky) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixDecompositionCholesky) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixDecompositionLU ----------

-- | MPSMatrixDecompositionLU
--
-- This depends on Metal.framework.
--
-- A kernel for computing the LU factorization of a matrix using              partial pivoting with row interchanges.
--
-- A MPSMatrixDecompositionLU object computes an LU factorization:
--
-- P * A = L * U
--
-- A is a matrix for which the LU factorization is to be computed.              L is a unit lower triangular matrix and U is an upper triangular              matrix.  P is a permutation matrix.
-- 
-- Phantom type for @MPSMatrixDecompositionLU@.
data MPSMatrixDecompositionLU

instance IsObjCObject (Id MPSMatrixDecompositionLU) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixDecompositionLU"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixDecompositionLU a where
  toMPSMatrixDecompositionLU :: a -> Id MPSMatrixDecompositionLU

instance IsMPSMatrixDecompositionLU (Id MPSMatrixDecompositionLU) where
  toMPSMatrixDecompositionLU = unsafeCastId

instance IsMPSKernel (Id MPSMatrixDecompositionLU) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixDecompositionLU) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixDecompositionLU) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixFindTopK ----------

-- | MPSMatrixFindTopK
--
-- This depends on Metal.framework.
--
-- A kernel that find top-K values and their corresponding indices withing a row of a matrix
--
-- A MPSMatrixFindTopK object computes finds the 'k' largest values within              a row of a matrix and returns the value found and the index of the entry              in the source matrix. This operation is performed independently on the              rows and matrices in batch of the source matrix.
-- 
-- Phantom type for @MPSMatrixFindTopK@.
data MPSMatrixFindTopK

instance IsObjCObject (Id MPSMatrixFindTopK) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixFindTopK"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixFindTopK a where
  toMPSMatrixFindTopK :: a -> Id MPSMatrixFindTopK

instance IsMPSMatrixFindTopK (Id MPSMatrixFindTopK) where
  toMPSMatrixFindTopK = unsafeCastId

instance IsMPSKernel (Id MPSMatrixFindTopK) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixFindTopK) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixFindTopK) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixNeuron ----------

-- | MPSMatrixNeuron
--
-- This depends on Metal.framework.
--
-- A neuron activation kernel that operates on matrices.
--
-- A MPSMatrixNeuron object computes:
--
-- y = neuron(alpha * x + bias)
--
-- y is the output matrix, x is the input matrix corresponding              to a collection of input vectors and bias is a vector which is broadcast              and accumulated to each row of the intermediate result.              alpha is a scale factor applied to the input.
--
-- neuron() defines the pointwise function that is applied to the intermediate result.
--
-- Note: This function computes the same result as MPSMatrixFullyConnected that has                      unit weight matrix.
-- 
-- Phantom type for @MPSMatrixNeuron@.
data MPSMatrixNeuron

instance IsObjCObject (Id MPSMatrixNeuron) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixNeuron"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixNeuron a where
  toMPSMatrixNeuron :: a -> Id MPSMatrixNeuron

instance IsMPSMatrixNeuron (Id MPSMatrixNeuron) where
  toMPSMatrixNeuron = unsafeCastId

instance IsMPSKernel (Id MPSMatrixNeuron) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixNeuron) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixNeuron) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixSoftMax ----------

-- | MPSMatrixSoftMax
--
-- This depends on Metal.framework.
--
-- A softmax kernel that operates on matrices.
--
-- A MPSMatrixSoftMax object computes:
--
-- B_ij = Exp { A_ij } / ( Sum_k Exp { A_ik } )
--
-- A and B are matrices which are represented by MPSMatrix              objects. This filter computes the same result for MPSMatrices as              MPSCNNSoftMax filter does for MPSImages by interpreting the columns              of the matrix as feature channels, that is the sum runs over column indices.
-- 
-- Phantom type for @MPSMatrixSoftMax@.
data MPSMatrixSoftMax

instance IsObjCObject (Id MPSMatrixSoftMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixSoftMax"

class IsMPSMatrixUnaryKernel a => IsMPSMatrixSoftMax a where
  toMPSMatrixSoftMax :: a -> Id MPSMatrixSoftMax

instance IsMPSMatrixSoftMax (Id MPSMatrixSoftMax) where
  toMPSMatrixSoftMax = unsafeCastId

instance IsMPSKernel (Id MPSMatrixSoftMax) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixSoftMax) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixSoftMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayMultiaryGradientKernel ----------

-- | Phantom type for @MPSNDArrayMultiaryGradientKernel@.
data MPSNDArrayMultiaryGradientKernel

instance IsObjCObject (Id MPSNDArrayMultiaryGradientKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayMultiaryGradientKernel"

class IsMPSNDArrayMultiaryBase a => IsMPSNDArrayMultiaryGradientKernel a where
  toMPSNDArrayMultiaryGradientKernel :: a -> Id MPSNDArrayMultiaryGradientKernel

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayMultiaryGradientKernel) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayMultiaryGradientKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayMultiaryGradientKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsNSObject (Id MPSNDArrayMultiaryGradientKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayMultiaryKernel ----------

-- | Phantom type for @MPSNDArrayMultiaryKernel@.
data MPSNDArrayMultiaryKernel

instance IsObjCObject (Id MPSNDArrayMultiaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayMultiaryKernel"

class IsMPSNDArrayMultiaryBase a => IsMPSNDArrayMultiaryKernel a where
  toMPSNDArrayMultiaryKernel :: a -> Id MPSNDArrayMultiaryKernel

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayMultiaryKernel) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayMultiaryKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayMultiaryKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsNSObject (Id MPSNDArrayMultiaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNNOptimizerAdam ----------

-- | MPSNNOptimizerAdam
--
-- The MPSNNOptimizerAdam performs an Adam Update
--
-- Initialization time              m[0] = 0 (Initialize initial 1st moment vector aka momentum, user is responsible for this)              v[0] = 0 (Initialize initial 2nd moment vector aka velocity, user is responsible for this)              t    = 0 (Initialize timestep)
--
-- https://arxiv.org/abs/1412.6980
--
-- At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                v[t] is velocity                m[t] is momentum
-- 
-- Phantom type for @MPSNNOptimizerAdam@.
data MPSNNOptimizerAdam

instance IsObjCObject (Id MPSNNOptimizerAdam) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNOptimizerAdam"

class IsMPSNNOptimizer a => IsMPSNNOptimizerAdam a where
  toMPSNNOptimizerAdam :: a -> Id MPSNNOptimizerAdam

instance IsMPSNNOptimizerAdam (Id MPSNNOptimizerAdam) where
  toMPSNNOptimizerAdam = unsafeCastId

instance IsMPSKernel (Id MPSNNOptimizerAdam) where
  toMPSKernel = unsafeCastId

instance IsMPSNNOptimizer (Id MPSNNOptimizerAdam) where
  toMPSNNOptimizer = unsafeCastId

instance IsNSObject (Id MPSNNOptimizerAdam) where
  toNSObject = unsafeCastId

-- ---------- MPSNNOptimizerRMSProp ----------

-- | MPSNNOptimizerRMSProp
--
-- The MPSNNOptimizerRMSProp performs an RMSProp Update              RMSProp is also known as root mean square propagation.
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
-- 
-- Phantom type for @MPSNNOptimizerRMSProp@.
data MPSNNOptimizerRMSProp

instance IsObjCObject (Id MPSNNOptimizerRMSProp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNOptimizerRMSProp"

class IsMPSNNOptimizer a => IsMPSNNOptimizerRMSProp a where
  toMPSNNOptimizerRMSProp :: a -> Id MPSNNOptimizerRMSProp

instance IsMPSNNOptimizerRMSProp (Id MPSNNOptimizerRMSProp) where
  toMPSNNOptimizerRMSProp = unsafeCastId

instance IsMPSKernel (Id MPSNNOptimizerRMSProp) where
  toMPSKernel = unsafeCastId

instance IsMPSNNOptimizer (Id MPSNNOptimizerRMSProp) where
  toMPSNNOptimizer = unsafeCastId

instance IsNSObject (Id MPSNNOptimizerRMSProp) where
  toNSObject = unsafeCastId

-- ---------- MPSNNOptimizerStochasticGradientDescent ----------

-- | MPSNNOptimizerStochasticGradientDescent
--
-- The MPSNNOptimizerStochasticGradientDescent performs a gradient descent with an optional momentum Update              RMSProp is also known as root mean square propagation.
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
-- 
-- Phantom type for @MPSNNOptimizerStochasticGradientDescent@.
data MPSNNOptimizerStochasticGradientDescent

instance IsObjCObject (Id MPSNNOptimizerStochasticGradientDescent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNOptimizerStochasticGradientDescent"

class IsMPSNNOptimizer a => IsMPSNNOptimizerStochasticGradientDescent a where
  toMPSNNOptimizerStochasticGradientDescent :: a -> Id MPSNNOptimizerStochasticGradientDescent

instance IsMPSNNOptimizerStochasticGradientDescent (Id MPSNNOptimizerStochasticGradientDescent) where
  toMPSNNOptimizerStochasticGradientDescent = unsafeCastId

instance IsMPSKernel (Id MPSNNOptimizerStochasticGradientDescent) where
  toMPSKernel = unsafeCastId

instance IsMPSNNOptimizer (Id MPSNNOptimizerStochasticGradientDescent) where
  toMPSNNOptimizer = unsafeCastId

instance IsNSObject (Id MPSNNOptimizerStochasticGradientDescent) where
  toNSObject = unsafeCastId

-- ---------- MPSImageAreaMax ----------

-- | MPSImageAreaMax
--
-- The MPSImageAreaMax kernel finds the maximum pixel value in a rectangular region centered around each pixel              in the source image. If there are multiple channels in the source image, each channel is processed independently.              The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
-- 
-- Phantom type for @MPSImageAreaMax@.
data MPSImageAreaMax

instance IsObjCObject (Id MPSImageAreaMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageAreaMax"

class IsMPSUnaryImageKernel a => IsMPSImageAreaMax a where
  toMPSImageAreaMax :: a -> Id MPSImageAreaMax

instance IsMPSImageAreaMax (Id MPSImageAreaMax) where
  toMPSImageAreaMax = unsafeCastId

instance IsMPSKernel (Id MPSImageAreaMax) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageAreaMax) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageAreaMax) where
  toNSObject = unsafeCastId

-- ---------- MPSImageBox ----------

-- | MPSImageBox
--
-- The MPSImageBox convolves an image with given filter of odd width and height. The kernel elements              all have equal weight, achieving a blur effect. (Each result is the unweighted average of the              surrounding pixels.) This allows for much faster algorithms, espcially for larger blur radii.              The box height and width must be odd numbers. The box blur is a separable filter. The implementation               is aware of this and will act accordingly to give best performance for multi-dimensional blurs.
-- 
-- Phantom type for @MPSImageBox@.
data MPSImageBox

instance IsObjCObject (Id MPSImageBox) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageBox"

class IsMPSUnaryImageKernel a => IsMPSImageBox a where
  toMPSImageBox :: a -> Id MPSImageBox

instance IsMPSImageBox (Id MPSImageBox) where
  toMPSImageBox = unsafeCastId

instance IsMPSKernel (Id MPSImageBox) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageBox) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageBox) where
  toNSObject = unsafeCastId

-- ---------- MPSImageCanny ----------

-- | MPSImageCanny
--
-- The MPSImageCanny implements the Canny edge detection algorithm.              When the color model of the source and destination textures match, the              filter is applied to each channel seperately. If the destination is monochrome              but source multichannel, the source will be converted to grayscale using the              linear gray color transform vector (v).                Luminance = v[0] * pixel.x + v[1] * pixel.y + v[2] * pixel.z;
--
-- The canny edge detection algorithm consists of 5 steps:              1. Blur the source image using a Gaussian blur with a sigma parameter              2. Use horizontal and vertical Sobel filters to find a gradient magnitude and                direction.                  G = sqrt(Sx^2 + Sy^2)                  G_ang = arctan(Sy / Sx)              3. Perform non-maximum suppression to thin edges to single pixel widths.                A pixel is considered to be a maxium along the edge if it has the largest                gradient magnitude along the positive and negatve gradient direction. That                is, if the gradient direction is 90°, if the gradient magnitude of a pixel is                greater than its neighbors at -90° and 90° it is the maximum. Any pixel                which is not a maximum will have its value suppressed, by setting it's                magnitude to 0.              4. Double thresholding is preformed with two values ht and lt with ht > lt                to classify a pixel as part of a weak or strong edge. A pixel with gradient                value G is classified as:                  Strong edge: G > ht                  Weak edge: ht >= G > lt                  Not an edge: lt >= G              5. Edge tracking is performed along all weak edges to determine if they                are part of a strong edge. Any weak edges which are connected to a                strong edge are labelled true edges, along with strong edges themselves.                A pixel can be connected through any of its 8 neighbors. Any pixel marked                as a true edge is output with a high value, and all others are considered                background and output with a low value.
-- 
-- Phantom type for @MPSImageCanny@.
data MPSImageCanny

instance IsObjCObject (Id MPSImageCanny) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageCanny"

class IsMPSUnaryImageKernel a => IsMPSImageCanny a where
  toMPSImageCanny :: a -> Id MPSImageCanny

instance IsMPSImageCanny (Id MPSImageCanny) where
  toMPSImageCanny = unsafeCastId

instance IsMPSKernel (Id MPSImageCanny) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageCanny) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageCanny) where
  toNSObject = unsafeCastId

-- ---------- MPSImageConversion ----------

-- | MPSImageConversion
--
-- The MPSImageConversion filter performs a conversion from source to destination
-- 
-- Phantom type for @MPSImageConversion@.
data MPSImageConversion

instance IsObjCObject (Id MPSImageConversion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageConversion"

class IsMPSUnaryImageKernel a => IsMPSImageConversion a where
  toMPSImageConversion :: a -> Id MPSImageConversion

instance IsMPSImageConversion (Id MPSImageConversion) where
  toMPSImageConversion = unsafeCastId

instance IsMPSKernel (Id MPSImageConversion) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageConversion) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageConversion) where
  toNSObject = unsafeCastId

-- ---------- MPSImageConvolution ----------

-- | MPSImageConvolution
--
-- The MPSImageConvolution convolves an image with given filter of odd width and height.              The center of the kernel aligns with the MPSImageConvolution.offset. That is, the position               of the top left corner of the area covered by the kernel is given by               MPSImageConvolution.offset - {kernel_width>>1, kernel_height>>1, 0}
--
-- Optimized cases include 3x3,5x5,7x7,9x9,11x11, 1xN and Nx1. If a convolution kernel               does not fall into one of these cases but is a rank-1 matrix (a.k.a. separable)              then it will fall on an optimzied separable path. Other convolutions will execute with              full MxN complexity.
--
-- If there are multiple channels in the source image, each channel is processed independently.
--
-- Separable convolution filters may perform better when done in two passes. A convolution filter              is separable if the ratio of filter values between all rows is constant over the whole row. For              example, this edge detection filter:
--
-- -1      0       1
-- -2      0       2
-- -1      0       1
--
-- can be separated into the product of two vectors:
--
-- 1
-- 2      x    [-1  0   1]
-- 1
--
-- and consequently can be done as two, one-dimensional convolution passes back to back on the same image.               In this way, the number of multiplies (ignoring the fact that we could skip zeros here) is reduced from              3*3=9 to 3+3 = 6. There are similar savings for addition. For large filters, the savings can be profound.
-- 
-- Phantom type for @MPSImageConvolution@.
data MPSImageConvolution

instance IsObjCObject (Id MPSImageConvolution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageConvolution"

class IsMPSUnaryImageKernel a => IsMPSImageConvolution a where
  toMPSImageConvolution :: a -> Id MPSImageConvolution

instance IsMPSImageConvolution (Id MPSImageConvolution) where
  toMPSImageConvolution = unsafeCastId

instance IsMPSKernel (Id MPSImageConvolution) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageConvolution) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageConvolution) where
  toNSObject = unsafeCastId

-- ---------- MPSImageDilate ----------

-- | MPSImageDilate
--
-- The MPSImageDilate finds the maximum pixel value in a rectangular region centered around each pixel in the              source image. It is like the MPSImageAreaMax, except that the intensity at each position is calculated relative              to a different value before determining which is the maximum pixel value, allowing for shaped, non-rectangular              morphological probes.
--
-- for each pixel in the filter window:
-- value =  pixel[filterY][filterX] - filter[filterY*filter_width+filterX]
-- if( value > bestValue ){
-- result = value
-- bestValue = value;
-- }
--
-- A filter that contains all zeros and is identical to a MPSImageAreaMax filter.  The center filter element              is assumed to be 0 to avoid causing a general darkening of the image.
--
-- The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
-- 
-- Phantom type for @MPSImageDilate@.
data MPSImageDilate

instance IsObjCObject (Id MPSImageDilate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageDilate"

class IsMPSUnaryImageKernel a => IsMPSImageDilate a where
  toMPSImageDilate :: a -> Id MPSImageDilate

instance IsMPSImageDilate (Id MPSImageDilate) where
  toMPSImageDilate = unsafeCastId

instance IsMPSKernel (Id MPSImageDilate) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageDilate) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageDilate) where
  toNSObject = unsafeCastId

-- ---------- MPSImageEuclideanDistanceTransform ----------

-- | MPSImageEuclideanDistanceTransform
--
-- Perform a Euclidean Distance Transform
-- 
-- Phantom type for @MPSImageEuclideanDistanceTransform@.
data MPSImageEuclideanDistanceTransform

instance IsObjCObject (Id MPSImageEuclideanDistanceTransform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageEuclideanDistanceTransform"

class IsMPSUnaryImageKernel a => IsMPSImageEuclideanDistanceTransform a where
  toMPSImageEuclideanDistanceTransform :: a -> Id MPSImageEuclideanDistanceTransform

instance IsMPSImageEuclideanDistanceTransform (Id MPSImageEuclideanDistanceTransform) where
  toMPSImageEuclideanDistanceTransform = unsafeCastId

instance IsMPSKernel (Id MPSImageEuclideanDistanceTransform) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageEuclideanDistanceTransform) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageEuclideanDistanceTransform) where
  toNSObject = unsafeCastId

-- ---------- MPSImageGaussianBlur ----------

-- | MPSImageGaussianBlur
--
-- The MPSImageGaussianBlur convolves an image with gaussian of given sigma in both x and y direction.
--
-- The MPSImageGaussianBlur utilizes a very fast algorith that typically runs at approximately                  1/2 of copy speeds. Notably, it is faster than either the tent or box blur except perhaps                  for very large filter windows. Mathematically, it is an approximate gaussian. Some                  non-gaussian behavior may be detectable with advanced analytical methods such as FFT.                    If a analytically clean gaussian filter is required, please use the MPSImageConvolution                   filter instead with an appropriate set of weights. The MPSImageGaussianBlur is intended                  to be suitable for all common image processing needs demanding ~10 bits of precision or                  less.
-- 
-- Phantom type for @MPSImageGaussianBlur@.
data MPSImageGaussianBlur

instance IsObjCObject (Id MPSImageGaussianBlur) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageGaussianBlur"

class IsMPSUnaryImageKernel a => IsMPSImageGaussianBlur a where
  toMPSImageGaussianBlur :: a -> Id MPSImageGaussianBlur

instance IsMPSImageGaussianBlur (Id MPSImageGaussianBlur) where
  toMPSImageGaussianBlur = unsafeCastId

instance IsMPSKernel (Id MPSImageGaussianBlur) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageGaussianBlur) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageGaussianBlur) where
  toNSObject = unsafeCastId

-- ---------- MPSImageHistogramEqualization ----------

-- | MPSImageHistogramEqualization
--
-- The MPSImageHistogramEqualization performs equalizes the histogram of an image.              The process is divided into three steps.
--
-- -# Call -initWithDevice:histogramInfo:   This creates a MPSImageHistogramEqualization              object.   It is done when the method returns.
--
-- -# Call -encodeTransform:sourceTexture:histogram:histogramOffset:  This creates a privately held              image transform (i.e. a cumulative distribution function of the histogram) which will be used to               equalize the distribution of the histogram of the source image. This process runs on a MTLCommandBuffer              when it is committed to a MTLCommandQueue. It must complete before the next step can be run.              It may be performed on the same MTLCommandBuffer.  The histogram argument specifies the histogram              buffer which contains the histogram values for sourceTexture.  The sourceTexture argument is used by              encodeTransform to determine the number of channels and therefore which histogram data in histogram               buffer to use. The histogram for sourceTexture must have been computed either on the CPU or using               the MPSImageHistogram kernel
--
-- -# Call -encodeToCommandBuffer:sourceTexture:destinationTexture: to read data from              sourceTexture, apply the equalization transform to it and write to destination texture.              This step is also done on the GPU on a MTLCommandQueue.
--
-- You can reuse the same equalization transform on other images to perform the              same transform on those images. (Since their distribution is probably different,              they will probably not be equalized by it.) This filter usually will not be able               to work in place.
-- 
-- Phantom type for @MPSImageHistogramEqualization@.
data MPSImageHistogramEqualization

instance IsObjCObject (Id MPSImageHistogramEqualization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageHistogramEqualization"

class IsMPSUnaryImageKernel a => IsMPSImageHistogramEqualization a where
  toMPSImageHistogramEqualization :: a -> Id MPSImageHistogramEqualization

instance IsMPSImageHistogramEqualization (Id MPSImageHistogramEqualization) where
  toMPSImageHistogramEqualization = unsafeCastId

instance IsMPSKernel (Id MPSImageHistogramEqualization) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageHistogramEqualization) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageHistogramEqualization) where
  toNSObject = unsafeCastId

-- ---------- MPSImageHistogramSpecification ----------

-- | MPSImageHistogramSpecification
--
-- The MPSImageHistogramSpecification performs a histogram specification operation on an image.              It is a generalized version of histogram equalization operation.  The histogram specificaiton filter              converts the image so that its histogram matches the desired histogram.
-- 
-- Phantom type for @MPSImageHistogramSpecification@.
data MPSImageHistogramSpecification

instance IsObjCObject (Id MPSImageHistogramSpecification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageHistogramSpecification"

class IsMPSUnaryImageKernel a => IsMPSImageHistogramSpecification a where
  toMPSImageHistogramSpecification :: a -> Id MPSImageHistogramSpecification

instance IsMPSImageHistogramSpecification (Id MPSImageHistogramSpecification) where
  toMPSImageHistogramSpecification = unsafeCastId

instance IsMPSKernel (Id MPSImageHistogramSpecification) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageHistogramSpecification) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageHistogramSpecification) where
  toNSObject = unsafeCastId

-- ---------- MPSImageIntegral ----------

-- | MPSImageIntegral
--
-- The MPSImageIntegral calculates the sum of pixels over a specified region in the image.              The value at each position is the sum of all pixels in a source image rectangle, sumRect:
--
-- sumRect.origin = MPSUnaryImageKernel.offset                  sumRect.size = dest_position - MPSUnaryImageKernel.clipRect.origin
--
-- If the channels in the source image are normalized, half-float or floating values,              the destination image is recommended to be a 32-bit floating-point image.              If the channels in the source image are integer values, it is recommended that              an appropriate 32-bit integer image destination format is used.
--
-- This kernel accepts uint and int textures in addition to unorm and floating-point textures.
-- 
-- Phantom type for @MPSImageIntegral@.
data MPSImageIntegral

instance IsObjCObject (Id MPSImageIntegral) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageIntegral"

class IsMPSUnaryImageKernel a => IsMPSImageIntegral a where
  toMPSImageIntegral :: a -> Id MPSImageIntegral

instance IsMPSImageIntegral (Id MPSImageIntegral) where
  toMPSImageIntegral = unsafeCastId

instance IsMPSKernel (Id MPSImageIntegral) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageIntegral) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageIntegral) where
  toNSObject = unsafeCastId

-- ---------- MPSImageIntegralOfSquares ----------

-- | MPSImageIntegralOfSquares
--
-- The MPSImageIntegralOfSquares calculates the sum of squared pixels over a specified region in the image.              The value at each position is the sum of all squared pixels in a source image rectangle, sumRect:
--
-- sumRect.origin = MPSUnaryImageKernel.offset                  sumRect.size = dest_position - MPSUnaryImageKernel.clipRect.origin
--
-- If the channels in the source image are normalized, half-float or floating values,              the destination image is recommended to be a 32-bit floating-point image.              If the channels in the source image are integer values, it is recommended that              an appropriate 32-bit integer image destination format is used.
--
-- This kernel accepts uint and int textures in addition to unorm and floating-point textures.
-- 
-- Phantom type for @MPSImageIntegralOfSquares@.
data MPSImageIntegralOfSquares

instance IsObjCObject (Id MPSImageIntegralOfSquares) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageIntegralOfSquares"

class IsMPSUnaryImageKernel a => IsMPSImageIntegralOfSquares a where
  toMPSImageIntegralOfSquares :: a -> Id MPSImageIntegralOfSquares

instance IsMPSImageIntegralOfSquares (Id MPSImageIntegralOfSquares) where
  toMPSImageIntegralOfSquares = unsafeCastId

instance IsMPSKernel (Id MPSImageIntegralOfSquares) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageIntegralOfSquares) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageIntegralOfSquares) where
  toNSObject = unsafeCastId

-- ---------- MPSImageLaplacian ----------

-- | MPSImageLaplacian
--
-- The MPSImageLaplacian is an optimized variant of the MPSImageConvolution filter provided primarily for ease of use.              This filter uses an optimized convolution filter with a 3 x 3 kernel with the following weights:                  [ 0  1  0                    1 -4  1                    0  1  0 ]
--
-- The optimized convolution filter used by MPSImageLaplacian can also be used by creating a MPSImageConvolution              object with kernelWidth = 3, kernelHeight = 3 and weights as specified above.
-- 
-- Phantom type for @MPSImageLaplacian@.
data MPSImageLaplacian

instance IsObjCObject (Id MPSImageLaplacian) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageLaplacian"

class IsMPSUnaryImageKernel a => IsMPSImageLaplacian a where
  toMPSImageLaplacian :: a -> Id MPSImageLaplacian

instance IsMPSImageLaplacian (Id MPSImageLaplacian) where
  toMPSImageLaplacian = unsafeCastId

instance IsMPSKernel (Id MPSImageLaplacian) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageLaplacian) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageLaplacian) where
  toNSObject = unsafeCastId

-- ---------- MPSImageMedian ----------

-- | MPSImageMedian
--
-- The MPSImageMedian applies a median filter to an image.  A median filter finds the               median color value for each channel within a kernelDiameter x kernelDiameter               window surrounding the pixel of interest.  It is a common means of noise reduction              and also as a smoothing filter with edge preserving qualities.
--
-- NOTE: The MPSImageMedian filter currently only supports images with <= 8 bits/channel.
-- 
-- Phantom type for @MPSImageMedian@.
data MPSImageMedian

instance IsObjCObject (Id MPSImageMedian) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageMedian"

class IsMPSUnaryImageKernel a => IsMPSImageMedian a where
  toMPSImageMedian :: a -> Id MPSImageMedian

instance IsMPSImageMedian (Id MPSImageMedian) where
  toMPSImageMedian = unsafeCastId

instance IsMPSKernel (Id MPSImageMedian) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageMedian) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageMedian) where
  toNSObject = unsafeCastId

-- ---------- MPSImagePyramid ----------

-- | MPSImagePyramid
--
-- The MPSImagePyramid is a base class for creating different kinds of pyramid images
--
-- Currently supported pyramid-types are:              MPSImageGaussianPyramid
--
-- The Gaussian image pyramid kernel is enqueued as a in-place operation using              MPSUnaryImageKernel::encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:              and all mipmap levels after level=1, present in the provided image are filled using              the provided filtering kernel. The fallbackCopyAllocator parameter is not used.
--
-- The Gaussian image pyramid filter ignores clipRect and offset and fills              the entire mipmap levels.
--
-- Note: Make sure your texture type is compatible with mipmapping and supports texture views                  (see MTLTextureUsagePixelFormatView).
--
-- Note: Recall the size of the nth mipmap level:
--
-- w_n = max(1, floor(w_0 / 2^n))
-- h_n = max(1, floor(h_0 / 2^n)),
--
-- where w_0, h_0 are the zeroth level width and height. ie the image dimensions themselves.
-- 
-- Phantom type for @MPSImagePyramid@.
data MPSImagePyramid

instance IsObjCObject (Id MPSImagePyramid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImagePyramid"

class IsMPSUnaryImageKernel a => IsMPSImagePyramid a where
  toMPSImagePyramid :: a -> Id MPSImagePyramid

instance IsMPSImagePyramid (Id MPSImagePyramid) where
  toMPSImagePyramid = unsafeCastId

instance IsMPSKernel (Id MPSImagePyramid) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImagePyramid) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImagePyramid) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceUnary ----------

-- | MPSImageReduceUnary
--
-- The MPSImageReduce performs a reduction operation              The reduction operations supported are:                   - Reduce row min                   - Reduce column min                   - Reduce row max                   - Reduce column max                   - Reduce row mean                   - Reduce column mean                   - Reduce row sum                   - Reduce column sum
-- 
-- Phantom type for @MPSImageReduceUnary@.
data MPSImageReduceUnary

instance IsObjCObject (Id MPSImageReduceUnary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceUnary"

class IsMPSUnaryImageKernel a => IsMPSImageReduceUnary a where
  toMPSImageReduceUnary :: a -> Id MPSImageReduceUnary

instance IsMPSImageReduceUnary (Id MPSImageReduceUnary) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceUnary) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceUnary) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceUnary) where
  toNSObject = unsafeCastId

-- ---------- MPSImageScale ----------

-- | MPSImageScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageScale filter can be used to resample an existing image              using a different sampling frequency in each dimension. This can be              used to enlarge or reduce the size of an image, or change the aspect              ratio of an image.
--
-- The resample methods supported are:                    Bilinear                    Bicubcic                    Lanczos
-- 
-- Phantom type for @MPSImageScale@.
data MPSImageScale

instance IsObjCObject (Id MPSImageScale) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageScale"

class IsMPSUnaryImageKernel a => IsMPSImageScale a where
  toMPSImageScale :: a -> Id MPSImageScale

instance IsMPSImageScale (Id MPSImageScale) where
  toMPSImageScale = unsafeCastId

instance IsMPSKernel (Id MPSImageScale) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageScale) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageScale) where
  toNSObject = unsafeCastId

-- ---------- MPSImageSobel ----------

-- | MPSImageSobel
--
-- The MPSImageSobel implements the Sobel filter.              When the color model (e.g. RGB, two-channel, grayscale, etc.) of source               and destination textures match, the filter is applied to each channel               separately. If the destination is monochrome (single channel) but source               multichannel, the pixel values are converted to grayscale before applying Sobel              operator using the linear gray color transform vector (v).
--
-- Luminance = v[0] * pixel.x + v[1] * pixel.y + v[2] * pixel.z;
-- 
-- Phantom type for @MPSImageSobel@.
data MPSImageSobel

instance IsObjCObject (Id MPSImageSobel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageSobel"

class IsMPSUnaryImageKernel a => IsMPSImageSobel a where
  toMPSImageSobel :: a -> Id MPSImageSobel

instance IsMPSImageSobel (Id MPSImageSobel) where
  toMPSImageSobel = unsafeCastId

instance IsMPSKernel (Id MPSImageSobel) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageSobel) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageSobel) where
  toNSObject = unsafeCastId

-- ---------- MPSImageStatisticsMean ----------

-- | MPSImageStatisticsMean
--
-- The MPSImageStatisticsMean computes the mean for a given region of an image.
-- 
-- Phantom type for @MPSImageStatisticsMean@.
data MPSImageStatisticsMean

instance IsObjCObject (Id MPSImageStatisticsMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageStatisticsMean"

class IsMPSUnaryImageKernel a => IsMPSImageStatisticsMean a where
  toMPSImageStatisticsMean :: a -> Id MPSImageStatisticsMean

instance IsMPSImageStatisticsMean (Id MPSImageStatisticsMean) where
  toMPSImageStatisticsMean = unsafeCastId

instance IsMPSKernel (Id MPSImageStatisticsMean) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageStatisticsMean) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageStatisticsMean) where
  toNSObject = unsafeCastId

-- ---------- MPSImageStatisticsMeanAndVariance ----------

-- | MPSImageStatisticsMeanAndVariance
--
-- The MPSImageStatisticsMeanAndVariance computes the mean and variance for a given region of an image.              The mean and variance values are written to the destination image at the following pixel locations:                  - mean value is written at pixel location (0, 0)                  - variance value is written at pixel location (1, 0)
-- 
-- Phantom type for @MPSImageStatisticsMeanAndVariance@.
data MPSImageStatisticsMeanAndVariance

instance IsObjCObject (Id MPSImageStatisticsMeanAndVariance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageStatisticsMeanAndVariance"

class IsMPSUnaryImageKernel a => IsMPSImageStatisticsMeanAndVariance a where
  toMPSImageStatisticsMeanAndVariance :: a -> Id MPSImageStatisticsMeanAndVariance

instance IsMPSImageStatisticsMeanAndVariance (Id MPSImageStatisticsMeanAndVariance) where
  toMPSImageStatisticsMeanAndVariance = unsafeCastId

instance IsMPSKernel (Id MPSImageStatisticsMeanAndVariance) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageStatisticsMeanAndVariance) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageStatisticsMeanAndVariance) where
  toNSObject = unsafeCastId

-- ---------- MPSImageStatisticsMinAndMax ----------

-- | MPSImageStatisticsMinAndMax
--
-- The MPSImageStatisticsMinAndMax computes the minimum and maximum pixel values for a given region of an image.              The min and max values are written to the destination image at the following pixel locations:                  - min value is written at pixel location (0, 0)                  - max value is written at pixel location (1, 0)
-- 
-- Phantom type for @MPSImageStatisticsMinAndMax@.
data MPSImageStatisticsMinAndMax

instance IsObjCObject (Id MPSImageStatisticsMinAndMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageStatisticsMinAndMax"

class IsMPSUnaryImageKernel a => IsMPSImageStatisticsMinAndMax a where
  toMPSImageStatisticsMinAndMax :: a -> Id MPSImageStatisticsMinAndMax

instance IsMPSImageStatisticsMinAndMax (Id MPSImageStatisticsMinAndMax) where
  toMPSImageStatisticsMinAndMax = unsafeCastId

instance IsMPSKernel (Id MPSImageStatisticsMinAndMax) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageStatisticsMinAndMax) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageStatisticsMinAndMax) where
  toNSObject = unsafeCastId

-- ---------- MPSImageThresholdBinary ----------

-- | MPSImageThresholdBinary
--
-- The MPSThreshold filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdBinary function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? maximumValue : 0
-- 
-- Phantom type for @MPSImageThresholdBinary@.
data MPSImageThresholdBinary

instance IsObjCObject (Id MPSImageThresholdBinary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageThresholdBinary"

class IsMPSUnaryImageKernel a => IsMPSImageThresholdBinary a where
  toMPSImageThresholdBinary :: a -> Id MPSImageThresholdBinary

instance IsMPSImageThresholdBinary (Id MPSImageThresholdBinary) where
  toMPSImageThresholdBinary = unsafeCastId

instance IsMPSKernel (Id MPSImageThresholdBinary) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageThresholdBinary) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageThresholdBinary) where
  toNSObject = unsafeCastId

-- ---------- MPSImageThresholdBinaryInverse ----------

-- | MPSImageThresholdBinaryInverse
--
-- The MPSImageThresholdBinaryInverse filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdBinaryInverse function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? 0 : maximumValue
-- 
-- Phantom type for @MPSImageThresholdBinaryInverse@.
data MPSImageThresholdBinaryInverse

instance IsObjCObject (Id MPSImageThresholdBinaryInverse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageThresholdBinaryInverse"

class IsMPSUnaryImageKernel a => IsMPSImageThresholdBinaryInverse a where
  toMPSImageThresholdBinaryInverse :: a -> Id MPSImageThresholdBinaryInverse

instance IsMPSImageThresholdBinaryInverse (Id MPSImageThresholdBinaryInverse) where
  toMPSImageThresholdBinaryInverse = unsafeCastId

instance IsMPSKernel (Id MPSImageThresholdBinaryInverse) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageThresholdBinaryInverse) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageThresholdBinaryInverse) where
  toNSObject = unsafeCastId

-- ---------- MPSImageThresholdToZero ----------

-- | MPSImageThresholdToZero
--
-- The MPSImageThresholdToZero filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdToZero function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? sourcePixelValue : 0
-- 
-- Phantom type for @MPSImageThresholdToZero@.
data MPSImageThresholdToZero

instance IsObjCObject (Id MPSImageThresholdToZero) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageThresholdToZero"

class IsMPSUnaryImageKernel a => IsMPSImageThresholdToZero a where
  toMPSImageThresholdToZero :: a -> Id MPSImageThresholdToZero

instance IsMPSImageThresholdToZero (Id MPSImageThresholdToZero) where
  toMPSImageThresholdToZero = unsafeCastId

instance IsMPSKernel (Id MPSImageThresholdToZero) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageThresholdToZero) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageThresholdToZero) where
  toNSObject = unsafeCastId

-- ---------- MPSImageThresholdToZeroInverse ----------

-- | MPSImageThresholdToZeroInverse
--
-- The MPSImageThresholdToZeroInverse filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdToZeroINverse function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? 0 : sourcePixelValue
-- 
-- Phantom type for @MPSImageThresholdToZeroInverse@.
data MPSImageThresholdToZeroInverse

instance IsObjCObject (Id MPSImageThresholdToZeroInverse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageThresholdToZeroInverse"

class IsMPSUnaryImageKernel a => IsMPSImageThresholdToZeroInverse a where
  toMPSImageThresholdToZeroInverse :: a -> Id MPSImageThresholdToZeroInverse

instance IsMPSImageThresholdToZeroInverse (Id MPSImageThresholdToZeroInverse) where
  toMPSImageThresholdToZeroInverse = unsafeCastId

instance IsMPSKernel (Id MPSImageThresholdToZeroInverse) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageThresholdToZeroInverse) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageThresholdToZeroInverse) where
  toNSObject = unsafeCastId

-- ---------- MPSImageThresholdTruncate ----------

-- | MPSImageThresholdTruncate
--
-- The MPSImageThresholdTruncate filter applies a fixed-level threshold to each pixel in the image:              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdTruncate function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? thresholdValue : sourcePixelValue
-- 
-- Phantom type for @MPSImageThresholdTruncate@.
data MPSImageThresholdTruncate

instance IsObjCObject (Id MPSImageThresholdTruncate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageThresholdTruncate"

class IsMPSUnaryImageKernel a => IsMPSImageThresholdTruncate a where
  toMPSImageThresholdTruncate :: a -> Id MPSImageThresholdTruncate

instance IsMPSImageThresholdTruncate (Id MPSImageThresholdTruncate) where
  toMPSImageThresholdTruncate = unsafeCastId

instance IsMPSKernel (Id MPSImageThresholdTruncate) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageThresholdTruncate) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageThresholdTruncate) where
  toNSObject = unsafeCastId

-- ---------- MPSImageTranspose ----------

-- | MPSImageTranspose
--
-- The MPSImageTranspose transposes an image
--
-- This kernel accepts uint and int textures in addition to unorm and floating-point textures.
-- 
-- Phantom type for @MPSImageTranspose@.
data MPSImageTranspose

instance IsObjCObject (Id MPSImageTranspose) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageTranspose"

class IsMPSUnaryImageKernel a => IsMPSImageTranspose a where
  toMPSImageTranspose :: a -> Id MPSImageTranspose

instance IsMPSImageTranspose (Id MPSImageTranspose) where
  toMPSImageTranspose = unsafeCastId

instance IsMPSKernel (Id MPSImageTranspose) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageTranspose) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageTranspose) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBinaryConvolutionNode ----------

-- | A MPSNNFilterNode representing a MPSCNNBinaryConvolution kernel
-- 
-- Phantom type for @MPSCNNBinaryConvolutionNode@.
data MPSCNNBinaryConvolutionNode

instance IsObjCObject (Id MPSCNNBinaryConvolutionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBinaryConvolutionNode"

class IsMPSCNNConvolutionNode a => IsMPSCNNBinaryConvolutionNode a where
  toMPSCNNBinaryConvolutionNode :: a -> Id MPSCNNBinaryConvolutionNode

instance IsMPSCNNBinaryConvolutionNode (Id MPSCNNBinaryConvolutionNode) where
  toMPSCNNBinaryConvolutionNode = unsafeCastId

instance IsMPSCNNConvolutionNode (Id MPSCNNBinaryConvolutionNode) where
  toMPSCNNConvolutionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNBinaryConvolutionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNBinaryConvolutionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTransposeNode ----------

-- | A MPSNNFilterNode representing a MPSCNNConvolutionTranspose kernel
-- 
-- Phantom type for @MPSCNNConvolutionTransposeNode@.
data MPSCNNConvolutionTransposeNode

instance IsObjCObject (Id MPSCNNConvolutionTransposeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTransposeNode"

class IsMPSCNNConvolutionNode a => IsMPSCNNConvolutionTransposeNode a where
  toMPSCNNConvolutionTransposeNode :: a -> Id MPSCNNConvolutionTransposeNode

instance IsMPSCNNConvolutionTransposeNode (Id MPSCNNConvolutionTransposeNode) where
  toMPSCNNConvolutionTransposeNode = unsafeCastId

instance IsMPSCNNConvolutionNode (Id MPSCNNConvolutionTransposeNode) where
  toMPSCNNConvolutionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNConvolutionTransposeNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTransposeNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNFullyConnectedNode ----------

-- | A MPSNNFilterNode representing a MPSCNNFullyConnected kernel
-- 
-- Phantom type for @MPSCNNFullyConnectedNode@.
data MPSCNNFullyConnectedNode

instance IsObjCObject (Id MPSCNNFullyConnectedNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNFullyConnectedNode"

class IsMPSCNNConvolutionNode a => IsMPSCNNFullyConnectedNode a where
  toMPSCNNFullyConnectedNode :: a -> Id MPSCNNFullyConnectedNode

instance IsMPSCNNFullyConnectedNode (Id MPSCNNFullyConnectedNode) where
  toMPSCNNFullyConnectedNode = unsafeCastId

instance IsMPSCNNConvolutionNode (Id MPSCNNFullyConnectedNode) where
  toMPSCNNConvolutionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNFullyConnectedNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNFullyConnectedNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronAbsoluteNode ----------

-- | A node representing a MPSCNNNeuronAbsolute kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = fabs(x)
-- 
-- Phantom type for @MPSCNNNeuronAbsoluteNode@.
data MPSCNNNeuronAbsoluteNode

instance IsObjCObject (Id MPSCNNNeuronAbsoluteNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronAbsoluteNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronAbsoluteNode a where
  toMPSCNNNeuronAbsoluteNode :: a -> Id MPSCNNNeuronAbsoluteNode

instance IsMPSCNNNeuronAbsoluteNode (Id MPSCNNNeuronAbsoluteNode) where
  toMPSCNNNeuronAbsoluteNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronAbsoluteNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronAbsoluteNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronAbsoluteNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronELUNode ----------

-- | A node representing a MPSCNNNeuronELU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * exp(x) - 1, x <  0
-- x             , x >= 0
-- 
-- Phantom type for @MPSCNNNeuronELUNode@.
data MPSCNNNeuronELUNode

instance IsObjCObject (Id MPSCNNNeuronELUNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronELUNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronELUNode a where
  toMPSCNNNeuronELUNode :: a -> Id MPSCNNNeuronELUNode

instance IsMPSCNNNeuronELUNode (Id MPSCNNNeuronELUNode) where
  toMPSCNNNeuronELUNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronELUNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronELUNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronELUNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronExponentialNode ----------

-- | A node representing a MPSCNNNeuronExponential kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = c ^ (a * x + b)
-- 
-- Phantom type for @MPSCNNNeuronExponentialNode@.
data MPSCNNNeuronExponentialNode

instance IsObjCObject (Id MPSCNNNeuronExponentialNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronExponentialNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronExponentialNode a where
  toMPSCNNNeuronExponentialNode :: a -> Id MPSCNNNeuronExponentialNode

instance IsMPSCNNNeuronExponentialNode (Id MPSCNNNeuronExponentialNode) where
  toMPSCNNNeuronExponentialNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronExponentialNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronExponentialNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronExponentialNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronGeLUNode ----------

-- | A node representing a MPSCNNNeuronGeLU kernel
--
-- For each pixel, applies the following function:
-- 
-- Phantom type for @MPSCNNNeuronGeLUNode@.
data MPSCNNNeuronGeLUNode

instance IsObjCObject (Id MPSCNNNeuronGeLUNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronGeLUNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronGeLUNode a where
  toMPSCNNNeuronGeLUNode :: a -> Id MPSCNNNeuronGeLUNode

instance IsMPSCNNNeuronGeLUNode (Id MPSCNNNeuronGeLUNode) where
  toMPSCNNNeuronGeLUNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronGeLUNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronGeLUNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronGeLUNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronHardSigmoidNode ----------

-- | A node representing a MPSCNNNeuronHardSigmoid kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = clamp((a * x) + b, 0, 1)
-- 
-- Phantom type for @MPSCNNNeuronHardSigmoidNode@.
data MPSCNNNeuronHardSigmoidNode

instance IsObjCObject (Id MPSCNNNeuronHardSigmoidNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronHardSigmoidNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronHardSigmoidNode a where
  toMPSCNNNeuronHardSigmoidNode :: a -> Id MPSCNNNeuronHardSigmoidNode

instance IsMPSCNNNeuronHardSigmoidNode (Id MPSCNNNeuronHardSigmoidNode) where
  toMPSCNNNeuronHardSigmoidNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronHardSigmoidNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronHardSigmoidNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronHardSigmoidNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronLinearNode ----------

-- | A node representing a MPSCNNNeuronLinear kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * x + b
-- 
-- Phantom type for @MPSCNNNeuronLinearNode@.
data MPSCNNNeuronLinearNode

instance IsObjCObject (Id MPSCNNNeuronLinearNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronLinearNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronLinearNode a where
  toMPSCNNNeuronLinearNode :: a -> Id MPSCNNNeuronLinearNode

instance IsMPSCNNNeuronLinearNode (Id MPSCNNNeuronLinearNode) where
  toMPSCNNNeuronLinearNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronLinearNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronLinearNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronLinearNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronLogarithmNode ----------

-- | A node representing a MPSCNNNeuronLogarithm kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = log_c(a * x + b)
-- 
-- Phantom type for @MPSCNNNeuronLogarithmNode@.
data MPSCNNNeuronLogarithmNode

instance IsObjCObject (Id MPSCNNNeuronLogarithmNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronLogarithmNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronLogarithmNode a where
  toMPSCNNNeuronLogarithmNode :: a -> Id MPSCNNNeuronLogarithmNode

instance IsMPSCNNNeuronLogarithmNode (Id MPSCNNNeuronLogarithmNode) where
  toMPSCNNNeuronLogarithmNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronLogarithmNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronLogarithmNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronLogarithmNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronPReLUNode ----------

-- | A ReLU node with parameter a provided independently for each feature channel
--
-- For each pixel, applies the following function:
--
-- f(x) = x                if x >= 0
-- = aData[i] * x     if x < 0,  i is the index of the feature channel
-- @param      sourceNode              The MPSNNImageNode representing the source MPSImage for the filter
-- @param      aData                   An array of single precision floating-point alpha values to use
-- 
-- Phantom type for @MPSCNNNeuronPReLUNode@.
data MPSCNNNeuronPReLUNode

instance IsObjCObject (Id MPSCNNNeuronPReLUNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronPReLUNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronPReLUNode a where
  toMPSCNNNeuronPReLUNode :: a -> Id MPSCNNNeuronPReLUNode

instance IsMPSCNNNeuronPReLUNode (Id MPSCNNNeuronPReLUNode) where
  toMPSCNNNeuronPReLUNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronPReLUNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronPReLUNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronPReLUNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronPowerNode ----------

-- | A node representing a MPSCNNNeuronPower kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = (a * x + b) ^ c
-- 
-- Phantom type for @MPSCNNNeuronPowerNode@.
data MPSCNNNeuronPowerNode

instance IsObjCObject (Id MPSCNNNeuronPowerNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronPowerNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronPowerNode a where
  toMPSCNNNeuronPowerNode :: a -> Id MPSCNNNeuronPowerNode

instance IsMPSCNNNeuronPowerNode (Id MPSCNNNeuronPowerNode) where
  toMPSCNNNeuronPowerNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronPowerNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronPowerNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronPowerNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronReLUNNode ----------

-- | A node representing a MPSCNNNeuronReLUN kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = min((x >= 0 ? x : a * x), b)
-- 
-- Phantom type for @MPSCNNNeuronReLUNNode@.
data MPSCNNNeuronReLUNNode

instance IsObjCObject (Id MPSCNNNeuronReLUNNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronReLUNNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronReLUNNode a where
  toMPSCNNNeuronReLUNNode :: a -> Id MPSCNNNeuronReLUNNode

instance IsMPSCNNNeuronReLUNNode (Id MPSCNNNeuronReLUNNode) where
  toMPSCNNNeuronReLUNNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronReLUNNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronReLUNNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronReLUNNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronReLUNode ----------

-- | A node representing a MPSCNNNeuronReLU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x            if x >= 0
-- = a * x        if x < 0
-- 
-- Phantom type for @MPSCNNNeuronReLUNode@.
data MPSCNNNeuronReLUNode

instance IsObjCObject (Id MPSCNNNeuronReLUNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronReLUNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronReLUNode a where
  toMPSCNNNeuronReLUNode :: a -> Id MPSCNNNeuronReLUNode

instance IsMPSCNNNeuronReLUNode (Id MPSCNNNeuronReLUNode) where
  toMPSCNNNeuronReLUNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronReLUNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronReLUNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronReLUNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSigmoidNode ----------

-- | A node representing a MPSCNNNeuronSigmoid kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = 1 / (1 + e^-x)
-- 
-- Phantom type for @MPSCNNNeuronSigmoidNode@.
data MPSCNNNeuronSigmoidNode

instance IsObjCObject (Id MPSCNNNeuronSigmoidNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSigmoidNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronSigmoidNode a where
  toMPSCNNNeuronSigmoidNode :: a -> Id MPSCNNNeuronSigmoidNode

instance IsMPSCNNNeuronSigmoidNode (Id MPSCNNNeuronSigmoidNode) where
  toMPSCNNNeuronSigmoidNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronSigmoidNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronSigmoidNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSigmoidNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSoftPlusNode ----------

-- | A node representing a MPSCNNNeuronSoftPlus kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * log(1 + e^(b * x))
-- 
-- Phantom type for @MPSCNNNeuronSoftPlusNode@.
data MPSCNNNeuronSoftPlusNode

instance IsObjCObject (Id MPSCNNNeuronSoftPlusNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSoftPlusNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronSoftPlusNode a where
  toMPSCNNNeuronSoftPlusNode :: a -> Id MPSCNNNeuronSoftPlusNode

instance IsMPSCNNNeuronSoftPlusNode (Id MPSCNNNeuronSoftPlusNode) where
  toMPSCNNNeuronSoftPlusNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronSoftPlusNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronSoftPlusNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSoftPlusNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSoftSignNode ----------

-- | A node representing a MPSCNNNeuronSoftSign kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x / (1 + abs(x))
-- 
-- Phantom type for @MPSCNNNeuronSoftSignNode@.
data MPSCNNNeuronSoftSignNode

instance IsObjCObject (Id MPSCNNNeuronSoftSignNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSoftSignNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronSoftSignNode a where
  toMPSCNNNeuronSoftSignNode :: a -> Id MPSCNNNeuronSoftSignNode

instance IsMPSCNNNeuronSoftSignNode (Id MPSCNNNeuronSoftSignNode) where
  toMPSCNNNeuronSoftSignNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronSoftSignNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronSoftSignNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSoftSignNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronTanHNode ----------

-- | A node representing a MPSCNNNeuronTanH kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * tanh(b * x)
-- 
-- Phantom type for @MPSCNNNeuronTanHNode@.
data MPSCNNNeuronTanHNode

instance IsObjCObject (Id MPSCNNNeuronTanHNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronTanHNode"

class IsMPSCNNNeuronNode a => IsMPSCNNNeuronTanHNode a where
  toMPSCNNNeuronTanHNode :: a -> Id MPSCNNNeuronTanHNode

instance IsMPSCNNNeuronTanHNode (Id MPSCNNNeuronTanHNode) where
  toMPSCNNNeuronTanHNode = unsafeCastId

instance IsMPSCNNNeuronNode (Id MPSCNNNeuronTanHNode) where
  toMPSCNNNeuronNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronTanHNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronTanHNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNCrossChannelNormalizationNode ----------

-- | Node representing MPSCNNCrossChannelNormalization
--
-- The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,               where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where               N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 5.0f
-- delta = 1.0f
-- kernelHeight = kernelWidth = kernelSize
-- 
-- Phantom type for @MPSCNNCrossChannelNormalizationNode@.
data MPSCNNCrossChannelNormalizationNode

instance IsObjCObject (Id MPSCNNCrossChannelNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNCrossChannelNormalizationNode"

class IsMPSCNNNormalizationNode a => IsMPSCNNCrossChannelNormalizationNode a where
  toMPSCNNCrossChannelNormalizationNode :: a -> Id MPSCNNCrossChannelNormalizationNode

instance IsMPSCNNCrossChannelNormalizationNode (Id MPSCNNCrossChannelNormalizationNode) where
  toMPSCNNCrossChannelNormalizationNode = unsafeCastId

instance IsMPSCNNNormalizationNode (Id MPSCNNCrossChannelNormalizationNode) where
  toMPSCNNNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNCrossChannelNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNCrossChannelNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLocalContrastNormalizationNode ----------

-- | Node representing MPSCNNLocalContrastNormalization
--
-- The result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / pow((delta + alpha * variance(i,j)), beta),
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. *
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 0.5f
-- delta = 2^-10
-- pm = 0
-- ps = 1
-- p0 = 1
-- kernelHeight = kernelWidth = kernelSize
-- 
-- Phantom type for @MPSCNNLocalContrastNormalizationNode@.
data MPSCNNLocalContrastNormalizationNode

instance IsObjCObject (Id MPSCNNLocalContrastNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLocalContrastNormalizationNode"

class IsMPSCNNNormalizationNode a => IsMPSCNNLocalContrastNormalizationNode a where
  toMPSCNNLocalContrastNormalizationNode :: a -> Id MPSCNNLocalContrastNormalizationNode

instance IsMPSCNNLocalContrastNormalizationNode (Id MPSCNNLocalContrastNormalizationNode) where
  toMPSCNNLocalContrastNormalizationNode = unsafeCastId

instance IsMPSCNNNormalizationNode (Id MPSCNNLocalContrastNormalizationNode) where
  toMPSCNNNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNLocalContrastNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNLocalContrastNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSpatialNormalizationNode ----------

-- | Node representing MPSCNNSpatialNormalization
--
-- For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).               It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,               where kw and kh are the kernelWidth and the kernelHeight.
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 5.0f
-- delta = 1.0f
-- kernelHeight = kernelWidth = kernelSize
-- 
-- Phantom type for @MPSCNNSpatialNormalizationNode@.
data MPSCNNSpatialNormalizationNode

instance IsObjCObject (Id MPSCNNSpatialNormalizationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSpatialNormalizationNode"

class IsMPSCNNNormalizationNode a => IsMPSCNNSpatialNormalizationNode a where
  toMPSCNNSpatialNormalizationNode :: a -> Id MPSCNNSpatialNormalizationNode

instance IsMPSCNNSpatialNormalizationNode (Id MPSCNNSpatialNormalizationNode) where
  toMPSCNNSpatialNormalizationNode = unsafeCastId

instance IsMPSCNNNormalizationNode (Id MPSCNNSpatialNormalizationNode) where
  toMPSCNNNormalizationNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNSpatialNormalizationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNSpatialNormalizationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingAverageNode ----------

-- | A node representing a MPSCNNPoolingAverage kernel
--
-- The default edge mode is MPSImageEdgeModeClamp
-- 
-- Phantom type for @MPSCNNPoolingAverageNode@.
data MPSCNNPoolingAverageNode

instance IsObjCObject (Id MPSCNNPoolingAverageNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingAverageNode"

class IsMPSCNNPoolingNode a => IsMPSCNNPoolingAverageNode a where
  toMPSCNNPoolingAverageNode :: a -> Id MPSCNNPoolingAverageNode

instance IsMPSCNNPoolingAverageNode (Id MPSCNNPoolingAverageNode) where
  toMPSCNNPoolingAverageNode = unsafeCastId

instance IsMPSCNNPoolingNode (Id MPSCNNPoolingAverageNode) where
  toMPSCNNPoolingNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingAverageNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingAverageNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingL2NormNode ----------

-- | A node representing a MPSCNNPoolingL2Norm kernel
--
-- The default edge mode is MPSImageEdgeModeClamp
-- 
-- Phantom type for @MPSCNNPoolingL2NormNode@.
data MPSCNNPoolingL2NormNode

instance IsObjCObject (Id MPSCNNPoolingL2NormNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingL2NormNode"

class IsMPSCNNPoolingNode a => IsMPSCNNPoolingL2NormNode a where
  toMPSCNNPoolingL2NormNode :: a -> Id MPSCNNPoolingL2NormNode

instance IsMPSCNNPoolingL2NormNode (Id MPSCNNPoolingL2NormNode) where
  toMPSCNNPoolingL2NormNode = unsafeCastId

instance IsMPSCNNPoolingNode (Id MPSCNNPoolingL2NormNode) where
  toMPSCNNPoolingNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingL2NormNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingL2NormNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingMaxNode ----------

-- | A node representing a MPSCNNPoolingMax kernel
--
-- The default edge mode is MPSImageEdgeModeClamp
-- 
-- Phantom type for @MPSCNNPoolingMaxNode@.
data MPSCNNPoolingMaxNode

instance IsObjCObject (Id MPSCNNPoolingMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingMaxNode"

class IsMPSCNNPoolingNode a => IsMPSCNNPoolingMaxNode a where
  toMPSCNNPoolingMaxNode :: a -> Id MPSCNNPoolingMaxNode

instance IsMPSCNNPoolingMaxNode (Id MPSCNNPoolingMaxNode) where
  toMPSCNNPoolingMaxNode = unsafeCastId

instance IsMPSCNNPoolingNode (Id MPSCNNPoolingMaxNode) where
  toMPSCNNPoolingNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNAdditionNode ----------

-- | returns elementwise sum of left + right
-- 
-- Phantom type for @MPSNNAdditionNode@.
data MPSNNAdditionNode

instance IsObjCObject (Id MPSNNAdditionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNAdditionNode"

class IsMPSNNBinaryArithmeticNode a => IsMPSNNAdditionNode a where
  toMPSNNAdditionNode :: a -> Id MPSNNAdditionNode

instance IsMPSNNAdditionNode (Id MPSNNAdditionNode) where
  toMPSNNAdditionNode = unsafeCastId

instance IsMPSNNBinaryArithmeticNode (Id MPSNNAdditionNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNAdditionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNAdditionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNComparisonNode ----------

-- | returns elementwise comparison of left and right
-- 
-- Phantom type for @MPSNNComparisonNode@.
data MPSNNComparisonNode

instance IsObjCObject (Id MPSNNComparisonNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNComparisonNode"

class IsMPSNNBinaryArithmeticNode a => IsMPSNNComparisonNode a where
  toMPSNNComparisonNode :: a -> Id MPSNNComparisonNode

instance IsMPSNNComparisonNode (Id MPSNNComparisonNode) where
  toMPSNNComparisonNode = unsafeCastId

instance IsMPSNNBinaryArithmeticNode (Id MPSNNComparisonNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNComparisonNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNComparisonNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNDivisionNode ----------

-- | returns elementwise quotient of left / right
-- 
-- Phantom type for @MPSNNDivisionNode@.
data MPSNNDivisionNode

instance IsObjCObject (Id MPSNNDivisionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNDivisionNode"

class IsMPSNNBinaryArithmeticNode a => IsMPSNNDivisionNode a where
  toMPSNNDivisionNode :: a -> Id MPSNNDivisionNode

instance IsMPSNNDivisionNode (Id MPSNNDivisionNode) where
  toMPSNNDivisionNode = unsafeCastId

instance IsMPSNNBinaryArithmeticNode (Id MPSNNDivisionNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNDivisionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNDivisionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNMultiplicationNode ----------

-- | returns elementwise product of left * right
-- 
-- Phantom type for @MPSNNMultiplicationNode@.
data MPSNNMultiplicationNode

instance IsObjCObject (Id MPSNNMultiplicationNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNMultiplicationNode"

class IsMPSNNBinaryArithmeticNode a => IsMPSNNMultiplicationNode a where
  toMPSNNMultiplicationNode :: a -> Id MPSNNMultiplicationNode

instance IsMPSNNMultiplicationNode (Id MPSNNMultiplicationNode) where
  toMPSNNMultiplicationNode = unsafeCastId

instance IsMPSNNBinaryArithmeticNode (Id MPSNNMultiplicationNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNMultiplicationNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNMultiplicationNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNSubtractionNode ----------

-- | returns elementwise difference of left - right
-- 
-- Phantom type for @MPSNNSubtractionNode@.
data MPSNNSubtractionNode

instance IsObjCObject (Id MPSNNSubtractionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNSubtractionNode"

class IsMPSNNBinaryArithmeticNode a => IsMPSNNSubtractionNode a where
  toMPSNNSubtractionNode :: a -> Id MPSNNSubtractionNode

instance IsMPSNNSubtractionNode (Id MPSNNSubtractionNode) where
  toMPSNNSubtractionNode = unsafeCastId

instance IsMPSNNBinaryArithmeticNode (Id MPSNNSubtractionNode) where
  toMPSNNBinaryArithmeticNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNSubtractionNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNSubtractionNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationGradientNode ----------

-- | MPSCNNBatchNormalizationGradientNode
--
-- A node representing batch normalization gradient for training
--
-- This filter encapsulates the MPSCNNBatchNormalizationStatisticsGradient              and MPSCNNBatchNormalizationGradient low level filters as a single              node. They will be called in sequence: statistics gradient until the              batch is complete, then batch normalization gradient on the result.
-- 
-- Phantom type for @MPSCNNBatchNormalizationGradientNode@.
data MPSCNNBatchNormalizationGradientNode

instance IsObjCObject (Id MPSCNNBatchNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNBatchNormalizationGradientNode a where
  toMPSCNNBatchNormalizationGradientNode :: a -> Id MPSCNNBatchNormalizationGradientNode

instance IsMPSCNNBatchNormalizationGradientNode (Id MPSCNNBatchNormalizationGradientNode) where
  toMPSCNNBatchNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNBatchNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNBatchNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionGradientNode ----------

-- | Phantom type for @MPSCNNConvolutionGradientNode@.
data MPSCNNConvolutionGradientNode

instance IsObjCObject (Id MPSCNNConvolutionGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNConvolutionGradientNode a where
  toMPSCNNConvolutionGradientNode :: a -> Id MPSCNNConvolutionGradientNode

instance IsMPSCNNConvolutionGradientNode (Id MPSCNNConvolutionGradientNode) where
  toMPSCNNConvolutionGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNConvolutionGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNConvolutionGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNCrossChannelNormalizationGradientNode ----------

-- | Phantom type for @MPSCNNCrossChannelNormalizationGradientNode@.
data MPSCNNCrossChannelNormalizationGradientNode

instance IsObjCObject (Id MPSCNNCrossChannelNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNCrossChannelNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNCrossChannelNormalizationGradientNode a where
  toMPSCNNCrossChannelNormalizationGradientNode :: a -> Id MPSCNNCrossChannelNormalizationGradientNode

instance IsMPSCNNCrossChannelNormalizationGradientNode (Id MPSCNNCrossChannelNormalizationGradientNode) where
  toMPSCNNCrossChannelNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNCrossChannelNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNCrossChannelNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNCrossChannelNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDropoutGradientNode ----------

-- | Phantom type for @MPSCNNDropoutGradientNode@.
data MPSCNNDropoutGradientNode

instance IsObjCObject (Id MPSCNNDropoutGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDropoutGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNDropoutGradientNode a where
  toMPSCNNDropoutGradientNode :: a -> Id MPSCNNDropoutGradientNode

instance IsMPSCNNDropoutGradientNode (Id MPSCNNDropoutGradientNode) where
  toMPSCNNDropoutGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNDropoutGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNDropoutGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNDropoutGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGroupNormalizationGradientNode ----------

-- | Phantom type for @MPSCNNGroupNormalizationGradientNode@.
data MPSCNNGroupNormalizationGradientNode

instance IsObjCObject (Id MPSCNNGroupNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGroupNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNGroupNormalizationGradientNode a where
  toMPSCNNGroupNormalizationGradientNode :: a -> Id MPSCNNGroupNormalizationGradientNode

instance IsMPSCNNGroupNormalizationGradientNode (Id MPSCNNGroupNormalizationGradientNode) where
  toMPSCNNGroupNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNGroupNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNGroupNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNGroupNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNInstanceNormalizationGradientNode ----------

-- | Phantom type for @MPSCNNInstanceNormalizationGradientNode@.
data MPSCNNInstanceNormalizationGradientNode

instance IsObjCObject (Id MPSCNNInstanceNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNInstanceNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNInstanceNormalizationGradientNode a where
  toMPSCNNInstanceNormalizationGradientNode :: a -> Id MPSCNNInstanceNormalizationGradientNode

instance IsMPSCNNInstanceNormalizationGradientNode (Id MPSCNNInstanceNormalizationGradientNode) where
  toMPSCNNInstanceNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNInstanceNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNInstanceNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNInstanceNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLocalContrastNormalizationGradientNode ----------

-- | Phantom type for @MPSCNNLocalContrastNormalizationGradientNode@.
data MPSCNNLocalContrastNormalizationGradientNode

instance IsObjCObject (Id MPSCNNLocalContrastNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLocalContrastNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNLocalContrastNormalizationGradientNode a where
  toMPSCNNLocalContrastNormalizationGradientNode :: a -> Id MPSCNNLocalContrastNormalizationGradientNode

instance IsMPSCNNLocalContrastNormalizationGradientNode (Id MPSCNNLocalContrastNormalizationGradientNode) where
  toMPSCNNLocalContrastNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNLocalContrastNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNLocalContrastNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNLocalContrastNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLogSoftMaxGradientNode ----------

-- | Node representing a MPSCNNLogSoftMaxGradient kernel
-- 
-- Phantom type for @MPSCNNLogSoftMaxGradientNode@.
data MPSCNNLogSoftMaxGradientNode

instance IsObjCObject (Id MPSCNNLogSoftMaxGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLogSoftMaxGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNLogSoftMaxGradientNode a where
  toMPSCNNLogSoftMaxGradientNode :: a -> Id MPSCNNLogSoftMaxGradientNode

instance IsMPSCNNLogSoftMaxGradientNode (Id MPSCNNLogSoftMaxGradientNode) where
  toMPSCNNLogSoftMaxGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNLogSoftMaxGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNLogSoftMaxGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNLogSoftMaxGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronGradientNode ----------

-- | A node representing a MPSCNNNeuronGradient
--
-- We use one generic neuron gradient node              instead of having dozens of subclasses.
-- 
-- Phantom type for @MPSCNNNeuronGradientNode@.
data MPSCNNNeuronGradientNode

instance IsObjCObject (Id MPSCNNNeuronGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNNeuronGradientNode a where
  toMPSCNNNeuronGradientNode :: a -> Id MPSCNNNeuronGradientNode

instance IsMPSCNNNeuronGradientNode (Id MPSCNNNeuronGradientNode) where
  toMPSCNNNeuronGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNNeuronGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNNeuronGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingGradientNode ----------

-- | Phantom type for @MPSCNNPoolingGradientNode@.
data MPSCNNPoolingGradientNode

instance IsObjCObject (Id MPSCNNPoolingGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNPoolingGradientNode a where
  toMPSCNNPoolingGradientNode :: a -> Id MPSCNNPoolingGradientNode

instance IsMPSCNNPoolingGradientNode (Id MPSCNNPoolingGradientNode) where
  toMPSCNNPoolingGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNPoolingGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSoftMaxGradientNode ----------

-- | Node representing a MPSCNNSoftMaxGradient kernel
-- 
-- Phantom type for @MPSCNNSoftMaxGradientNode@.
data MPSCNNSoftMaxGradientNode

instance IsObjCObject (Id MPSCNNSoftMaxGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSoftMaxGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNSoftMaxGradientNode a where
  toMPSCNNSoftMaxGradientNode :: a -> Id MPSCNNSoftMaxGradientNode

instance IsMPSCNNSoftMaxGradientNode (Id MPSCNNSoftMaxGradientNode) where
  toMPSCNNSoftMaxGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNSoftMaxGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNSoftMaxGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNSoftMaxGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSpatialNormalizationGradientNode ----------

-- | Phantom type for @MPSCNNSpatialNormalizationGradientNode@.
data MPSCNNSpatialNormalizationGradientNode

instance IsObjCObject (Id MPSCNNSpatialNormalizationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSpatialNormalizationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNSpatialNormalizationGradientNode a where
  toMPSCNNSpatialNormalizationGradientNode :: a -> Id MPSCNNSpatialNormalizationGradientNode

instance IsMPSCNNSpatialNormalizationGradientNode (Id MPSCNNSpatialNormalizationGradientNode) where
  toMPSCNNSpatialNormalizationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNSpatialNormalizationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNSpatialNormalizationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNSpatialNormalizationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingBilinearGradientNode ----------

-- | Node representing a MPSCNNUpsamplingBilinear kernel
-- 
-- Phantom type for @MPSCNNUpsamplingBilinearGradientNode@.
data MPSCNNUpsamplingBilinearGradientNode

instance IsObjCObject (Id MPSCNNUpsamplingBilinearGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingBilinearGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNUpsamplingBilinearGradientNode a where
  toMPSCNNUpsamplingBilinearGradientNode :: a -> Id MPSCNNUpsamplingBilinearGradientNode

instance IsMPSCNNUpsamplingBilinearGradientNode (Id MPSCNNUpsamplingBilinearGradientNode) where
  toMPSCNNUpsamplingBilinearGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNUpsamplingBilinearGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNUpsamplingBilinearGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingBilinearGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingNearestGradientNode ----------

-- | Node representing a MPSCNNUpsamplingNearest kernel
-- 
-- Phantom type for @MPSCNNUpsamplingNearestGradientNode@.
data MPSCNNUpsamplingNearestGradientNode

instance IsObjCObject (Id MPSCNNUpsamplingNearestGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingNearestGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSCNNUpsamplingNearestGradientNode a where
  toMPSCNNUpsamplingNearestGradientNode :: a -> Id MPSCNNUpsamplingNearestGradientNode

instance IsMPSCNNUpsamplingNearestGradientNode (Id MPSCNNUpsamplingNearestGradientNode) where
  toMPSCNNUpsamplingNearestGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNUpsamplingNearestGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNUpsamplingNearestGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingNearestGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNArithmeticGradientNode ----------

-- | Phantom type for @MPSNNArithmeticGradientNode@.
data MPSNNArithmeticGradientNode

instance IsObjCObject (Id MPSNNArithmeticGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNArithmeticGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNArithmeticGradientNode a where
  toMPSNNArithmeticGradientNode :: a -> Id MPSNNArithmeticGradientNode

instance IsMPSNNArithmeticGradientNode (Id MPSNNArithmeticGradientNode) where
  toMPSNNArithmeticGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNArithmeticGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNArithmeticGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNArithmeticGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNConcatenationGradientNode ----------

-- | MPSNNConcatenationGradientNode
--
-- A MPSNNSlice filter that operates as the conjugate computation for concatentation operators during training
--
-- As concatenation is formally just a copy and not a computation, there isn't a lot of arithmetic for              the slice operator to do, but we still need to extract out the relevant portion              of the gradient of the input signal that went into the corresponding concatenation              destination image.
-- 
-- Phantom type for @MPSNNConcatenationGradientNode@.
data MPSNNConcatenationGradientNode

instance IsObjCObject (Id MPSNNConcatenationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNConcatenationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNConcatenationGradientNode a where
  toMPSNNConcatenationGradientNode :: a -> Id MPSNNConcatenationGradientNode

instance IsMPSNNConcatenationGradientNode (Id MPSNNConcatenationGradientNode) where
  toMPSNNConcatenationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNConcatenationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNConcatenationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNConcatenationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGramMatrixCalculationGradientNode ----------

-- | Node representing a MPSNNGramMatrixCalculationGradient kernel
-- 
-- Phantom type for @MPSNNGramMatrixCalculationGradientNode@.
data MPSNNGramMatrixCalculationGradientNode

instance IsObjCObject (Id MPSNNGramMatrixCalculationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGramMatrixCalculationGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNGramMatrixCalculationGradientNode a where
  toMPSNNGramMatrixCalculationGradientNode :: a -> Id MPSNNGramMatrixCalculationGradientNode

instance IsMPSNNGramMatrixCalculationGradientNode (Id MPSNNGramMatrixCalculationGradientNode) where
  toMPSNNGramMatrixCalculationGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNGramMatrixCalculationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNGramMatrixCalculationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNGramMatrixCalculationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNLossGradientNode ----------

-- | Node representing a MPSNNLossGradient kernel
-- 
-- Phantom type for @MPSNNLossGradientNode@.
data MPSNNLossGradientNode

instance IsObjCObject (Id MPSNNLossGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNLossGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNLossGradientNode a where
  toMPSNNLossGradientNode :: a -> Id MPSNNLossGradientNode

instance IsMPSNNLossGradientNode (Id MPSNNLossGradientNode) where
  toMPSNNLossGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNLossGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNLossGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNLossGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNPadGradientNode ----------

-- | Phantom type for @MPSNNPadGradientNode@.
data MPSNNPadGradientNode

instance IsObjCObject (Id MPSNNPadGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNPadGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNPadGradientNode a where
  toMPSNNPadGradientNode :: a -> Id MPSNNPadGradientNode

instance IsMPSNNPadGradientNode (Id MPSNNPadGradientNode) where
  toMPSNNPadGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNPadGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNPadGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNPadGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionSpatialMeanGradientNode ----------

-- | Phantom type for @MPSNNReductionSpatialMeanGradientNode@.
data MPSNNReductionSpatialMeanGradientNode

instance IsObjCObject (Id MPSNNReductionSpatialMeanGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionSpatialMeanGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNReductionSpatialMeanGradientNode a where
  toMPSNNReductionSpatialMeanGradientNode :: a -> Id MPSNNReductionSpatialMeanGradientNode

instance IsMPSNNReductionSpatialMeanGradientNode (Id MPSNNReductionSpatialMeanGradientNode) where
  toMPSNNReductionSpatialMeanGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionSpatialMeanGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNReductionSpatialMeanGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionSpatialMeanGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReshapeGradientNode ----------

-- | Phantom type for @MPSNNReshapeGradientNode@.
data MPSNNReshapeGradientNode

instance IsObjCObject (Id MPSNNReshapeGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReshapeGradientNode"

class IsMPSNNGradientFilterNode a => IsMPSNNReshapeGradientNode a where
  toMPSNNReshapeGradientNode :: a -> Id MPSNNReshapeGradientNode

instance IsMPSNNReshapeGradientNode (Id MPSNNReshapeGradientNode) where
  toMPSNNReshapeGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReshapeGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNReshapeGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNReshapeGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNBilinearScaleNode ----------

-- | A MPSNNScale object that uses bilinear interpolation for resampling
--
-- Caution: bilinear downscaling by more than a factor of                    two in any dimension causes loss of information if a                    low pass filter is not run over the image first. Details                    may be omitted.
-- 
-- Phantom type for @MPSNNBilinearScaleNode@.
data MPSNNBilinearScaleNode

instance IsObjCObject (Id MPSNNBilinearScaleNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNBilinearScaleNode"

class IsMPSNNScaleNode a => IsMPSNNBilinearScaleNode a where
  toMPSNNBilinearScaleNode :: a -> Id MPSNNBilinearScaleNode

instance IsMPSNNBilinearScaleNode (Id MPSNNBilinearScaleNode) where
  toMPSNNBilinearScaleNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNBilinearScaleNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNScaleNode (Id MPSNNBilinearScaleNode) where
  toMPSNNScaleNode = unsafeCastId

instance IsNSObject (Id MPSNNBilinearScaleNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNLanczosScaleNode ----------

-- | A MPSNNScale object that uses the Lanczos resampling filter
--
-- This method does not require a low pass filter for downsampling                    by more than a factor of two. Caution: may cause ringing, which                    could prove distracting to a neural network unused to seeing it.                    You should use the resampling method that was used to train the                    network.
-- 
-- Phantom type for @MPSNNLanczosScaleNode@.
data MPSNNLanczosScaleNode

instance IsObjCObject (Id MPSNNLanczosScaleNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNLanczosScaleNode"

class IsMPSNNScaleNode a => IsMPSNNLanczosScaleNode a where
  toMPSNNLanczosScaleNode :: a -> Id MPSNNLanczosScaleNode

instance IsMPSNNLanczosScaleNode (Id MPSNNLanczosScaleNode) where
  toMPSNNLanczosScaleNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNLanczosScaleNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNScaleNode (Id MPSNNLanczosScaleNode) where
  toMPSNNScaleNode = unsafeCastId

instance IsNSObject (Id MPSNNLanczosScaleNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionColumnMaxNode ----------

-- | Phantom type for @MPSNNReductionColumnMaxNode@.
data MPSNNReductionColumnMaxNode

instance IsObjCObject (Id MPSNNReductionColumnMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionColumnMaxNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionColumnMaxNode a where
  toMPSNNReductionColumnMaxNode :: a -> Id MPSNNReductionColumnMaxNode

instance IsMPSNNReductionColumnMaxNode (Id MPSNNReductionColumnMaxNode) where
  toMPSNNReductionColumnMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionColumnMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionColumnMaxNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionColumnMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionColumnMeanNode ----------

-- | Phantom type for @MPSNNReductionColumnMeanNode@.
data MPSNNReductionColumnMeanNode

instance IsObjCObject (Id MPSNNReductionColumnMeanNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionColumnMeanNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionColumnMeanNode a where
  toMPSNNReductionColumnMeanNode :: a -> Id MPSNNReductionColumnMeanNode

instance IsMPSNNReductionColumnMeanNode (Id MPSNNReductionColumnMeanNode) where
  toMPSNNReductionColumnMeanNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionColumnMeanNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionColumnMeanNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionColumnMeanNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionColumnMinNode ----------

-- | Phantom type for @MPSNNReductionColumnMinNode@.
data MPSNNReductionColumnMinNode

instance IsObjCObject (Id MPSNNReductionColumnMinNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionColumnMinNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionColumnMinNode a where
  toMPSNNReductionColumnMinNode :: a -> Id MPSNNReductionColumnMinNode

instance IsMPSNNReductionColumnMinNode (Id MPSNNReductionColumnMinNode) where
  toMPSNNReductionColumnMinNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionColumnMinNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionColumnMinNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionColumnMinNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionColumnSumNode ----------

-- | Phantom type for @MPSNNReductionColumnSumNode@.
data MPSNNReductionColumnSumNode

instance IsObjCObject (Id MPSNNReductionColumnSumNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionColumnSumNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionColumnSumNode a where
  toMPSNNReductionColumnSumNode :: a -> Id MPSNNReductionColumnSumNode

instance IsMPSNNReductionColumnSumNode (Id MPSNNReductionColumnSumNode) where
  toMPSNNReductionColumnSumNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionColumnSumNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionColumnSumNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionColumnSumNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsArgumentMaxNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsArgumentMaxNode@.
data MPSNNReductionFeatureChannelsArgumentMaxNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsArgumentMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsArgumentMaxNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsArgumentMaxNode a where
  toMPSNNReductionFeatureChannelsArgumentMaxNode :: a -> Id MPSNNReductionFeatureChannelsArgumentMaxNode

instance IsMPSNNReductionFeatureChannelsArgumentMaxNode (Id MPSNNReductionFeatureChannelsArgumentMaxNode) where
  toMPSNNReductionFeatureChannelsArgumentMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsArgumentMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsArgumentMaxNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsArgumentMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsArgumentMinNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsArgumentMinNode@.
data MPSNNReductionFeatureChannelsArgumentMinNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsArgumentMinNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsArgumentMinNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsArgumentMinNode a where
  toMPSNNReductionFeatureChannelsArgumentMinNode :: a -> Id MPSNNReductionFeatureChannelsArgumentMinNode

instance IsMPSNNReductionFeatureChannelsArgumentMinNode (Id MPSNNReductionFeatureChannelsArgumentMinNode) where
  toMPSNNReductionFeatureChannelsArgumentMinNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsArgumentMinNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsArgumentMinNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsArgumentMinNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsMaxNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsMaxNode@.
data MPSNNReductionFeatureChannelsMaxNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsMaxNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsMaxNode a where
  toMPSNNReductionFeatureChannelsMaxNode :: a -> Id MPSNNReductionFeatureChannelsMaxNode

instance IsMPSNNReductionFeatureChannelsMaxNode (Id MPSNNReductionFeatureChannelsMaxNode) where
  toMPSNNReductionFeatureChannelsMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsMaxNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsMeanNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsMeanNode@.
data MPSNNReductionFeatureChannelsMeanNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsMeanNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsMeanNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsMeanNode a where
  toMPSNNReductionFeatureChannelsMeanNode :: a -> Id MPSNNReductionFeatureChannelsMeanNode

instance IsMPSNNReductionFeatureChannelsMeanNode (Id MPSNNReductionFeatureChannelsMeanNode) where
  toMPSNNReductionFeatureChannelsMeanNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsMeanNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsMeanNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsMeanNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsMinNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsMinNode@.
data MPSNNReductionFeatureChannelsMinNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsMinNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsMinNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsMinNode a where
  toMPSNNReductionFeatureChannelsMinNode :: a -> Id MPSNNReductionFeatureChannelsMinNode

instance IsMPSNNReductionFeatureChannelsMinNode (Id MPSNNReductionFeatureChannelsMinNode) where
  toMPSNNReductionFeatureChannelsMinNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsMinNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsMinNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsMinNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionFeatureChannelsSumNode ----------

-- | Phantom type for @MPSNNReductionFeatureChannelsSumNode@.
data MPSNNReductionFeatureChannelsSumNode

instance IsObjCObject (Id MPSNNReductionFeatureChannelsSumNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionFeatureChannelsSumNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionFeatureChannelsSumNode a where
  toMPSNNReductionFeatureChannelsSumNode :: a -> Id MPSNNReductionFeatureChannelsSumNode

instance IsMPSNNReductionFeatureChannelsSumNode (Id MPSNNReductionFeatureChannelsSumNode) where
  toMPSNNReductionFeatureChannelsSumNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionFeatureChannelsSumNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionFeatureChannelsSumNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionFeatureChannelsSumNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionRowMaxNode ----------

-- | Phantom type for @MPSNNReductionRowMaxNode@.
data MPSNNReductionRowMaxNode

instance IsObjCObject (Id MPSNNReductionRowMaxNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionRowMaxNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionRowMaxNode a where
  toMPSNNReductionRowMaxNode :: a -> Id MPSNNReductionRowMaxNode

instance IsMPSNNReductionRowMaxNode (Id MPSNNReductionRowMaxNode) where
  toMPSNNReductionRowMaxNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionRowMaxNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionRowMaxNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionRowMaxNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionRowMeanNode ----------

-- | Phantom type for @MPSNNReductionRowMeanNode@.
data MPSNNReductionRowMeanNode

instance IsObjCObject (Id MPSNNReductionRowMeanNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionRowMeanNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionRowMeanNode a where
  toMPSNNReductionRowMeanNode :: a -> Id MPSNNReductionRowMeanNode

instance IsMPSNNReductionRowMeanNode (Id MPSNNReductionRowMeanNode) where
  toMPSNNReductionRowMeanNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionRowMeanNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionRowMeanNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionRowMeanNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionRowMinNode ----------

-- | Phantom type for @MPSNNReductionRowMinNode@.
data MPSNNReductionRowMinNode

instance IsObjCObject (Id MPSNNReductionRowMinNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionRowMinNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionRowMinNode a where
  toMPSNNReductionRowMinNode :: a -> Id MPSNNReductionRowMinNode

instance IsMPSNNReductionRowMinNode (Id MPSNNReductionRowMinNode) where
  toMPSNNReductionRowMinNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionRowMinNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionRowMinNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionRowMinNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionRowSumNode ----------

-- | Phantom type for @MPSNNReductionRowSumNode@.
data MPSNNReductionRowSumNode

instance IsObjCObject (Id MPSNNReductionRowSumNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionRowSumNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionRowSumNode a where
  toMPSNNReductionRowSumNode :: a -> Id MPSNNReductionRowSumNode

instance IsMPSNNReductionRowSumNode (Id MPSNNReductionRowSumNode) where
  toMPSNNReductionRowSumNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionRowSumNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionRowSumNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionRowSumNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReductionSpatialMeanNode ----------

-- | Phantom type for @MPSNNReductionSpatialMeanNode@.
data MPSNNReductionSpatialMeanNode

instance IsObjCObject (Id MPSNNReductionSpatialMeanNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReductionSpatialMeanNode"

class IsMPSNNUnaryReductionNode a => IsMPSNNReductionSpatialMeanNode a where
  toMPSNNReductionSpatialMeanNode :: a -> Id MPSNNReductionSpatialMeanNode

instance IsMPSNNReductionSpatialMeanNode (Id MPSNNReductionSpatialMeanNode) where
  toMPSNNReductionSpatialMeanNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNReductionSpatialMeanNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNUnaryReductionNode (Id MPSNNReductionSpatialMeanNode) where
  toMPSNNUnaryReductionNode = unsafeCastId

instance IsNSObject (Id MPSNNReductionSpatialMeanNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNArithmeticGradientStateNode ----------

-- | Phantom type for @MPSNNArithmeticGradientStateNode@.
data MPSNNArithmeticGradientStateNode

instance IsObjCObject (Id MPSNNArithmeticGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNArithmeticGradientStateNode"

class IsMPSNNBinaryGradientStateNode a => IsMPSNNArithmeticGradientStateNode a where
  toMPSNNArithmeticGradientStateNode :: a -> Id MPSNNArithmeticGradientStateNode

instance IsMPSNNArithmeticGradientStateNode (Id MPSNNArithmeticGradientStateNode) where
  toMPSNNArithmeticGradientStateNode = unsafeCastId

instance IsMPSNNBinaryGradientStateNode (Id MPSNNArithmeticGradientStateNode) where
  toMPSNNBinaryGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSNNArithmeticGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSNNArithmeticGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionGradientStateNode ----------

-- | Phantom type for @MPSCNNConvolutionGradientStateNode@.
data MPSCNNConvolutionGradientStateNode

instance IsObjCObject (Id MPSCNNConvolutionGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionGradientStateNode"

class IsMPSNNGradientStateNode a => IsMPSCNNConvolutionGradientStateNode a where
  toMPSCNNConvolutionGradientStateNode :: a -> Id MPSCNNConvolutionGradientStateNode

instance IsMPSCNNConvolutionGradientStateNode (Id MPSCNNConvolutionGradientStateNode) where
  toMPSCNNConvolutionGradientStateNode = unsafeCastId

instance IsMPSNNGradientStateNode (Id MPSCNNConvolutionGradientStateNode) where
  toMPSNNGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSCNNConvolutionGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayGatherGradientState ----------

-- | A state created to record a MPSNDArrayGather kernel properties
--
-- at the time an -encode call was made.
--
-- Must be created with the appropriate MPSNDArray kernel method, for example:
--
-- MPSNDArrayGather* gather = [[MPSNDArrayGather alloc] initWithDevice: device];          MPSNDArrayGatherGradientState* state = [gather resultStateForSourceArrays:...];
-- 
-- Phantom type for @MPSNDArrayGatherGradientState@.
data MPSNDArrayGatherGradientState

instance IsObjCObject (Id MPSNDArrayGatherGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayGatherGradientState"

class IsMPSNDArrayGradientState a => IsMPSNDArrayGatherGradientState a where
  toMPSNDArrayGatherGradientState :: a -> Id MPSNDArrayGatherGradientState

instance IsMPSNDArrayGatherGradientState (Id MPSNDArrayGatherGradientState) where
  toMPSNDArrayGatherGradientState = unsafeCastId

instance IsMPSNDArrayGradientState (Id MPSNDArrayGatherGradientState) where
  toMPSNDArrayGradientState = unsafeCastId

instance IsMPSState (Id MPSNDArrayGatherGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSNDArrayGatherGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNArithmeticGradientState ----------

-- | MPSCNNArithmeticGradientState
--
-- This depends on Metal.framework.
--
-- The MPSCNNArithmeticGradientState is used to hold the clamp mask used by both              MPSCNNArithmetic forward filter and MPSCNNArithmeticGradient backward filter.              The MPSCNNArithmetic forward filter populates the MPSCNNArithmeticGradientState              object and the MPSCNNArithmeticGradient backward filter consumes the state              object.
--
-- The clamp mask is stored internally and is not accessible by the user.
-- 
-- Phantom type for @MPSCNNArithmeticGradientState@.
data MPSCNNArithmeticGradientState

instance IsObjCObject (Id MPSCNNArithmeticGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNArithmeticGradientState"

class IsMPSNNBinaryGradientState a => IsMPSCNNArithmeticGradientState a where
  toMPSCNNArithmeticGradientState :: a -> Id MPSCNNArithmeticGradientState

instance IsMPSCNNArithmeticGradientState (Id MPSCNNArithmeticGradientState) where
  toMPSCNNArithmeticGradientState = unsafeCastId

instance IsMPSNNBinaryGradientState (Id MPSCNNArithmeticGradientState) where
  toMPSNNBinaryGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNArithmeticGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNArithmeticGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationState ----------

-- | MPSCNNBatchNormalizationState
--
-- MPSCNNBatchNormalizationState encapsulates the data necessary              to execute batch normalization.
--
-- MPSCNNBatchNormalizationState cannot initialize the size of its own              underlying resources.  Use [MPSCNNBatchNormalizationStatistics resultStateForSourceImages:]              or [MPSCNNBatchNormalizationStatistics temporaryResultStateForCommandBuffer:sourceImages:].
-- 
-- Phantom type for @MPSCNNBatchNormalizationState@.
data MPSCNNBatchNormalizationState

instance IsObjCObject (Id MPSCNNBatchNormalizationState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationState"

class IsMPSNNGradientState a => IsMPSCNNBatchNormalizationState a where
  toMPSCNNBatchNormalizationState :: a -> Id MPSCNNBatchNormalizationState

instance IsMPSCNNBatchNormalizationState (Id MPSCNNBatchNormalizationState) where
  toMPSCNNBatchNormalizationState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNBatchNormalizationState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNBatchNormalizationState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionGradientState ----------

-- | MPSCNNConvolutionGradientState
--
-- The MPSCNNConvolutionGradientState is returned by resultStateForSourceImage:sourceStates method on MPSCNNConvolution object.              Note that resultStateForSourceImage:sourceStates:destinationImage creates the object on autoreleasepool.              It will be consumed by MPSCNNConvolutionGradient. This is also used by MPSCNNConvolutionTranspose encode call              that returns MPSImage on left hand side to correctly size the destination.              Note that state objects are not usable across batches i.e. when batch is done you should nuke the state object and create              new one for next batch.
--
-- This state exposes the gradient with respect to weights and biases, as computed by the MPSCNNConvolutionGradient kernel, as a metal buffer to be used              during weights and biases update. The standard weights and biases update formula is:
--
-- weights(t+1) = f(weights(t), gradientForWeights(t)) and                        biases(t+1) = f(biases(t), gradientForBiases(t)),
--
-- where the weights(t)/biases(t) are the wegihts and the biases at step t that are provided by data source provider used to create MPSCNNConvolution and              MPSCNNConvoltuionGradient objects. There are multiple ways user can update weights and biases as described below:
--
-- 1) For check pointing, i.e. updating weights/biases and storing:                   once the command buffer on which MPSCNNConvolutionGradient is enqueued is done (e.g. in command                 buffer completion callback), the application can simply use                                    float* delta_w = (float*)((char*)[gradientForWeights contents]);                                    float* delta_b = (float*)((char*)[gradientForBiases contents]);                  to update the weights and biases in the data provider directly.                  The application can instead provide a metal kernel that reads from gradientForWeights and gradientForBiases buffer and the buffer created using data provided by the data source                  to do any kind of update it will like to do, then read back the updated weights/biases and store to the data source. Note that lifetime of the                  gradientForWeights and gradientForBiases buffer is the same as the MPSCNNConvolutionGradientState. So it's the applications's responsibility to make sure the buffer is alive                  (retained) when the update kernel is running if the command buffer doesn't retain the buffer. Also, in order to gaurantee that the buffer is correctly                  synchronized for CPU side access, it is the application's responsibility to call                                     [gradientState synchronizeOnCommandBuffer:]                  before accessing data from the buffer.
--
-- 2) For a CPU side update, once the weights and biases in the data source provider are updated as above, the original MPSCNNConvolution and                 MPSCNNConvolutionGradient objects need to be updated with the new weigths and biases by calling the                       -(void) reloadWeightsAndBiasesFromDataSource                 method. Again application needs to call [gradientState synchronizeOnCommandBuffer:] before touching data on CPU side.
--
-- 3) The above CPU side update requires command buffer to be done. If the application doesn't want to update its data source provider object and would prefer to directly                 enqueue an update of the internal MPSCNNConvolution and MPSCNNConvolutionGradient weights/biases buffers on the GPU without CPU side involvement, it needs to do                 following:                     i) get gradientForWeights and gradientForBiases buffers from this gradient state object and set it as source of update kernel                    ii) create a temporary buffer, dest, of same size and set it as destination of update kernel                   iii) enqueue update kernel on command buffer                    iv) call reloadWeightsAndBiasesWithCommandBuffer:dest:weightsOffset:biasesOffset on MPSCNNConvolution and MPSCNNConvolutionGradient objects. This                        will reload the weights from application's update kernel in dest on GPU without CPU side involvement.
-- 
-- Phantom type for @MPSCNNConvolutionGradientState@.
data MPSCNNConvolutionGradientState

instance IsObjCObject (Id MPSCNNConvolutionGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionGradientState"

class IsMPSNNGradientState a => IsMPSCNNConvolutionGradientState a where
  toMPSCNNConvolutionGradientState :: a -> Id MPSCNNConvolutionGradientState

instance IsMPSCNNConvolutionGradientState (Id MPSCNNConvolutionGradientState) where
  toMPSCNNConvolutionGradientState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNConvolutionGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNConvolutionGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDropoutGradientState ----------

-- | MPSCNNDropoutGradientState
--
-- This depends on Metal.framework.
--
-- The MPSCNNDropoutGradientState is used to hold the mask used by both              MPSCNNDropout forward filter and MPSCNNDropoutGradient backward filter.              The MPSCNNDropout forward filter populates the MPSCNNDropoutGradientState              object and the MPSCNNDropoutGradient backward filter consumes the state              object.
--
-- While the mask is stored internally, the mask data is accessible by the              user for debugging purposes via an accessor method.
-- 
-- Phantom type for @MPSCNNDropoutGradientState@.
data MPSCNNDropoutGradientState

instance IsObjCObject (Id MPSCNNDropoutGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDropoutGradientState"

class IsMPSNNGradientState a => IsMPSCNNDropoutGradientState a where
  toMPSCNNDropoutGradientState :: a -> Id MPSCNNDropoutGradientState

instance IsMPSCNNDropoutGradientState (Id MPSCNNDropoutGradientState) where
  toMPSCNNDropoutGradientState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNDropoutGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNDropoutGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNDropoutGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGroupNormalizationGradientState ----------

-- | MPSCNNGroupNormalizationGradientState
--
-- This depends on Metal.framework
--
-- A state to hold information necessary to execute a gradient              pass for MPSCNNGroupNormalization.  Gradient states should              be created by using the forward kernel's methods.  This will              ensure that the state captures all information necessary to              execute the corresponding gradient pass.
-- 
-- Phantom type for @MPSCNNGroupNormalizationGradientState@.
data MPSCNNGroupNormalizationGradientState

instance IsObjCObject (Id MPSCNNGroupNormalizationGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGroupNormalizationGradientState"

class IsMPSNNGradientState a => IsMPSCNNGroupNormalizationGradientState a where
  toMPSCNNGroupNormalizationGradientState :: a -> Id MPSCNNGroupNormalizationGradientState

instance IsMPSCNNGroupNormalizationGradientState (Id MPSCNNGroupNormalizationGradientState) where
  toMPSCNNGroupNormalizationGradientState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNGroupNormalizationGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNGroupNormalizationGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNGroupNormalizationGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNInstanceNormalizationGradientState ----------

-- | MPSCNNInstanceNormalizationGradientState
--
-- This depends on Metal.framework
--
-- A state to hold information necessary to execute a gradient              pass for MPSCNNInstanceNormalization.  Gradient states should              be created by using the forward kernel's methods.  This will              ensure that the state captures all information necessary to              execute the corresponding gradient pass.
-- 
-- Phantom type for @MPSCNNInstanceNormalizationGradientState@.
data MPSCNNInstanceNormalizationGradientState

instance IsObjCObject (Id MPSCNNInstanceNormalizationGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNInstanceNormalizationGradientState"

class IsMPSNNGradientState a => IsMPSCNNInstanceNormalizationGradientState a where
  toMPSCNNInstanceNormalizationGradientState :: a -> Id MPSCNNInstanceNormalizationGradientState

instance IsMPSCNNInstanceNormalizationGradientState (Id MPSCNNInstanceNormalizationGradientState) where
  toMPSCNNInstanceNormalizationGradientState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNInstanceNormalizationGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNInstanceNormalizationGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNInstanceNormalizationGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSKeyedUnarchiver ----------

-- | MPSKeyedUnarchiver
--
-- A NSKeyedArchiver that supports the MPSDeviceProvider protocol for MPSKernel decoding
-- 
-- Phantom type for @MPSKeyedUnarchiver@.
data MPSKeyedUnarchiver

instance IsObjCObject (Id MPSKeyedUnarchiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSKeyedUnarchiver"

class IsNSKeyedUnarchiver a => IsMPSKeyedUnarchiver a where
  toMPSKeyedUnarchiver :: a -> Id MPSKeyedUnarchiver

instance IsMPSKeyedUnarchiver (Id MPSKeyedUnarchiver) where
  toMPSKeyedUnarchiver = unsafeCastId

instance IsNSCoder (Id MPSKeyedUnarchiver) where
  toNSCoder = unsafeCastId

instance IsNSKeyedUnarchiver (Id MPSKeyedUnarchiver) where
  toNSKeyedUnarchiver = unsafeCastId

instance IsNSObject (Id MPSKeyedUnarchiver) where
  toNSObject = unsafeCastId

-- ---------- MPSQuadrilateralAccelerationStructure ----------

-- | An acceleration structure built over quadrilaterals
--
-- See MPSPolygonAccelerationStructure for more information
-- 
-- Phantom type for @MPSQuadrilateralAccelerationStructure@.
data MPSQuadrilateralAccelerationStructure

instance IsObjCObject (Id MPSQuadrilateralAccelerationStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSQuadrilateralAccelerationStructure"

class IsMPSPolygonAccelerationStructure a => IsMPSQuadrilateralAccelerationStructure a where
  toMPSQuadrilateralAccelerationStructure :: a -> Id MPSQuadrilateralAccelerationStructure

instance IsMPSQuadrilateralAccelerationStructure (Id MPSQuadrilateralAccelerationStructure) where
  toMPSQuadrilateralAccelerationStructure = unsafeCastId

instance IsMPSAccelerationStructure (Id MPSQuadrilateralAccelerationStructure) where
  toMPSAccelerationStructure = unsafeCastId

instance IsMPSKernel (Id MPSQuadrilateralAccelerationStructure) where
  toMPSKernel = unsafeCastId

instance IsMPSPolygonAccelerationStructure (Id MPSQuadrilateralAccelerationStructure) where
  toMPSPolygonAccelerationStructure = unsafeCastId

instance IsNSObject (Id MPSQuadrilateralAccelerationStructure) where
  toNSObject = unsafeCastId

-- ---------- MPSTriangleAccelerationStructure ----------

-- | An acceleration structure built over triangles
--
-- See MPSPolygonAccelerationStructure for more information
-- 
-- Phantom type for @MPSTriangleAccelerationStructure@.
data MPSTriangleAccelerationStructure

instance IsObjCObject (Id MPSTriangleAccelerationStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSTriangleAccelerationStructure"

class IsMPSPolygonAccelerationStructure a => IsMPSTriangleAccelerationStructure a where
  toMPSTriangleAccelerationStructure :: a -> Id MPSTriangleAccelerationStructure

instance IsMPSTriangleAccelerationStructure (Id MPSTriangleAccelerationStructure) where
  toMPSTriangleAccelerationStructure = unsafeCastId

instance IsMPSAccelerationStructure (Id MPSTriangleAccelerationStructure) where
  toMPSAccelerationStructure = unsafeCastId

instance IsMPSKernel (Id MPSTriangleAccelerationStructure) where
  toMPSKernel = unsafeCastId

instance IsMPSPolygonAccelerationStructure (Id MPSTriangleAccelerationStructure) where
  toMPSPolygonAccelerationStructure = unsafeCastId

instance IsNSObject (Id MPSTriangleAccelerationStructure) where
  toNSObject = unsafeCastId

-- ---------- MPSImageAdd ----------

-- | MPSImageAdd
--
-- This depends on Metal.framework.
--
-- Specifies the addition operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) + (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSImageAdd@.
data MPSImageAdd

instance IsObjCObject (Id MPSImageAdd) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageAdd"

class IsMPSImageArithmetic a => IsMPSImageAdd a where
  toMPSImageAdd :: a -> Id MPSImageAdd

instance IsMPSImageAdd (Id MPSImageAdd) where
  toMPSImageAdd = unsafeCastId

instance IsMPSBinaryImageKernel (Id MPSImageAdd) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSImageArithmetic (Id MPSImageAdd) where
  toMPSImageArithmetic = unsafeCastId

instance IsMPSKernel (Id MPSImageAdd) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageAdd) where
  toNSObject = unsafeCastId

-- ---------- MPSImageDivide ----------

-- | MPSImageDivide
--
-- This depends on Metal.framework.
--
-- Specifies the division operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) / (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSImageDivide@.
data MPSImageDivide

instance IsObjCObject (Id MPSImageDivide) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageDivide"

class IsMPSImageArithmetic a => IsMPSImageDivide a where
  toMPSImageDivide :: a -> Id MPSImageDivide

instance IsMPSImageDivide (Id MPSImageDivide) where
  toMPSImageDivide = unsafeCastId

instance IsMPSBinaryImageKernel (Id MPSImageDivide) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSImageArithmetic (Id MPSImageDivide) where
  toMPSImageArithmetic = unsafeCastId

instance IsMPSKernel (Id MPSImageDivide) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageDivide) where
  toNSObject = unsafeCastId

-- ---------- MPSImageMultiply ----------

-- | MPSImageMultiply
--
-- This depends on Metal.framework.
--
-- Specifies the multiplication operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) * (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSImageMultiply@.
data MPSImageMultiply

instance IsObjCObject (Id MPSImageMultiply) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageMultiply"

class IsMPSImageArithmetic a => IsMPSImageMultiply a where
  toMPSImageMultiply :: a -> Id MPSImageMultiply

instance IsMPSImageMultiply (Id MPSImageMultiply) where
  toMPSImageMultiply = unsafeCastId

instance IsMPSBinaryImageKernel (Id MPSImageMultiply) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSImageArithmetic (Id MPSImageMultiply) where
  toMPSImageArithmetic = unsafeCastId

instance IsMPSKernel (Id MPSImageMultiply) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageMultiply) where
  toNSObject = unsafeCastId

-- ---------- MPSImageSubtract ----------

-- | MPSImageSubtract
--
-- This depends on Metal.framework.
--
-- Specifies the subtraction operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) - (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSImageSubtract@.
data MPSImageSubtract

instance IsObjCObject (Id MPSImageSubtract) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageSubtract"

class IsMPSImageArithmetic a => IsMPSImageSubtract a where
  toMPSImageSubtract :: a -> Id MPSImageSubtract

instance IsMPSImageSubtract (Id MPSImageSubtract) where
  toMPSImageSubtract = unsafeCastId

instance IsMPSBinaryImageKernel (Id MPSImageSubtract) where
  toMPSBinaryImageKernel = unsafeCastId

instance IsMPSImageArithmetic (Id MPSImageSubtract) where
  toMPSImageArithmetic = unsafeCastId

instance IsMPSKernel (Id MPSImageSubtract) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSImageSubtract) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNAdd ----------

-- | MPSCNNAdd
--
-- This depends on Metal.framework.
--
-- Specifies the addition operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) + (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSCNNAdd@.
data MPSCNNAdd

instance IsObjCObject (Id MPSCNNAdd) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNAdd"

class IsMPSCNNArithmetic a => IsMPSCNNAdd a where
  toMPSCNNAdd :: a -> Id MPSCNNAdd

instance IsMPSCNNAdd (Id MPSCNNAdd) where
  toMPSCNNAdd = unsafeCastId

instance IsMPSCNNArithmetic (Id MPSCNNAdd) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNAdd) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNAdd) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNAdd) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDivide ----------

-- | MPSCNNDivide
--
-- This depends on Metal.framework.
--
-- Specifies the division operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) / (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSCNNDivide@.
data MPSCNNDivide

instance IsObjCObject (Id MPSCNNDivide) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDivide"

class IsMPSCNNArithmetic a => IsMPSCNNDivide a where
  toMPSCNNDivide :: a -> Id MPSCNNDivide

instance IsMPSCNNDivide (Id MPSCNNDivide) where
  toMPSCNNDivide = unsafeCastId

instance IsMPSCNNArithmetic (Id MPSCNNDivide) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNDivide) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNDivide) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNDivide) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNMultiply ----------

-- | MPSCNNMultiply
--
-- This depends on Metal.framework.
--
-- Specifies the multiplication operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) * (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSCNNMultiply@.
data MPSCNNMultiply

instance IsObjCObject (Id MPSCNNMultiply) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNMultiply"

class IsMPSCNNArithmetic a => IsMPSCNNMultiply a where
  toMPSCNNMultiply :: a -> Id MPSCNNMultiply

instance IsMPSCNNMultiply (Id MPSCNNMultiply) where
  toMPSCNNMultiply = unsafeCastId

instance IsMPSCNNArithmetic (Id MPSCNNMultiply) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNMultiply) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNMultiply) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNMultiply) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSubtract ----------

-- | MPSCNNSubtract
--
-- This depends on Metal.framework.
--
-- Specifies the subtraction operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) - (secondaryScale * y)) + bias.
-- 
-- Phantom type for @MPSCNNSubtract@.
data MPSCNNSubtract

instance IsObjCObject (Id MPSCNNSubtract) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSubtract"

class IsMPSCNNArithmetic a => IsMPSCNNSubtract a where
  toMPSCNNSubtract :: a -> Id MPSCNNSubtract

instance IsMPSCNNSubtract (Id MPSCNNSubtract) where
  toMPSCNNSubtract = unsafeCastId

instance IsMPSCNNArithmetic (Id MPSCNNSubtract) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNSubtract) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSubtract) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSubtract) where
  toNSObject = unsafeCastId

-- ---------- MPSNNCompare ----------

-- | MPSNNCompare
--
-- This depends on Metal.framework.
--
-- Specifies the elementwise comparison operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = (abs(x-y)) <= threshold
-- 
-- Phantom type for @MPSNNCompare@.
data MPSNNCompare

instance IsObjCObject (Id MPSNNCompare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNCompare"

class IsMPSCNNArithmetic a => IsMPSNNCompare a where
  toMPSNNCompare :: a -> Id MPSNNCompare

instance IsMPSNNCompare (Id MPSNNCompare) where
  toMPSNNCompare = unsafeCastId

instance IsMPSCNNArithmetic (Id MPSNNCompare) where
  toMPSCNNArithmetic = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNCompare) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNCompare) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNCompare) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNArithmeticGradient ----------

-- | MPSCNNArithmeticGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNArithmeticGradient filter is the backward filter for the MPSCNNArithmetic              forward filter.
--
-- The forward filter takes two inputs, primary and secondary source images, and produces              a single output image. Thus, going backwards requires two separate filters (one for              the primary source image and one for the secondary source image) that take multiple              inputs and produce a single output. The secondarySourceFilter property is used to              indicate whether the filter is operating on the primary or secondary source image from              the forward pass.
--
-- All the arithmetic gradient filters require the following inputs: gradient image from              the previous layer (going backwards) and all the applicable input source images from              the forward pass.
--
-- The forward filter takes the following additional parameters:              - primaryStrideInPixelsX, primaryStrideInPixelsY, primaryStrideInFeatureChannels              - secondaryStrideInPixelsX, secondaryStrideInPixelsY, secondaryStrideInFeatureChannels              These parameters can be used in the forward filter to control broadcasting for the data              stored in the primary and secondary source images. For example, setting all strides for              the primary source image to 0 will result in the primarySource image being treated as a              single pixel. The only supported values are 0 or 1. The default value of these parameters              is 1.
--
-- The first input to the backward filter is the gradient image from the previous layer              (going backwards), so there are no broadcasting parameters for this input. For the              backward filter, the broadcasting parameters for the second input must match the              broadcasting parameters set for the same image in the forward filter.
--
-- In the backward pass, broadcasting results in a reduction operation (sum) across all of the              applicable broadcasting dimensions (rows, columns, feature channels, or any combination              thereof) to produce the destination image of the size that matches the primary/secondary              input images used in the forward pass.
--
-- In the case of no broadcasting, the following arithmetic gradient operations are copy              operations (that can be optimized away by the graph interface):              - Add (primarySource, secondarySource)              - Subtract (primarySource)
--
-- Similarly to the forward filter, this backward filter takes additional parameters:              primaryScale, secondaryScale, and bias. The default value for primaryScale and secondaryScale              is 1.0f. The default value for bias is 0.0f. This filter applies primaryScale to the primary              source image, applies the secondaryScale to the secondary source image, where appropriate,              and applies bias to the result, i.e.:              result = ((primaryScale * x) [insert operation] (secondaryScale * y)) + bias.
--
-- The subtraction gradient filter for the secondary source image requires that the primaryScale              property is set to -1.0f (for x - y, d/dy(x - y) = -1).
--
-- In the forward filter, there is support for clamping the result of the available operations,              where result = clamp(result, minimumValue, maximumValue). The clamp backward operation is              not supported in the arithmetic gradient filters. If you require this functionality, it can              be implemented by performing a clamp backward operation before calling the arithmetic gradient              filters. You would need to apply the following function on the incomping gradient input image:              f(x) = ((minimumValue < x) && (x < maximumValue)) ? 1 : 0, where x is the original result              (before clamping) of the forward arithmetic filter.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- You must use one of the sub-classes of MPSImageArithmeticGradient.
-- 
-- Phantom type for @MPSCNNArithmeticGradient@.
data MPSCNNArithmeticGradient

instance IsObjCObject (Id MPSCNNArithmeticGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNArithmeticGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNArithmeticGradient a where
  toMPSCNNArithmeticGradient :: a -> Id MPSCNNArithmeticGradient

instance IsMPSCNNArithmeticGradient (Id MPSCNNArithmeticGradient) where
  toMPSCNNArithmeticGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNArithmeticGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNArithmeticGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNArithmeticGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNArithmeticGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationGradient ----------

-- | MPSCNNBatchNormalizationGradient
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationGradient computes the gradients of a              loss function resulting from a network containing a corresponding              MPSCNNBatchNormalization kernel.
--
-- Two sets of values are computed: the gradient of the loss function              with respect to the batch normalization source images, and the              gradient of the loss function with respect to the scale and bias              terms used to compute the batch normalization.
-- 
-- Phantom type for @MPSCNNBatchNormalizationGradient@.
data MPSCNNBatchNormalizationGradient

instance IsObjCObject (Id MPSCNNBatchNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNBatchNormalizationGradient a where
  toMPSCNNBatchNormalizationGradient :: a -> Id MPSCNNBatchNormalizationGradient

instance IsMPSCNNBatchNormalizationGradient (Id MPSCNNBatchNormalizationGradient) where
  toMPSCNNBatchNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNBatchNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNBatchNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBatchNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBatchNormalizationStatisticsGradient ----------

-- | MPSCNNBatchNormalizationStatisticsGradient
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationStatisticsGradient updates a MPSCNNBatchNormalizationState              with the gradient of the loss function with respect to the batch statistics and              batch normalization weights used to perform a batch normalization.
-- 
-- Phantom type for @MPSCNNBatchNormalizationStatisticsGradient@.
data MPSCNNBatchNormalizationStatisticsGradient

instance IsObjCObject (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBatchNormalizationStatisticsGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNBatchNormalizationStatisticsGradient a where
  toMPSCNNBatchNormalizationStatisticsGradient :: a -> Id MPSCNNBatchNormalizationStatisticsGradient

instance IsMPSCNNBatchNormalizationStatisticsGradient (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toMPSCNNBatchNormalizationStatisticsGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBatchNormalizationStatisticsGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionGradient ----------

-- | MPSCNNConvolutionGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionGradient implementents backward propagation of gradient i.e. it computes the gradient of loss function              with respect input data of corresonding forward convolution and gradient of loss function with respect to weights and bias              of corresponding convolution in forward pass.
--
-- Gradient with respect to data              ==============================              Gradient with respect to input data of corresponding forward convolution will be written in destination image passed to              encode call of MPSCNNConvolutionGradient.              This step is similar to convolution transpose in that the strided convolution in forward pass become zero filled convolution in              backward propagation of gradients. The difference between MPSCNNConvolutionTranspose and gradient wrt data is how the              weights, that are provided by data source, are interpreted. MPSCNNConvolution and MPSCNNConvolutionTranspose interpret weights              provided by data source as                                          weights[outputFeatureChannels][kernelWidth][kernelHeight][inputFeatureChannels]              whereas convoution gradient with respect to data interpret the weights as                                          weights[inputFeatureChannels][kernelWidth][kernelHeight][outputFeatureChannels]              i.e. weights are transposed in inputFeatureChannels/outputFeatureChannels dimension and also rotated 180 degress in spatial dimension
--
-- User should use the same data source provider to initialize MPSCNNConvolutionGradient as is used to initialize corresponding              forward MPSCNNConvolution. Implementation will do the transposition/shuffling needed.              Thus, while the forward MPSCNNConvolution takes sourceImage of inputFeatureChannels and convolves it with              Wt[outputFeatureChannels][kernelHeight][kernelWidth][inputFeatureChannels] to produce destinationImage of outputFeatureChannels,              MPSConvolutionGradient takes sourceGradient of outputFeatureChannels which is out of previous layer (nomally neuron backward layer),              convolves it with transposed and rotated weights and produces destinationGradient of inputFeatureChannels.              If the user decide to double buffer data source provider i.e. different data source providers are passed to forward MPSCNNConvolution object and              corresponding MPSCNNConvolutionGradient object, it is user responsibility to make sure both data source providers provide same weights/bias data              and have same properties in convolution descriptor else behavior is undefined.
--
-- Gradient with respect to weights and bias              =========================================              Gradient with respect to weights and bias are returned in MPSCNNConvolutionGradientState object to be used in weights update functions.              If I denotes the input image to corresponding MPSCNNConvolution in forward pass and E denoates the loss gradient from previous layer              (normally neuron backward layer) in backward pass, gradient of E with respect to weights is
--
-- delta_E/delta_Wkpqc = sum_i sum_j [ E(i, j, k) * I( secondaryStrideInPixelX*i + secondaryOffset.x + secondaryDilationRateX*p,                                                                  secondaryStrideinPixelY*i + secondaryOffset.y + secondaryDilationRateY*q, c) ]
--
-- where i goes over 0..W-1 and j goes over 0..H-1, (W,H) being width and height of E.              p in [0, secondaryKernelWidth-1]              q in [0, secondaryKernelHeight-1]              c in [0, inputeFeatureChannels/groups - 1]              k in [0, outputFeatureChannels]
--
-- and gradient with respect to bias
--
-- delta_E/delta_bk = sum_i sum_j [ E(i, j, k) ]
--
-- These gradients with respect to weights and bias are returned as buffers in MPSCNNConvolutionGradientState object passed in the encode call.              These are consumed by MPSCNNConvolution object's -updateWeightsAndBias:MPSCNNConvolutionGradientState* method for CPU side update and              encodeWeightsAndBiasUpdate:commandBuffer:MPSCNNConvolutionGradientState* method of MPSCNNConvolution object for GPU side update.              UPdated weights and biases are computed as
--
-- Wkpqc_new = Wkpqc_old + delta_E/delta_Wkpqc                         bk_new = bk_old + delta_E/delta_bk
--
-- Note that MPSCNNConvolutionGradientState objects's buffers that contain gradients, for CPU side update, will only contain              valid data after command buffer is complete so              its only makes sense to call -updateWeightsAndBias method on MPSCNNConvolution objects after command bufer is              complete. One can achieve this by enqueueing a command buffer completion handler block that make this call.              Since MPSCNNConvolutionGradientState is used across command buffers i.e. its created in forward pass, consumed by  MPSCNNConvolutionGradient in backward pass in same command buffer and passed onto MPSCNNConvolution updateWeightsAndBias method              after completion of command buffer, it cannot be a temporary state.
--
-- In order to gaurantee consistency between forward pass (MPSCNNConvolution) and weights gradient computation in this filter, certain requirements              must be met.              1) Dimensions of loss gradient E from previous layer in backward pass must be equal to clipRect.size of corresponding MPSCNNConvolution in forward pass.                 This is to gaurantee that only those pixels for which weights/bias contributed in destination of forward pass end up contributing to weights/bias gradient update.                 If the dimension of loss gradient E from previous layer is not equal to clipRect.size of corresponding forward MPSCNNConvolution,                    i) one can insert a slice operation to extract out the region of size clipRect.size from appropriate offset in E and set primaryOffset = 0 Or                   ii) set primatryOffset to offset in E at which valid data starts and make sure data outside is zeroed.              2) secondaryOffset should be set to what offset property of MPSCNNConvolution was set to in forward pass.
--
-- Currently back propagation for gradients is only supported for regualar convolution and depthwise convolution. Back propagation              sub-pixel convolution are not supported. So channelMultiplier and subPixelScaleFactor must be one.
--
-- Note on setting correct offsets  ===============================              If the forward convolution is called with                              offset = _offset; kernelWidth = kW; kernelHeight = kH; strideInPixelsX = sX; strideInPixelsY = sY;                              dilationRateX = dX; dilationRateY = dY;              thus dilated filter parameters are                              kW_Dilated = (kW - 1)*dX + 1; kH_Dilated = (kH - 1)*dY + 1;              Then the correct offset can be computed as follows.              Convoluton Gradient with Data              =============================                Convolution gradient with data of forward convolution with stride > 1 is essentially normal convoution with unit stride,                on an image that is formed by inserting strideInPixelsX-1 zeros in between each column and strideInPixelsY-1 zeros in between each                row of input gradient (output gradient of last layer) with kernel weights that are rotated by 180 degrees in spatial dimension (MPSCNNConvolutionGradient                does this rotation internally). primaryOffset property defines offset in original input gradient coordinate system. In order to                translate it in zero filled intermediate image coordinate system, kernelOffsetX and kernelOffsetY properties can be used as follows                       offsetInZeroFilledImageX = primaryOffset.x * primaryStrideInPixelsX + kernelOffsetX;                       offsetInZeroFilledImageY = primaryOffset.y * primaryStrideInPixelsY + kernelOffsetY;                This is what internally MPSCNNConvolutionGradient do. In order to correctly match forward convolution offset setting (so that padding policy is                consistent), application should set                       primaryOffset.x = 0; primaryOffset.y = 0;                       kernelOffset.x = -_offset.x + (~(NSInteger) kW_Dilated & 1L);                       kernelOffset.y = -_offset.y + (~(NSInteger) kH_Dilated & 1L);                Convolution gradient with data does not use secondaryOffset.
--
-- Convolution Gradient with Weights and Biases                ============================================                 For consistent padding policy with respect to forward convolution,                       secondaryOffset.x = _offset.x - kW_Dilated/2                       secondaryOffset.y = _offset.y - kH_Dilated/2                 Convolution gradient with weights and biases does not use primaryOffset (or it is assumed to be zero) as summation is over entire                 gradient image and only gradient image without any padding is currently accepted. If previous layer produces gradient image with                 padding, slice operation should be used to extract out the gradient which will be input to MPSCNNConvolutionGradient.
--
-- Note that if application uses encode method that return destination gradient on left hand side and consumes MPSCNNConvolutionGradientState             object produced by forward MPSCNNConvolution, all these parameters are set automatically for the application i.e. applicaiton does not             need to worry about setting these.
-- 
-- Phantom type for @MPSCNNConvolutionGradient@.
data MPSCNNConvolutionGradient

instance IsObjCObject (Id MPSCNNConvolutionGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNConvolutionGradient a where
  toMPSCNNConvolutionGradient :: a -> Id MPSCNNConvolutionGradient

instance IsMPSCNNConvolutionGradient (Id MPSCNNConvolutionGradient) where
  toMPSCNNConvolutionGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNConvolutionGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNConvolutionGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNConvolutionGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTransposeGradient ----------

-- | MPSCNNConvolutionTransposeGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionTransposeGradient implementents backward propagation of gradient for MPSCNNConvolutionTranspose forward filter
-- 
-- Phantom type for @MPSCNNConvolutionTransposeGradient@.
data MPSCNNConvolutionTransposeGradient

instance IsObjCObject (Id MPSCNNConvolutionTransposeGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTransposeGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNConvolutionTransposeGradient a where
  toMPSCNNConvolutionTransposeGradient :: a -> Id MPSCNNConvolutionTransposeGradient

instance IsMPSCNNConvolutionTransposeGradient (Id MPSCNNConvolutionTransposeGradient) where
  toMPSCNNConvolutionTransposeGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNConvolutionTransposeGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNConvolutionTransposeGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNConvolutionTransposeGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTransposeGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNCrossChannelNormalizationGradient ----------

-- | MPSCNNCrossChannelNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the normalization gradient filter across feature channels.               This normalization filter applies the filter to a local region across nearby feature channels,              but with no spatial extent (i.e., they have shape kernelSize x 1 x 1).              The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,              where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where              N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- OutputGradient:                  dZ/dX(i,j,k) = dZ/dY(i,j,k) * (L(i,j,k)^-beta) - 2 * alpha * beta * X(i,j,k) * ( sum_{r in R(k)} dZ/dY(i,j,r) * X(i,j,r) * (L(i,j,r) ^ (-beta-1)) )              N is the kernel size. The window L(i) and K(j) itself is defined as:                  R(k) = [max(0, k-floor((N-1)/2)), min(D-1, k+floor(N/2)]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
-- 
-- Phantom type for @MPSCNNCrossChannelNormalizationGradient@.
data MPSCNNCrossChannelNormalizationGradient

instance IsObjCObject (Id MPSCNNCrossChannelNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNCrossChannelNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNCrossChannelNormalizationGradient a where
  toMPSCNNCrossChannelNormalizationGradient :: a -> Id MPSCNNCrossChannelNormalizationGradient

instance IsMPSCNNCrossChannelNormalizationGradient (Id MPSCNNCrossChannelNormalizationGradient) where
  toMPSCNNCrossChannelNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNCrossChannelNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNCrossChannelNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNCrossChannelNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNCrossChannelNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDropoutGradient ----------

-- | MPSCNNDropoutGradient
--
-- This depends on Metal.framework
--
-- This filter is the backward filter for the MPSCNNDropout forward filter.              It requires the mask data, along with all the associated parameters used              to generate the mask, from the forward pass. The mask is associated with              a MPSCNNDropoutGradientState object.
--
-- In this kernel, use the secondaryOffset to apply an offset to the mask data.
-- 
-- Phantom type for @MPSCNNDropoutGradient@.
data MPSCNNDropoutGradient

instance IsObjCObject (Id MPSCNNDropoutGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDropoutGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNDropoutGradient a where
  toMPSCNNDropoutGradient :: a -> Id MPSCNNDropoutGradient

instance IsMPSCNNDropoutGradient (Id MPSCNNDropoutGradient) where
  toMPSCNNDropoutGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNDropoutGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNDropoutGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNDropoutGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNDropoutGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNGroupNormalizationGradient ----------

-- | MPSCNNGroupNormalizationGradient
--
-- This depends on Metal.framework
--
-- This kernel executes a gradient pass corresponding to MPSCNNGroupNormalization.
-- 
-- Phantom type for @MPSCNNGroupNormalizationGradient@.
data MPSCNNGroupNormalizationGradient

instance IsObjCObject (Id MPSCNNGroupNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNGroupNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNGroupNormalizationGradient a where
  toMPSCNNGroupNormalizationGradient :: a -> Id MPSCNNGroupNormalizationGradient

instance IsMPSCNNGroupNormalizationGradient (Id MPSCNNGroupNormalizationGradient) where
  toMPSCNNGroupNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNGroupNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNGroupNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNGroupNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNGroupNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNInstanceNormalizationGradient ----------

-- | MPSCNNInstanceNormalizationGradient
--
-- This depends on Metal.framework
--
-- This kernel executes a gradient pass corresponding to MPSCNNInstanceNormalization.
-- 
-- Phantom type for @MPSCNNInstanceNormalizationGradient@.
data MPSCNNInstanceNormalizationGradient

instance IsObjCObject (Id MPSCNNInstanceNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNInstanceNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNInstanceNormalizationGradient a where
  toMPSCNNInstanceNormalizationGradient :: a -> Id MPSCNNInstanceNormalizationGradient

instance IsMPSCNNInstanceNormalizationGradient (Id MPSCNNInstanceNormalizationGradient) where
  toMPSCNNInstanceNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNInstanceNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNInstanceNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNInstanceNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNInstanceNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLocalContrastNormalizationGradient ----------

-- | MPSCNNLocalContrastNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the local contrast normalization gradient filter.              The local contrast normalization is quite similar to spatial normalization              (see MPSCNNSpatialNormalization) in that it applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight),              but instead of dividing by the local "energy" of the feature, the denominator uses the local variance              of the feature - effectively the mean value of the feature is subtracted from the signal.              For each feature channel, the function computes the variance VAR(i,j) and              mean M(i,j) of X(i,j) inside each rectangle around the spatial point (i,j).
--
-- Then the result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / (delta + alpha * VAR(i,j))^beta,
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. For example setting              pm=0, ps=1, p0=1, delta=0, alpha=1.0 and beta=0.5 scales input data so that the result has              unit variance and zero mean, provided that input variance is positive.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined. A good way to guard              against tiny variances is to regulate the expression with a small value for delta, for example              delta = 1/1024 = 0.0009765625.
--
-- T(i,j) = (delta + alpha * VAR(i,j))              N      = kw * kh
--
-- OutputGradient:                  dZ/dX(i,j) =  ps * T(i,j)^(-beta) * ( dZ/dY(i,j) - (sum_{l,k in L(i),K(j)} dZ/dY(l,k) * (((p0/N) + (2*alpha*beta/N)*(X(k,l)-1)*(X(i,j)-M(i,j)*p0)/T(i,j)))) )              N is the kernel size. The window L(i) and K(j) itself is defined as:                  L(i) = [i-floor((kw-1)/2), i+floor(kw/2]                  K(j) = [j-floor((kh-1)/2), j+floor(kh/2]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
-- 
-- Phantom type for @MPSCNNLocalContrastNormalizationGradient@.
data MPSCNNLocalContrastNormalizationGradient

instance IsObjCObject (Id MPSCNNLocalContrastNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLocalContrastNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNLocalContrastNormalizationGradient a where
  toMPSCNNLocalContrastNormalizationGradient :: a -> Id MPSCNNLocalContrastNormalizationGradient

instance IsMPSCNNLocalContrastNormalizationGradient (Id MPSCNNLocalContrastNormalizationGradient) where
  toMPSCNNLocalContrastNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNLocalContrastNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNLocalContrastNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNLocalContrastNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNLocalContrastNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNLogSoftMaxGradient ----------

-- | MPSCNNLogSoftMaxGradient
--
-- This depends on Metal.framework
--
-- The logSoftMax gradient filter calculates the gradient to be backpropagated.              The logSoftMax gradient just as the log softMax filter, is applied across feature channels and at all spatial locations.              It computes the gradient for a given output generated by the corresponding logSoftMax (i.e. MPSCNNLogSoftMax) layer and              the gradient computed by the previous layer in the back-propagation pass.              For each feature channel per pixel in an image in a feature map, the logSoftMax gradient filter computes the following:                  result gradient channel in pixel                      outputGradient(x,y,k) = inputGradient(x,y,k) - exp(logSoftMax(x,y,k)) * sum(inputGradient(x,y,0) ... inputGradient(x,y,N-1))                      where N is the number of feature channels
--
-- The incoming gradient is the primary source.              The original output of corresponding logSoftMax is the secondary source.
-- 
-- Phantom type for @MPSCNNLogSoftMaxGradient@.
data MPSCNNLogSoftMaxGradient

instance IsObjCObject (Id MPSCNNLogSoftMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNLogSoftMaxGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNLogSoftMaxGradient a where
  toMPSCNNLogSoftMaxGradient :: a -> Id MPSCNNLogSoftMaxGradient

instance IsMPSCNNLogSoftMaxGradient (Id MPSCNNLogSoftMaxGradient) where
  toMPSCNNLogSoftMaxGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNLogSoftMaxGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNLogSoftMaxGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNLogSoftMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNLogSoftMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronGradient ----------

-- | MPSCNNNeuronGradient
--
-- This depends on Metal.framework
--
-- This filter is a backward filter for the neuron activation function filter.
--
-- The following filter types are supported:  MPSCNNNeuronTypeNone            ///< df/dx = 1  MPSCNNNeuronTypeLinear          ///< df/dx = a  MPSCNNNeuronTypeReLU            ///< df/dx = [ 1, if x >= 0                                               [ a, if x <  0  MPSCNNNeuronTypeSigmoid         ///< df/dx = e^x / (e^x + 1)^2  MPSCNNNeuronTypeHardSigmoid     ///< df/dx = [ a, if ((x * a) + b >= 0) and ((x * a) + b <= 1)                                               [ 0, otherwise  MPSCNNNeuronTypeTanH            ///< df/dx = a * b * (1 - tanh^2(b * x))  MPSCNNNeuronTypeAbsolute        ///< df/dx = sign(x)  MPSCNNNeuronTypeSoftPlus        ///< df/dx = (a * b * exp(b * x)) / (exp(b * x) + 1)  MPSCNNNeuronTypeSoftSign        ///< df/dx = 1 / (|x| + 1)^2  MPSCNNNeuronTypeELU             ///< df/dx = [ a * exp(x), x <  0                                               [          1, x >= 0  MPSCNNNeuronTypePReLU           ///< df/dx = [  1, if x >= 0                                               [ aV, if x <  0  MPSCNNNeuronTypeReLUN           ///< df/dx = [ 1, if x >= 0                                               [ a, if x <  0                                               [ b, if x >= b  MPSCNNNeuronTypePower           ///< df/dx = a * c * (a * x + b)^(c - 1)  MPSCNNNeuronTypeExponential     ///< df/dx = [         a * exp(a * x + b), if c == -1                                               [ a * log(c) * c^(a * x + b), if c != -1  MPSCNNNeuronTypeLogarithm       ///< df/dx = [            a / (a * in + b), if c == -1                                               [ a / (log(c) * (a * in + b)), if c != -1  MPSCNNNeuronTypeGeLU            ///< df/dx = 0.5 * (1.0 + erf(x * sqrt(0.5))) + (sqrt(0.5) * M_2_SQRTPI * exp(-x*x * 0.5) * x) )
--
-- The result of the above operation is multiplied with the gradient, computed by the preceeding filter (going backwards).
-- 
-- Phantom type for @MPSCNNNeuronGradient@.
data MPSCNNNeuronGradient

instance IsObjCObject (Id MPSCNNNeuronGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNNeuronGradient a where
  toMPSCNNNeuronGradient :: a -> Id MPSCNNNeuronGradient

instance IsMPSCNNNeuronGradient (Id MPSCNNNeuronGradient) where
  toMPSCNNNeuronGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNNeuronGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNNeuronGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingGradient ----------

-- | MPSCNNPoolingGradient
--
-- This depends on Metal.framework
--
-- Specifies the base class for computing the gradient of the pooling filters.              The operation backpropagates a gradient vector using the chain rule.
--
-- Given the input gradient vector dL(x) = dL/d out(x), which is the derivative of the              loss-function wrt. (original) pooling filter output the output gradient at position y              (dL/d in(y)) is computed as follows:
--
-- dL/d in(y) = sum_x (dL/d out(x)) * (d out(x)/d in(y)), where
--
-- the sum runs over the input gradient pixels starting from primaryOffset              extending to primaryOffset + sourceSize. Note here that we need a separate              variable 'sourceSize' to specify which input gradients are included in the output              gradient computation as this information cannot be deduced directly from the cliprect              size due to fractional striding or simply because the user wants to examine a subset              of the contributions to the gradients. In normal operation the sourceSize is specified              as the cliprect.size of the forward pooling filter in order to compute the gradients for              all outputs the forward direction produced and the primaryOffset is set to              cliprect.origin of the original forward pooling operation for the same reason.
--
-- The cliprect property of the filter allows the user to send the gradients to a new location,              which may not match the original forward pooling filter window locations:              The index 'y' in the formula above refers to the pixel location in the secondary source,              which is the source image of the original forward pooling filter and secondaryOffset specifies              the center of the first pooling window as specified in MPSCNNPooling filter specification.              The first (top leftmost) pixel written into the cliprect computes the derivative of the first pixel              within the first pooling window that is contained within the secondary source image and              subsequent values are defined by normal striding rules from secondary source to primary source.              This means that typically the cliprect is set to fill the effective source area of the original forward              operation, clamped to edges of the original source image, which in the normal case is the same size              as the size of the gradient destination image.
--
-- If there are any values in the destination cliprect that do not contribute to the forward              pooling result in the area specified by primaryOffset and sourceSize,              due to large strides or dilation factors or simply because all forward pass induced values would be              outside the source area, then those result values are set to zero.
--
-- The actual value of d out(x) / d in(y) depends on the pooling operation and these are defined in the              subclasses of MPSCNNPoolingGradient.
-- 
-- Phantom type for @MPSCNNPoolingGradient@.
data MPSCNNPoolingGradient

instance IsObjCObject (Id MPSCNNPoolingGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNPoolingGradient a where
  toMPSCNNPoolingGradient :: a -> Id MPSCNNPoolingGradient

instance IsMPSCNNPoolingGradient (Id MPSCNNPoolingGradient) where
  toMPSCNNPoolingGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNPoolingGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNPoolingGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSoftMaxGradient ----------

-- | MPSCNNSoftMaxGradient
--
-- This depends on Metal.framework
--
-- The softMax gradient filter calculates the gradient to be backpropagated.              The softMax gradient just as the softMax filter, is applied across feature channels and at all spatial locations.              It computes the gradient for a given output generated by the corresponding softMax (i.e. MPSCNNSoftMax) layer and              the gradient computed by the previous layer in the back-propagation pass.              For each feature channel in an image in a feature map, the softMax gradient filter computes the following:                  result gradient channel in pixel                      outputGradient(x,y,k) = softMax(x,y,k) * (inputGradient(x,y,k) -                                               sum(inputGradient(x,y,0) * softMax(x,y,0) ... inputGradient(x,y,N-1) * softMax(x,y,N-1)))                      where N is the number of feature channels
--
-- The incoming gradient is the primary source.              The original output of corresponding softMax is the secondary source.
-- 
-- Phantom type for @MPSCNNSoftMaxGradient@.
data MPSCNNSoftMaxGradient

instance IsObjCObject (Id MPSCNNSoftMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSoftMaxGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNSoftMaxGradient a where
  toMPSCNNSoftMaxGradient :: a -> Id MPSCNNSoftMaxGradient

instance IsMPSCNNSoftMaxGradient (Id MPSCNNSoftMaxGradient) where
  toMPSCNNSoftMaxGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNSoftMaxGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNSoftMaxGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSoftMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSoftMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSpatialNormalizationGradient ----------

-- | MPSCNNSpatialNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the spatial normalization gradient filter.              The spatial normalization for a feature channel applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight).              For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).              It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,              where kw and kh are the kernelWidth and the kernelHeight.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- T(i,j) = (delta + alpha/(kw*kh) * N2(i,j))              N      = kw * kh
--
-- OutputGradient:                  dZ/dX(i,j) =  T(i,j)^(-beta) * ( dZ/dY(i,j) - (2*alpha*beta*X(i,j)/T(i,j)) * (sum_{l,k in L(i),K(j)} dZ/dY(l,k)*X(l,k)) )              N is the kernel size. The window R(k) itself is defined as:                  L(i) = [i-floor((kw-1)/2), i+floor(kw/2]                  K(j) = [j-floor((kh-1)/2), j+floor(kh/2]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
-- 
-- Phantom type for @MPSCNNSpatialNormalizationGradient@.
data MPSCNNSpatialNormalizationGradient

instance IsObjCObject (Id MPSCNNSpatialNormalizationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSpatialNormalizationGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNSpatialNormalizationGradient a where
  toMPSCNNSpatialNormalizationGradient :: a -> Id MPSCNNSpatialNormalizationGradient

instance IsMPSCNNSpatialNormalizationGradient (Id MPSCNNSpatialNormalizationGradient) where
  toMPSCNNSpatialNormalizationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNSpatialNormalizationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNSpatialNormalizationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSpatialNormalizationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSpatialNormalizationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingGradient ----------

-- | MPSCNNUpsamplingGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNUpsamplingGradient filter is used for training. It is the backward              filter for the MPSCNNUpsampling filter. It operates on the gradient input,              specifically, it reduces the size of the gradient input in the x and y dimensions.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- The scaleFactor must be an integer value >= 1. The default value is 1.              If scaleFactor == 1, the filter acts as a copy kernel.
--
-- Nearest and bilinear variants are supported.
--
-- For example, for the nearest variant with scaleFactorX = scaleFactorY = 2, the              forward pass produced the following output:
--
-- Input:	    Output:                          a a b b              a b         a a b b              c d         c c d d                          c c d d
--
-- To upsample the image, the input data is replicated.
--
-- And, the backward pass for the above froward pass is computed in the following              way:
--
-- Input:		    Output:              a1 a2 b1 b2              a2 a3 b3 b4	    x y              c1 c2 d1 d2	    z w              c3 c4 d3 d4
--
-- where	x = a1 + a2 + a3 + a4                      y = b1 + b2 + b3 + b4                      z = c1 + c2 + c3 + c4                      w = d1 + d2 + d3 + d4
-- 
-- Phantom type for @MPSCNNUpsamplingGradient@.
data MPSCNNUpsamplingGradient

instance IsObjCObject (Id MPSCNNUpsamplingGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingGradient"

class IsMPSCNNGradientKernel a => IsMPSCNNUpsamplingGradient a where
  toMPSCNNUpsamplingGradient :: a -> Id MPSCNNUpsamplingGradient

instance IsMPSCNNUpsamplingGradient (Id MPSCNNUpsamplingGradient) where
  toMPSCNNUpsamplingGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNUpsamplingGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNUpsamplingGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsamplingGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNGramMatrixCalculationGradient ----------

-- | MPSNNGramMatrixCalculationGradient
--
-- This depends on Metal.framework
--
-- The MPSNNGramMatrixCalculationGradient defines the gradient filter for MPSNNGramMatrixCalculation.
-- 
-- Phantom type for @MPSNNGramMatrixCalculationGradient@.
data MPSNNGramMatrixCalculationGradient

instance IsObjCObject (Id MPSNNGramMatrixCalculationGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNGramMatrixCalculationGradient"

class IsMPSCNNGradientKernel a => IsMPSNNGramMatrixCalculationGradient a where
  toMPSNNGramMatrixCalculationGradient :: a -> Id MPSNNGramMatrixCalculationGradient

instance IsMPSNNGramMatrixCalculationGradient (Id MPSNNGramMatrixCalculationGradient) where
  toMPSNNGramMatrixCalculationGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNGramMatrixCalculationGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSNNGramMatrixCalculationGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNGramMatrixCalculationGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNGramMatrixCalculationGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNPadGradient ----------

-- | MPSNNPadGradient
--
-- This depends on Metal.framework
--
-- Computes the gradient for the MPSNNPad layer.              Since the padding forward operation typically increases the size of the image, the gradient operation              decreases it. In case of zero or constant padding forward operation the gradient operation slices the              input gradient and in other edge modes the padded values copied in the forward operation are              summed together in the gradient operation.              For Example for the MPSImageEdgeModeClamp the forward operation with offset = -2, destSize = 8              or paddingSizeBefore = 2, paddingSizeAfter = 3, sourceSize = 3:
--
-- Source Image:
-- |--------------|
-- | x0 | x1 | x2 |
-- |--------------|
-- Destination Image:
-- |---------------------------------------|
-- | x0 | x0 | x0 | x1 | x2 | x2 | x2 | x2 |
-- |---------------------------------------|
--
-- Then the gradient operation becomes:
--
-- Source Gradient Image:
-- |---------------------------------------|
-- | d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 |
-- |---------------------------------------|
-- Destination Gradient Image:
-- |-----------------------------|
-- | d0+d1+d2 | d3 | d4+d5+d6+d7 |
-- |-----------------------------|
--
-- Another example with MPSImageEdgeModeMirror, the forward operation with offset = -4, destSize = 8              or paddingSizeBefore = 4, paddingSizeAfter = 1, sourceSize = 3:
--
-- Source Image:
-- |--------------|
-- | x0 | x1 | x2 |
-- |--------------|
-- Destination Image:
-- |---------------------------------------|
-- | x0 | x1 | x2 | x1 | x0 | x1 | x2 | x1 |
-- |---------------------------------------|
--
-- Then the gradient operation becomes:
--
-- Source Gradient Image:
-- |---------------------------------------|
-- | d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 |
-- |---------------------------------------|
-- Destination Gradient Image:
-- |-----------------------------|
-- | d0+d4 | d1+d3+d5+d7 | d2+d6 |
-- |-----------------------------|
--
-- NOTE: There are no channel fill-values to use with MPSImageEdgeModeConstant              since the gradient values are independent of the constant of the forward pass.              NOTE: In case the forward pass defined a slice operation in feature channels then              the channels not read in the forward pass will be filled with zeros in the gradient pass.
-- 
-- Phantom type for @MPSNNPadGradient@.
data MPSNNPadGradient

instance IsObjCObject (Id MPSNNPadGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNPadGradient"

class IsMPSCNNGradientKernel a => IsMPSNNPadGradient a where
  toMPSNNPadGradient :: a -> Id MPSNNPadGradient

instance IsMPSNNPadGradient (Id MPSNNPadGradient) where
  toMPSNNPadGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNPadGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSNNPadGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNPadGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNPadGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReshapeGradient ----------

-- | MPSNNReshapeGradient
--
-- This depends on Metal.framework
--
-- The reshape gradient filter reshapes the incoming gradient into the dimensions              of the forward reshape kernel's source image.
-- 
-- Phantom type for @MPSNNReshapeGradient@.
data MPSNNReshapeGradient

instance IsObjCObject (Id MPSNNReshapeGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReshapeGradient"

class IsMPSCNNGradientKernel a => IsMPSNNReshapeGradient a where
  toMPSNNReshapeGradient :: a -> Id MPSNNReshapeGradient

instance IsMPSNNReshapeGradient (Id MPSNNReshapeGradient) where
  toMPSNNReshapeGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNReshapeGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSNNReshapeGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReshapeGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSNNReshapeGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNNLocalCorrelation ----------

-- | MPSNNLocalCorrelation
--
-- The MPSNNLocalCorrelation filter computes the correlation between two images locally with a              varying offset on x-y plane between the two source images (controlled by the window and              stride properties) and the end result is summed over the feature channels. The results are              stored in the different feature channels of the destination image, ordered such that the offset              in the x direction is the faster running index.
--
-- Given two images A and B, the output image has (2*windowInX + 1)*(2*windowInY + 1)              feature channels, with each feature channel computed as:                                  O(x, y, f(m, n)) = sum_z{A(x, y, z) * B(x + M[m], y + N[n], z)}              where m runs from {0, 1, ... , (2*windowInX)}, n runs from {0, 1, ... , (2*windowInY)},              f(m, n) = n * (2*windowInY + 1) + m,              M = {-windowInX*strideInX, (-windowInX + 1)*strideInX,  ... 0 ... , (windowInX - 1)*strideInX, windowInX*strideInX},              N = {-windowInY*strideInY, (-windowInY + 1)*strideInY,  ... 0 ... , (windowInY - 1)*strideInY, windowInX*strideInY}
-- 
-- Phantom type for @MPSNNLocalCorrelation@.
data MPSNNLocalCorrelation

instance IsObjCObject (Id MPSNNLocalCorrelation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNLocalCorrelation"

class IsMPSNNReduceBinary a => IsMPSNNLocalCorrelation a where
  toMPSNNLocalCorrelation :: a -> Id MPSNNLocalCorrelation

instance IsMPSNNLocalCorrelation (Id MPSNNLocalCorrelation) where
  toMPSNNLocalCorrelation = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNLocalCorrelation) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNLocalCorrelation) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceBinary (Id MPSNNLocalCorrelation) where
  toMPSNNReduceBinary = unsafeCastId

instance IsNSObject (Id MPSNNLocalCorrelation) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsAndWeightsMean ----------

-- | Phantom type for @MPSNNReduceFeatureChannelsAndWeightsMean@.
data MPSNNReduceFeatureChannelsAndWeightsMean

instance IsObjCObject (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsAndWeightsMean"

class IsMPSNNReduceBinary a => IsMPSNNReduceFeatureChannelsAndWeightsMean a where
  toMPSNNReduceFeatureChannelsAndWeightsMean :: a -> Id MPSNNReduceFeatureChannelsAndWeightsMean

instance IsMPSNNReduceFeatureChannelsAndWeightsMean (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toMPSNNReduceFeatureChannelsAndWeightsMean = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceBinary (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toMPSNNReduceBinary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsAndWeightsMean) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsAndWeightsSum ----------

-- | Phantom type for @MPSNNReduceFeatureChannelsAndWeightsSum@.
data MPSNNReduceFeatureChannelsAndWeightsSum

instance IsObjCObject (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsAndWeightsSum"

class IsMPSNNReduceBinary a => IsMPSNNReduceFeatureChannelsAndWeightsSum a where
  toMPSNNReduceFeatureChannelsAndWeightsSum :: a -> Id MPSNNReduceFeatureChannelsAndWeightsSum

instance IsMPSNNReduceFeatureChannelsAndWeightsSum (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toMPSNNReduceFeatureChannelsAndWeightsSum = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceBinary (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toMPSNNReduceBinary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsAndWeightsSum) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBinaryFullyConnected ----------

-- | MPSCNNBinaryFullyConnected
--
-- This depends on Metal.framework
--
-- The MPSCNNBinaryFullyConnected specifies a fully connected convolution layer with binary weights              and optionally binarized input image.              See MPSCNNFullyConnected for details on the fully connected layer and              MPSCNNBinaryConvolution for binary convolutions.
--
-- The default padding policy for MPSCNNBinaryConvolution is different from most               filters. It uses MPSNNPaddingMethodSizeValidOnly instead of MPSNNPaddingMethodSizeSame.
-- 
-- Phantom type for @MPSCNNBinaryFullyConnected@.
data MPSCNNBinaryFullyConnected

instance IsObjCObject (Id MPSCNNBinaryFullyConnected) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBinaryFullyConnected"

class IsMPSCNNBinaryConvolution a => IsMPSCNNBinaryFullyConnected a where
  toMPSCNNBinaryFullyConnected :: a -> Id MPSCNNBinaryFullyConnected

instance IsMPSCNNBinaryFullyConnected (Id MPSCNNBinaryFullyConnected) where
  toMPSCNNBinaryFullyConnected = unsafeCastId

instance IsMPSCNNBinaryConvolution (Id MPSCNNBinaryFullyConnected) where
  toMPSCNNBinaryConvolution = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNBinaryFullyConnected) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNBinaryFullyConnected) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNBinaryFullyConnected) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNFullyConnected ----------

-- | MPSCNNFullyConnected
--
-- This depends on Metal.framework
--
-- The MPSCNNFullyConnected specifies a fully connected convolution layer a.k.a. Inner product              layer. A fully connected CNN layer is one where every input channel is connected              to every output channel. The kernel width is equal to width of source image              and the kernel height is equal to the height of source image. Width and height of the output              is 1x1. Thus, it takes a srcW x srcH x Ni MPSCNNImage, convolves it with Weights[No][SrcW][srcH][Ni]              and produces a 1 x 1 x No output. The following must be true:
--
-- kernelWidth  == source.width
-- kernelHeight == source.height
-- clipRect.size.width == 1
-- clipRect.size.height == 1
--
-- One can think of a fully connected layer as a matrix multiplication that flattens an image into a vector of length              srcW*srcH*Ni. The weights are arragned in a matrix of dimension No x (srcW*srcH*Ni) for product output vectors              of length No. The strideInPixelsX, strideInPixelsY, and group must be 1. Offset is not applicable and is ignored.              Since clipRect is clamped to the destination image bounds, if the destination is 1x1, one doesn't need to set the              clipRect.
--
-- Note that one can implement an inner product using MPSCNNConvolution by setting
--
-- offset = (kernelWidth/2,kernelHeight/2)
-- clipRect.origin = (ox,oy), clipRect.size = (1,1)
-- strideX = strideY = group = 1
--
-- However, using the MPSCNNFullyConnected for this is better for performance as it lets us choose the most              performant method which may not be possible when using a general convolution. For example,              we may internally use matrix multiplication or special reduction kernels for a specific platform.
-- 
-- Phantom type for @MPSCNNFullyConnected@.
data MPSCNNFullyConnected

instance IsObjCObject (Id MPSCNNFullyConnected) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNFullyConnected"

class IsMPSCNNConvolution a => IsMPSCNNFullyConnected a where
  toMPSCNNFullyConnected :: a -> Id MPSCNNFullyConnected

instance IsMPSCNNFullyConnected (Id MPSCNNFullyConnected) where
  toMPSCNNFullyConnected = unsafeCastId

instance IsMPSCNNConvolution (Id MPSCNNFullyConnected) where
  toMPSCNNConvolution = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNFullyConnected) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNFullyConnected) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNFullyConnected) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronAbsolute ----------

-- | MPSCNNNeuronAbsolute
--
-- This depends on Metal.framework
--
-- Specifies the absolute neuron filter.  For each pixel, applies the following function: f(x) = | x |
-- 
-- Phantom type for @MPSCNNNeuronAbsolute@.
data MPSCNNNeuronAbsolute

instance IsObjCObject (Id MPSCNNNeuronAbsolute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronAbsolute"

class IsMPSCNNNeuron a => IsMPSCNNNeuronAbsolute a where
  toMPSCNNNeuronAbsolute :: a -> Id MPSCNNNeuronAbsolute

instance IsMPSCNNNeuronAbsolute (Id MPSCNNNeuronAbsolute) where
  toMPSCNNNeuronAbsolute = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronAbsolute) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronAbsolute) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronAbsolute) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronAbsolute) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronELU ----------

-- | MPSCNNNeuronELU
--
-- This depends on Metal.framework
--
-- Specifies the parametric ELU neuron filter.              For each pixel, applies the following function: f(x) = [ a * (exp(x) - 1), x <  0                                                                     [ x               , x >= 0
-- 
-- Phantom type for @MPSCNNNeuronELU@.
data MPSCNNNeuronELU

instance IsObjCObject (Id MPSCNNNeuronELU) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronELU"

class IsMPSCNNNeuron a => IsMPSCNNNeuronELU a where
  toMPSCNNNeuronELU :: a -> Id MPSCNNNeuronELU

instance IsMPSCNNNeuronELU (Id MPSCNNNeuronELU) where
  toMPSCNNNeuronELU = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronELU) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronELU) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronELU) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronELU) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronExponential ----------

-- | MPSCNNNeuronExponential
--
-- This depends on Metal.framework.
--
-- Specifies the Exponential neuron filter.              For each pixel, applies the following function: f(x) = c ^ (a * x + b).
--
-- If the value of c is -1.0f, the base (c) is set to e.
-- 
-- Phantom type for @MPSCNNNeuronExponential@.
data MPSCNNNeuronExponential

instance IsObjCObject (Id MPSCNNNeuronExponential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronExponential"

class IsMPSCNNNeuron a => IsMPSCNNNeuronExponential a where
  toMPSCNNNeuronExponential :: a -> Id MPSCNNNeuronExponential

instance IsMPSCNNNeuronExponential (Id MPSCNNNeuronExponential) where
  toMPSCNNNeuronExponential = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronExponential) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronExponential) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronExponential) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronExponential) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronHardSigmoid ----------

-- | MPSCNNNeuronHardSigmoid
--
-- This depends on Metal.framework
--
-- Specifies the hard sigmoid neuron filter.  For each pixel, applies the following function: f(x) = clamp((a * x) + b, 0, 1)
-- 
-- Phantom type for @MPSCNNNeuronHardSigmoid@.
data MPSCNNNeuronHardSigmoid

instance IsObjCObject (Id MPSCNNNeuronHardSigmoid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronHardSigmoid"

class IsMPSCNNNeuron a => IsMPSCNNNeuronHardSigmoid a where
  toMPSCNNNeuronHardSigmoid :: a -> Id MPSCNNNeuronHardSigmoid

instance IsMPSCNNNeuronHardSigmoid (Id MPSCNNNeuronHardSigmoid) where
  toMPSCNNNeuronHardSigmoid = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronHardSigmoid) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronHardSigmoid) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronHardSigmoid) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronHardSigmoid) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronLinear ----------

-- | MPSCNNNeuronLinear
--
-- This depends on Metal.framework
--
-- Specifies the linear neuron filter. For each pixel, applies the following function: f(x) = a * x + b
-- 
-- Phantom type for @MPSCNNNeuronLinear@.
data MPSCNNNeuronLinear

instance IsObjCObject (Id MPSCNNNeuronLinear) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronLinear"

class IsMPSCNNNeuron a => IsMPSCNNNeuronLinear a where
  toMPSCNNNeuronLinear :: a -> Id MPSCNNNeuronLinear

instance IsMPSCNNNeuronLinear (Id MPSCNNNeuronLinear) where
  toMPSCNNNeuronLinear = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronLinear) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronLinear) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronLinear) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronLinear) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronLogarithm ----------

-- | MPSCNNNeuronLogarithm
--
-- This depends on Metal.framework.
--
-- Specifies the Logarithm neuron filter.              For each pixel, applies the following function: f(x) = log_c(a * x + b).
--
-- If the value of c is -1.0f, the base (c) is set to e.
-- 
-- Phantom type for @MPSCNNNeuronLogarithm@.
data MPSCNNNeuronLogarithm

instance IsObjCObject (Id MPSCNNNeuronLogarithm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronLogarithm"

class IsMPSCNNNeuron a => IsMPSCNNNeuronLogarithm a where
  toMPSCNNNeuronLogarithm :: a -> Id MPSCNNNeuronLogarithm

instance IsMPSCNNNeuronLogarithm (Id MPSCNNNeuronLogarithm) where
  toMPSCNNNeuronLogarithm = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronLogarithm) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronLogarithm) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronLogarithm) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronLogarithm) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronPReLU ----------

-- | MPSCNNNeuronPReLU
--
-- This depends on Metal.framework
--
-- Specifies the parametric ReLU neuron filter.              For each pixel, applies the following function: f(x_i) = x_i, if x_i >= 0                                                                     = a_i * x_i if x_i < 0              i in [0...channels-1]              i.e. parameters a_i are learned and applied to each channel separately. Compare              this to ReLu where parameter a is shared across all channels.              See https://arxiv.org/pdf/1502.01852.pdf for details.
-- 
-- Phantom type for @MPSCNNNeuronPReLU@.
data MPSCNNNeuronPReLU

instance IsObjCObject (Id MPSCNNNeuronPReLU) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronPReLU"

class IsMPSCNNNeuron a => IsMPSCNNNeuronPReLU a where
  toMPSCNNNeuronPReLU :: a -> Id MPSCNNNeuronPReLU

instance IsMPSCNNNeuronPReLU (Id MPSCNNNeuronPReLU) where
  toMPSCNNNeuronPReLU = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronPReLU) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronPReLU) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronPReLU) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronPReLU) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronPower ----------

-- | MPSCNNNeuronPower
--
-- This depends on Metal.framework.
--
-- Specifies the Power neuron filter.              For each pixel, applies the following function: f(x) = (a * x + b) ^ c.
-- 
-- Phantom type for @MPSCNNNeuronPower@.
data MPSCNNNeuronPower

instance IsObjCObject (Id MPSCNNNeuronPower) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronPower"

class IsMPSCNNNeuron a => IsMPSCNNNeuronPower a where
  toMPSCNNNeuronPower :: a -> Id MPSCNNNeuronPower

instance IsMPSCNNNeuronPower (Id MPSCNNNeuronPower) where
  toMPSCNNNeuronPower = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronPower) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronPower) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronPower) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronPower) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronReLU ----------

-- | MPSCNNNeuronReLU
--
-- This depends on Metal.framework
--
-- Specifies the ReLU neuron filter.              For each pixel, applies the following function: f(x) = x, if x >= 0                                                                   = a * x if x < 0              This is called Leaky ReLU in literature. Some literature defines              classical ReLU as max(0, x). If you want this behavior, simply pass a = 0
-- 
-- Phantom type for @MPSCNNNeuronReLU@.
data MPSCNNNeuronReLU

instance IsObjCObject (Id MPSCNNNeuronReLU) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronReLU"

class IsMPSCNNNeuron a => IsMPSCNNNeuronReLU a where
  toMPSCNNNeuronReLU :: a -> Id MPSCNNNeuronReLU

instance IsMPSCNNNeuronReLU (Id MPSCNNNeuronReLU) where
  toMPSCNNNeuronReLU = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronReLU) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronReLU) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronReLU) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronReLU) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronReLUN ----------

-- | MPSCNNNeuronReLUN
--
-- This depends on Metal.framework
--
-- Specifies the ReLUN neuron filter.              For each pixel, applies the following function: f(x) = [ x    , x >= 0                                                                     [ a * x, x <  0                                                                     [ b    , x >= b              As an example, the TensorFlow Relu6 activation layer can be implemented              by setting the parameter b to 6.0f:              https://www.tensorflow.org/api_docs/cc/class/tensorflow/ops/relu6.
-- 
-- Phantom type for @MPSCNNNeuronReLUN@.
data MPSCNNNeuronReLUN

instance IsObjCObject (Id MPSCNNNeuronReLUN) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronReLUN"

class IsMPSCNNNeuron a => IsMPSCNNNeuronReLUN a where
  toMPSCNNNeuronReLUN :: a -> Id MPSCNNNeuronReLUN

instance IsMPSCNNNeuronReLUN (Id MPSCNNNeuronReLUN) where
  toMPSCNNNeuronReLUN = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronReLUN) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronReLUN) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronReLUN) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronReLUN) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSigmoid ----------

-- | MPSCNNNeuronSigmoid
--
-- This depends on Metal.framework
--
-- Specifies the sigmoid neuron filter.  For each pixel, applies the following function: f(x) = 1 / (1 + e^-x)
-- 
-- Phantom type for @MPSCNNNeuronSigmoid@.
data MPSCNNNeuronSigmoid

instance IsObjCObject (Id MPSCNNNeuronSigmoid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSigmoid"

class IsMPSCNNNeuron a => IsMPSCNNNeuronSigmoid a where
  toMPSCNNNeuronSigmoid :: a -> Id MPSCNNNeuronSigmoid

instance IsMPSCNNNeuronSigmoid (Id MPSCNNNeuronSigmoid) where
  toMPSCNNNeuronSigmoid = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronSigmoid) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronSigmoid) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronSigmoid) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSigmoid) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSoftPlus ----------

-- | MPSCNNNeuronSoftPlus
--
-- This depends on Metal.framework
--
-- Specifies the parametric softplus neuron filter.              For each pixel, applies the following function: f(x) = a * log(1 + e^(b * x))
-- 
-- Phantom type for @MPSCNNNeuronSoftPlus@.
data MPSCNNNeuronSoftPlus

instance IsObjCObject (Id MPSCNNNeuronSoftPlus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSoftPlus"

class IsMPSCNNNeuron a => IsMPSCNNNeuronSoftPlus a where
  toMPSCNNNeuronSoftPlus :: a -> Id MPSCNNNeuronSoftPlus

instance IsMPSCNNNeuronSoftPlus (Id MPSCNNNeuronSoftPlus) where
  toMPSCNNNeuronSoftPlus = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronSoftPlus) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronSoftPlus) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronSoftPlus) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSoftPlus) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronSoftSign ----------

-- | MPSCNNNeuronSoftSign
--
-- This depends on Metal.framework
--
-- Specifies the softsign neuron filter.              For each pixel, applies the following function: f(x) = x / (1 + abs(x))
-- 
-- Phantom type for @MPSCNNNeuronSoftSign@.
data MPSCNNNeuronSoftSign

instance IsObjCObject (Id MPSCNNNeuronSoftSign) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronSoftSign"

class IsMPSCNNNeuron a => IsMPSCNNNeuronSoftSign a where
  toMPSCNNNeuronSoftSign :: a -> Id MPSCNNNeuronSoftSign

instance IsMPSCNNNeuronSoftSign (Id MPSCNNNeuronSoftSign) where
  toMPSCNNNeuronSoftSign = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronSoftSign) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronSoftSign) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronSoftSign) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronSoftSign) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNNeuronTanH ----------

-- | MPSCNNNeuronTanH
--
-- This depends on Metal.framework
--
-- Specifies the hyperbolic tangent neuron filter.              For each pixel, applies the following function: f(x) = a * tanh(b * x)
-- 
-- Phantom type for @MPSCNNNeuronTanH@.
data MPSCNNNeuronTanH

instance IsObjCObject (Id MPSCNNNeuronTanH) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNNeuronTanH"

class IsMPSCNNNeuron a => IsMPSCNNNeuronTanH a where
  toMPSCNNNeuronTanH :: a -> Id MPSCNNNeuronTanH

instance IsMPSCNNNeuronTanH (Id MPSCNNNeuronTanH) where
  toMPSCNNNeuronTanH = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNNeuronTanH) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNNeuron (Id MPSCNNNeuronTanH) where
  toMPSCNNNeuron = unsafeCastId

instance IsMPSKernel (Id MPSCNNNeuronTanH) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNNeuronTanH) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDilatedPoolingMax ----------

-- | MPSCNNDilatedPoolingMax
--
-- This depends on Metal.framework
--
-- Specifies the dilated max pooling filter.  For each pixel, returns the maximum value of pixels              in the kernelWidth x kernelHeight filter region by step size dilationRateX x dilationRateY.
-- 
-- Phantom type for @MPSCNNDilatedPoolingMax@.
data MPSCNNDilatedPoolingMax

instance IsObjCObject (Id MPSCNNDilatedPoolingMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDilatedPoolingMax"

class IsMPSCNNPooling a => IsMPSCNNDilatedPoolingMax a where
  toMPSCNNDilatedPoolingMax :: a -> Id MPSCNNDilatedPoolingMax

instance IsMPSCNNDilatedPoolingMax (Id MPSCNNDilatedPoolingMax) where
  toMPSCNNDilatedPoolingMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNDilatedPoolingMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNPooling (Id MPSCNNDilatedPoolingMax) where
  toMPSCNNPooling = unsafeCastId

instance IsMPSKernel (Id MPSCNNDilatedPoolingMax) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNDilatedPoolingMax) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingAverage ----------

-- | MPSCNNPoolingAverage
--
-- This depends on Metal.framework
--
-- Specifies the average pooling filter.  For each pixel, returns the mean value of pixels              in the kernelWidth x kernelHeight filter region.              When edgeMode is MPSImageEdgeModeClamp the filtering window is shrunk to remain              within the source image borders. What this means is that close to image borders the filtering window              will be smaller in order to fit inside the source image and less values will be used to compute the              average. In case the filtering window is entirely outside the source image border the              outputted value will be zero.
-- 
-- Phantom type for @MPSCNNPoolingAverage@.
data MPSCNNPoolingAverage

instance IsObjCObject (Id MPSCNNPoolingAverage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingAverage"

class IsMPSCNNPooling a => IsMPSCNNPoolingAverage a where
  toMPSCNNPoolingAverage :: a -> Id MPSCNNPoolingAverage

instance IsMPSCNNPoolingAverage (Id MPSCNNPoolingAverage) where
  toMPSCNNPoolingAverage = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNPoolingAverage) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNPooling (Id MPSCNNPoolingAverage) where
  toMPSCNNPooling = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingAverage) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingAverage) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingL2Norm ----------

-- | MPSCNNPoolingL2Norm
--
-- This depends on Metal.framework
--
-- Specifies the L2-norm pooling filter.  For each pixel, returns L2-Norm of pixels              in the kernelWidth x kernelHeight filter region.                  out[c,x,y] = sqrt ( sum_{dx,dy} in[c,x+dx,y+dy] * in[c,x+dx,y+dy] ).
-- 
-- Phantom type for @MPSCNNPoolingL2Norm@.
data MPSCNNPoolingL2Norm

instance IsObjCObject (Id MPSCNNPoolingL2Norm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingL2Norm"

class IsMPSCNNPooling a => IsMPSCNNPoolingL2Norm a where
  toMPSCNNPoolingL2Norm :: a -> Id MPSCNNPoolingL2Norm

instance IsMPSCNNPoolingL2Norm (Id MPSCNNPoolingL2Norm) where
  toMPSCNNPoolingL2Norm = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNPoolingL2Norm) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNPooling (Id MPSCNNPoolingL2Norm) where
  toMPSCNNPooling = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingL2Norm) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingL2Norm) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingMax ----------

-- | MPSCNNPoolingMax
--
-- This depends on Metal.framework
--
-- Specifies the max pooling filter.  For each pixel, returns the maximum value of pixels              in the kernelWidth x kernelHeight filter region.
-- 
-- Phantom type for @MPSCNNPoolingMax@.
data MPSCNNPoolingMax

instance IsObjCObject (Id MPSCNNPoolingMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingMax"

class IsMPSCNNPooling a => IsMPSCNNPoolingMax a where
  toMPSCNNPoolingMax :: a -> Id MPSCNNPoolingMax

instance IsMPSCNNPoolingMax (Id MPSCNNPoolingMax) where
  toMPSCNNPoolingMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNPoolingMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNPooling (Id MPSCNNPoolingMax) where
  toMPSCNNPooling = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingMax) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingMax) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingBilinear ----------

-- | MPSCNNUpsamplingBilinear
--
-- This depends on Metal.framework.
--
-- Specifies the bilinear spatial upsampling filter.
-- 
-- Phantom type for @MPSCNNUpsamplingBilinear@.
data MPSCNNUpsamplingBilinear

instance IsObjCObject (Id MPSCNNUpsamplingBilinear) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingBilinear"

class IsMPSCNNUpsampling a => IsMPSCNNUpsamplingBilinear a where
  toMPSCNNUpsamplingBilinear :: a -> Id MPSCNNUpsamplingBilinear

instance IsMPSCNNUpsamplingBilinear (Id MPSCNNUpsamplingBilinear) where
  toMPSCNNUpsamplingBilinear = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNUpsamplingBilinear) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNUpsampling (Id MPSCNNUpsamplingBilinear) where
  toMPSCNNUpsampling = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsamplingBilinear) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingBilinear) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingNearest ----------

-- | MPSCNNUpsamplingNearest
--
-- This depends on Metal.framework.
--
-- Specifies the nearest spatial upsampling filter.
-- 
-- Phantom type for @MPSCNNUpsamplingNearest@.
data MPSCNNUpsamplingNearest

instance IsObjCObject (Id MPSCNNUpsamplingNearest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingNearest"

class IsMPSCNNUpsampling a => IsMPSCNNUpsamplingNearest a where
  toMPSCNNUpsamplingNearest :: a -> Id MPSCNNUpsamplingNearest

instance IsMPSCNNUpsamplingNearest (Id MPSCNNUpsamplingNearest) where
  toMPSCNNUpsamplingNearest = unsafeCastId

instance IsMPSCNNKernel (Id MPSCNNUpsamplingNearest) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSCNNUpsampling (Id MPSCNNUpsamplingNearest) where
  toMPSCNNUpsampling = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsamplingNearest) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingNearest) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceColumnMax ----------

-- | MPSNNReduceColumnMax
--
-- The MPSNNReduceColumnMax performs a reduction operation returning the maximum value for each column of an image
-- 
-- Phantom type for @MPSNNReduceColumnMax@.
data MPSNNReduceColumnMax

instance IsObjCObject (Id MPSNNReduceColumnMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceColumnMax"

class IsMPSNNReduceUnary a => IsMPSNNReduceColumnMax a where
  toMPSNNReduceColumnMax :: a -> Id MPSNNReduceColumnMax

instance IsMPSNNReduceColumnMax (Id MPSNNReduceColumnMax) where
  toMPSNNReduceColumnMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceColumnMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceColumnMax) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceColumnMax) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceColumnMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceColumnMean ----------

-- | MPSNNReduceColumnMean
--
-- The MPSNNReduceColumnMean performs a reduction operation returning the mean value for each column of an image
-- 
-- Phantom type for @MPSNNReduceColumnMean@.
data MPSNNReduceColumnMean

instance IsObjCObject (Id MPSNNReduceColumnMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceColumnMean"

class IsMPSNNReduceUnary a => IsMPSNNReduceColumnMean a where
  toMPSNNReduceColumnMean :: a -> Id MPSNNReduceColumnMean

instance IsMPSNNReduceColumnMean (Id MPSNNReduceColumnMean) where
  toMPSNNReduceColumnMean = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceColumnMean) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceColumnMean) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceColumnMean) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceColumnMean) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceColumnMin ----------

-- | MPSNNReduceColumnMin
--
-- The MPSNNReduceColumnMin performs a reduction operation returning the mininmum value for each column of an image
-- 
-- Phantom type for @MPSNNReduceColumnMin@.
data MPSNNReduceColumnMin

instance IsObjCObject (Id MPSNNReduceColumnMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceColumnMin"

class IsMPSNNReduceUnary a => IsMPSNNReduceColumnMin a where
  toMPSNNReduceColumnMin :: a -> Id MPSNNReduceColumnMin

instance IsMPSNNReduceColumnMin (Id MPSNNReduceColumnMin) where
  toMPSNNReduceColumnMin = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceColumnMin) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceColumnMin) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceColumnMin) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceColumnMin) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceColumnSum ----------

-- | MPSNNReduceColumnSum
--
-- The MPSNNReduceColumnSum performs a reduction operation returning the sum for each column of an image
-- 
-- Phantom type for @MPSNNReduceColumnSum@.
data MPSNNReduceColumnSum

instance IsObjCObject (Id MPSNNReduceColumnSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceColumnSum"

class IsMPSNNReduceUnary a => IsMPSNNReduceColumnSum a where
  toMPSNNReduceColumnSum :: a -> Id MPSNNReduceColumnSum

instance IsMPSNNReduceColumnSum (Id MPSNNReduceColumnSum) where
  toMPSNNReduceColumnSum = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceColumnSum) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceColumnSum) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceColumnSum) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceColumnSum) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsArgumentMax ----------

-- | MPSNNReduceFeatureChannelsArgumentMax
--
-- The MPSNNReduceFeatureChannelsArgumentMax performs returns the argument index that is the              location of the maximum value for feature channels of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsArgumentMax@.
data MPSNNReduceFeatureChannelsArgumentMax

instance IsObjCObject (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsArgumentMax"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsArgumentMax a where
  toMPSNNReduceFeatureChannelsArgumentMax :: a -> Id MPSNNReduceFeatureChannelsArgumentMax

instance IsMPSNNReduceFeatureChannelsArgumentMax (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toMPSNNReduceFeatureChannelsArgumentMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsArgumentMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsArgumentMin ----------

-- | MPSNNReduceFeatureChannelsArgumentMin
--
-- The MPSNNReduceFeatureChannelsArgumentMin returns the argument index that is the              location of the minimum value for feature channels of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsArgumentMin@.
data MPSNNReduceFeatureChannelsArgumentMin

instance IsObjCObject (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsArgumentMin"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsArgumentMin a where
  toMPSNNReduceFeatureChannelsArgumentMin :: a -> Id MPSNNReduceFeatureChannelsArgumentMin

instance IsMPSNNReduceFeatureChannelsArgumentMin (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toMPSNNReduceFeatureChannelsArgumentMin = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsArgumentMin) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsMax ----------

-- | MPSNNReduceFeatureChannelsMax
--
-- The MPSNNReduceFeatureChannelsMax performs a reduction operation returning the maximum value for feature channels of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsMax@.
data MPSNNReduceFeatureChannelsMax

instance IsObjCObject (Id MPSNNReduceFeatureChannelsMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsMax"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsMax a where
  toMPSNNReduceFeatureChannelsMax :: a -> Id MPSNNReduceFeatureChannelsMax

instance IsMPSNNReduceFeatureChannelsMax (Id MPSNNReduceFeatureChannelsMax) where
  toMPSNNReduceFeatureChannelsMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsMax) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsMax) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsMean ----------

-- | MPSNNReduceFeatureChannelsMean
--
-- The MPSNNReduceFeatureChannelsMean performs a reduction operation returning the mean value for each column of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsMean@.
data MPSNNReduceFeatureChannelsMean

instance IsObjCObject (Id MPSNNReduceFeatureChannelsMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsMean"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsMean a where
  toMPSNNReduceFeatureChannelsMean :: a -> Id MPSNNReduceFeatureChannelsMean

instance IsMPSNNReduceFeatureChannelsMean (Id MPSNNReduceFeatureChannelsMean) where
  toMPSNNReduceFeatureChannelsMean = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsMean) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsMean) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsMean) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsMean) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsMin ----------

-- | MPSNNReduceFeatureChannelsMin
--
-- The MPSNNReduceFeatureChannelsMin performs a reduction operation returning the mininmum value for feature channels of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsMin@.
data MPSNNReduceFeatureChannelsMin

instance IsObjCObject (Id MPSNNReduceFeatureChannelsMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsMin"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsMin a where
  toMPSNNReduceFeatureChannelsMin :: a -> Id MPSNNReduceFeatureChannelsMin

instance IsMPSNNReduceFeatureChannelsMin (Id MPSNNReduceFeatureChannelsMin) where
  toMPSNNReduceFeatureChannelsMin = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsMin) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsMin) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsMin) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsMin) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceFeatureChannelsSum ----------

-- | MPSNNReduceFeatureChannelsSum
--
-- The MPSNNReduceFeatureChannelsSum performs a reduction operation returning the sum for each column of an image
-- 
-- Phantom type for @MPSNNReduceFeatureChannelsSum@.
data MPSNNReduceFeatureChannelsSum

instance IsObjCObject (Id MPSNNReduceFeatureChannelsSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceFeatureChannelsSum"

class IsMPSNNReduceUnary a => IsMPSNNReduceFeatureChannelsSum a where
  toMPSNNReduceFeatureChannelsSum :: a -> Id MPSNNReduceFeatureChannelsSum

instance IsMPSNNReduceFeatureChannelsSum (Id MPSNNReduceFeatureChannelsSum) where
  toMPSNNReduceFeatureChannelsSum = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceFeatureChannelsSum) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceFeatureChannelsSum) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceFeatureChannelsSum) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceFeatureChannelsSum) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceRowMax ----------

-- | MPSNNReduceRowMax
--
-- The MPSNNReduceRowMax performs a reduction operation returning the maximum value for each row of an image
-- 
-- Phantom type for @MPSNNReduceRowMax@.
data MPSNNReduceRowMax

instance IsObjCObject (Id MPSNNReduceRowMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceRowMax"

class IsMPSNNReduceUnary a => IsMPSNNReduceRowMax a where
  toMPSNNReduceRowMax :: a -> Id MPSNNReduceRowMax

instance IsMPSNNReduceRowMax (Id MPSNNReduceRowMax) where
  toMPSNNReduceRowMax = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceRowMax) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceRowMax) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceRowMax) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceRowMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceRowMean ----------

-- | MPSNNReduceRowMean
--
-- The MPSNNReduceRowMean performs a reduction operation returning the mean value for each row of an image
-- 
-- Phantom type for @MPSNNReduceRowMean@.
data MPSNNReduceRowMean

instance IsObjCObject (Id MPSNNReduceRowMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceRowMean"

class IsMPSNNReduceUnary a => IsMPSNNReduceRowMean a where
  toMPSNNReduceRowMean :: a -> Id MPSNNReduceRowMean

instance IsMPSNNReduceRowMean (Id MPSNNReduceRowMean) where
  toMPSNNReduceRowMean = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceRowMean) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceRowMean) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceRowMean) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceRowMean) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceRowMin ----------

-- | MPSNNReduceRowMin
--
-- The MPSNNReduceRowMin performs a reduction operation returning the mininmum value for each row of an image
-- 
-- Phantom type for @MPSNNReduceRowMin@.
data MPSNNReduceRowMin

instance IsObjCObject (Id MPSNNReduceRowMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceRowMin"

class IsMPSNNReduceUnary a => IsMPSNNReduceRowMin a where
  toMPSNNReduceRowMin :: a -> Id MPSNNReduceRowMin

instance IsMPSNNReduceRowMin (Id MPSNNReduceRowMin) where
  toMPSNNReduceRowMin = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceRowMin) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceRowMin) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceRowMin) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceRowMin) where
  toNSObject = unsafeCastId

-- ---------- MPSNNReduceRowSum ----------

-- | MPSNNReduceRowSum
--
-- The MPSNNReduceRowSum performs a reduction operation returning the sum for each row of an image
-- 
-- Phantom type for @MPSNNReduceRowSum@.
data MPSNNReduceRowSum

instance IsObjCObject (Id MPSNNReduceRowSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNReduceRowSum"

class IsMPSNNReduceUnary a => IsMPSNNReduceRowSum a where
  toMPSNNReduceRowSum :: a -> Id MPSNNReduceRowSum

instance IsMPSNNReduceRowSum (Id MPSNNReduceRowSum) where
  toMPSNNReduceRowSum = unsafeCastId

instance IsMPSCNNKernel (Id MPSNNReduceRowSum) where
  toMPSCNNKernel = unsafeCastId

instance IsMPSKernel (Id MPSNNReduceRowSum) where
  toMPSKernel = unsafeCastId

instance IsMPSNNReduceUnary (Id MPSNNReduceRowSum) where
  toMPSNNReduceUnary = unsafeCastId

instance IsNSObject (Id MPSNNReduceRowSum) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixLogSoftMaxGradient ----------

-- | MPSMatrixLogSoftMaxGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient corresponding to a forward MPSMatrixLogSoftMax object.
--
-- A MPSMatrixLogSoftMaxGradient object computes:
--
-- dL_dX_ij = dL_dY_ij - exp(Y_ij * sum_k(dL_dY_ik))
--
-- Where dL_dX is the resulting gradient of the loss function with respect to              the original input to the forward MPSMatrixLogSoftMax operation, Y is              the output of the forward MPSMatrixLogSoftMax operation, and dL_dY is the              gradient of the loss function with respect to Y.
-- 
-- Phantom type for @MPSMatrixLogSoftMaxGradient@.
data MPSMatrixLogSoftMaxGradient

instance IsObjCObject (Id MPSMatrixLogSoftMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixLogSoftMaxGradient"

class IsMPSMatrixSoftMaxGradient a => IsMPSMatrixLogSoftMaxGradient a where
  toMPSMatrixLogSoftMaxGradient :: a -> Id MPSMatrixLogSoftMaxGradient

instance IsMPSMatrixLogSoftMaxGradient (Id MPSMatrixLogSoftMaxGradient) where
  toMPSMatrixLogSoftMaxGradient = unsafeCastId

instance IsMPSKernel (Id MPSMatrixLogSoftMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixBinaryKernel (Id MPSMatrixLogSoftMaxGradient) where
  toMPSMatrixBinaryKernel = unsafeCastId

instance IsMPSMatrixSoftMaxGradient (Id MPSMatrixLogSoftMaxGradient) where
  toMPSMatrixSoftMaxGradient = unsafeCastId

instance IsNSObject (Id MPSMatrixLogSoftMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSMatrixLogSoftMax ----------

-- | MPSMatrixLogSoftMax
--
-- This depends on Metal.framework.
--
-- A logarithmic softmax kernel that operates on matrices.
--
-- A MPSMatrixLogSoftMax object computes:
--
-- B_ij = ln { Exp { A_ij } / ( Sum_k Exp { A_ik } ) } = A_ij - ln { Sum_k Exp { A_ik } }
--
-- A and B are matrices which are represented by MPSMatrix              objects. This filter computes the same result for MPSMatrices as              MPSCNNLogSoftMax filter does for MPSImages by interpreting the columns              of the matrix as feature channels, that is the sum runs over column indices.
-- 
-- Phantom type for @MPSMatrixLogSoftMax@.
data MPSMatrixLogSoftMax

instance IsObjCObject (Id MPSMatrixLogSoftMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSMatrixLogSoftMax"

class IsMPSMatrixSoftMax a => IsMPSMatrixLogSoftMax a where
  toMPSMatrixLogSoftMax :: a -> Id MPSMatrixLogSoftMax

instance IsMPSMatrixLogSoftMax (Id MPSMatrixLogSoftMax) where
  toMPSMatrixLogSoftMax = unsafeCastId

instance IsMPSKernel (Id MPSMatrixLogSoftMax) where
  toMPSKernel = unsafeCastId

instance IsMPSMatrixSoftMax (Id MPSMatrixLogSoftMax) where
  toMPSMatrixSoftMax = unsafeCastId

instance IsMPSMatrixUnaryKernel (Id MPSMatrixLogSoftMax) where
  toMPSMatrixUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSMatrixLogSoftMax) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayBinaryPrimaryGradientKernel ----------

-- | MPSNDArrayDivisionPrimaryGradient
--
-- This depends on Metal.framework.
-- 
-- Phantom type for @MPSNDArrayBinaryPrimaryGradientKernel@.
data MPSNDArrayBinaryPrimaryGradientKernel

instance IsObjCObject (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayBinaryPrimaryGradientKernel"

class IsMPSNDArrayMultiaryGradientKernel a => IsMPSNDArrayBinaryPrimaryGradientKernel a where
  toMPSNDArrayBinaryPrimaryGradientKernel :: a -> Id MPSNDArrayBinaryPrimaryGradientKernel

instance IsMPSNDArrayBinaryPrimaryGradientKernel (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toMPSNDArrayBinaryPrimaryGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayBinaryPrimaryGradientKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayBinarySecondaryGradientKernel ----------

-- | MPSNDArrayDivisionSecondaryGradient
--
-- This depends on Metal.framework.
-- 
-- Phantom type for @MPSNDArrayBinarySecondaryGradientKernel@.
data MPSNDArrayBinarySecondaryGradientKernel

instance IsObjCObject (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayBinarySecondaryGradientKernel"

class IsMPSNDArrayMultiaryGradientKernel a => IsMPSNDArrayBinarySecondaryGradientKernel a where
  toMPSNDArrayBinarySecondaryGradientKernel :: a -> Id MPSNDArrayBinarySecondaryGradientKernel

instance IsMPSNDArrayBinarySecondaryGradientKernel (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toMPSNDArrayBinarySecondaryGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayBinarySecondaryGradientKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayUnaryGradientKernel ----------

-- | Phantom type for @MPSNDArrayUnaryGradientKernel@.
data MPSNDArrayUnaryGradientKernel

instance IsObjCObject (Id MPSNDArrayUnaryGradientKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayUnaryGradientKernel"

class IsMPSNDArrayMultiaryGradientKernel a => IsMPSNDArrayUnaryGradientKernel a where
  toMPSNDArrayUnaryGradientKernel :: a -> Id MPSNDArrayUnaryGradientKernel

instance IsMPSNDArrayUnaryGradientKernel (Id MPSNDArrayUnaryGradientKernel) where
  toMPSNDArrayUnaryGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayUnaryGradientKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayUnaryGradientKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayUnaryGradientKernel) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayUnaryGradientKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayAffineInt4Dequantize ----------

-- | MPSNDArrayAffineInt4Dequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes an input with affine quantization scheme.
--
-- The kernel works with 2-4 inputs, order of inputs: 1) quantized input, 2) scale, 3) zeropoint, 4) minValue
-- 
-- Phantom type for @MPSNDArrayAffineInt4Dequantize@.
data MPSNDArrayAffineInt4Dequantize

instance IsObjCObject (Id MPSNDArrayAffineInt4Dequantize) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayAffineInt4Dequantize"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayAffineInt4Dequantize a where
  toMPSNDArrayAffineInt4Dequantize :: a -> Id MPSNDArrayAffineInt4Dequantize

instance IsMPSNDArrayAffineInt4Dequantize (Id MPSNDArrayAffineInt4Dequantize) where
  toMPSNDArrayAffineInt4Dequantize = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayAffineInt4Dequantize) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayAffineInt4Dequantize) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayAffineInt4Dequantize) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayAffineInt4Dequantize) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayBinaryKernel ----------

-- | Phantom type for @MPSNDArrayBinaryKernel@.
data MPSNDArrayBinaryKernel

instance IsObjCObject (Id MPSNDArrayBinaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayBinaryKernel"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayBinaryKernel a where
  toMPSNDArrayBinaryKernel :: a -> Id MPSNDArrayBinaryKernel

instance IsMPSNDArrayBinaryKernel (Id MPSNDArrayBinaryKernel) where
  toMPSNDArrayBinaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayBinaryKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayBinaryKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayBinaryKernel) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayBinaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayLUTDequantize ----------

-- | MPSNDArrayLUTDequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes a lookup-table based NDArray.
--
-- The kernel works with 2 inputs: 1) The quantized input, 2) The LookUp table array.
-- 
-- Phantom type for @MPSNDArrayLUTDequantize@.
data MPSNDArrayLUTDequantize

instance IsObjCObject (Id MPSNDArrayLUTDequantize) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayLUTDequantize"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayLUTDequantize a where
  toMPSNDArrayLUTDequantize :: a -> Id MPSNDArrayLUTDequantize

instance IsMPSNDArrayLUTDequantize (Id MPSNDArrayLUTDequantize) where
  toMPSNDArrayLUTDequantize = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayLUTDequantize) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayLUTDequantize) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayLUTDequantize) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayLUTDequantize) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayMatrixMultiplication ----------

-- | MPSNDArrayMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix multiplication kernel operating on MPSNDArray objects.
--
-- A MPSNDArrayMatrixMultiplication object computes, for each 2-D matrix within              a 4-D MPSNDArray object:
--
-- D = alpha * A * B + beta * C
--
-- A, B, C, and D are matrices which are represented by objects stored              in the two most major dimensions of the MPSNDArray. alpha and beta              are scalar values (of the same data type as values of D and C) which              are applied as shown above.
--
-- If an input's 3rd or 4th dimension is 1 its data will be broadcast as              appropriate to the remaining input's 3rd or 4th dimension respectively.
-- 
-- Phantom type for @MPSNDArrayMatrixMultiplication@.
data MPSNDArrayMatrixMultiplication

instance IsObjCObject (Id MPSNDArrayMatrixMultiplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayMatrixMultiplication"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayMatrixMultiplication a where
  toMPSNDArrayMatrixMultiplication :: a -> Id MPSNDArrayMatrixMultiplication

instance IsMPSNDArrayMatrixMultiplication (Id MPSNDArrayMatrixMultiplication) where
  toMPSNDArrayMatrixMultiplication = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayMatrixMultiplication) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayMatrixMultiplication) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayMatrixMultiplication) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayMatrixMultiplication) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayUnaryKernel ----------

-- | Phantom type for @MPSNDArrayUnaryKernel@.
data MPSNDArrayUnaryKernel

instance IsObjCObject (Id MPSNDArrayUnaryKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayUnaryKernel"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayUnaryKernel a where
  toMPSNDArrayUnaryKernel :: a -> Id MPSNDArrayUnaryKernel

instance IsMPSNDArrayUnaryKernel (Id MPSNDArrayUnaryKernel) where
  toMPSNDArrayUnaryKernel = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayUnaryKernel) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayUnaryKernel) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayUnaryKernel) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayUnaryKernel) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayVectorLUTDequantize ----------

-- | MPSNDArrayVectorLUTDequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes a lookup-table based NDArray with vector LUT support.
--
-- The kernel works with 2 inputs: 1) The quantized input, 2) The LookUp table array.
-- 
-- Phantom type for @MPSNDArrayVectorLUTDequantize@.
data MPSNDArrayVectorLUTDequantize

instance IsObjCObject (Id MPSNDArrayVectorLUTDequantize) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayVectorLUTDequantize"

class IsMPSNDArrayMultiaryKernel a => IsMPSNDArrayVectorLUTDequantize a where
  toMPSNDArrayVectorLUTDequantize :: a -> Id MPSNDArrayVectorLUTDequantize

instance IsMPSNDArrayVectorLUTDequantize (Id MPSNDArrayVectorLUTDequantize) where
  toMPSNDArrayVectorLUTDequantize = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayVectorLUTDequantize) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayVectorLUTDequantize) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayVectorLUTDequantize) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayVectorLUTDequantize) where
  toNSObject = unsafeCastId

-- ---------- MPSImageAreaMin ----------

-- | MPSImageAreaMin
--
-- The MPSImageAreaMin finds the minimum pixel value in a rectangular region centered around each pixel in the               source image. If there are multiple channels in the source image, each channel is processed independently.               It has the same methods as MPSImageAreaMax               The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
-- 
-- Phantom type for @MPSImageAreaMin@.
data MPSImageAreaMin

instance IsObjCObject (Id MPSImageAreaMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageAreaMin"

class IsMPSImageAreaMax a => IsMPSImageAreaMin a where
  toMPSImageAreaMin :: a -> Id MPSImageAreaMin

instance IsMPSImageAreaMin (Id MPSImageAreaMin) where
  toMPSImageAreaMin = unsafeCastId

instance IsMPSImageAreaMax (Id MPSImageAreaMin) where
  toMPSImageAreaMax = unsafeCastId

instance IsMPSKernel (Id MPSImageAreaMin) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageAreaMin) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageAreaMin) where
  toNSObject = unsafeCastId

-- ---------- MPSImageTent ----------

-- | MPSImageTent
--
-- The box filter, while fast, may yield square-ish looking blur effects. However, multiple              passes of the box filter tend to smooth out with each additional pass. For example, two 3-wide              box blurs produces the same effective convolution as a 5-wide tent blur:
--
-- 1   1   1
-- 1   1   1
-- +       1   1   1
-- =================
-- 1   2   3   2   1
--
-- Addition passes tend to approximate a gaussian line shape.
--
-- The MPSImageTent convolves an image with a tent filter. These form a tent shape with incrementally              increasing sides, for example:
--
-- 1   2   3   2   1
--
-- 1   2   1                  2   4   2                  1   2   1
--
-- Like the box filter, this arrangement allows for much faster algorithms, espcially for for larger blur              radii but with a more pleasing appearance.
--
-- The tent blur is a separable filter. The implementation is aware of this and will act accordingly              to give best performance for multi-dimensional blurs.
-- 
-- Phantom type for @MPSImageTent@.
data MPSImageTent

instance IsObjCObject (Id MPSImageTent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageTent"

class IsMPSImageBox a => IsMPSImageTent a where
  toMPSImageTent :: a -> Id MPSImageTent

instance IsMPSImageTent (Id MPSImageTent) where
  toMPSImageTent = unsafeCastId

instance IsMPSImageBox (Id MPSImageTent) where
  toMPSImageBox = unsafeCastId

instance IsMPSKernel (Id MPSImageTent) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageTent) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageTent) where
  toNSObject = unsafeCastId

-- ---------- MPSImageErode ----------

-- | MPSImageErode
--
-- The MPSImageErode filter finds the minimum pixel value in a rectangular region centered around each pixel in the              source image. It is like the MPSImageAreaMin, except that the intensity at each position is calculated relative              to a different value before determining which is the maximum pixel value, allowing for shaped, non-rectangular              morphological probes.
--
-- for each pixel in the filter window:
-- value =  pixel[filterY][filterX] + filter[filterY*filter_width+filterX]
-- if( value < bestValue ){
-- result = value
-- bestValue = value;
-- }
--
-- A filter that contains all zeros is identical to a MPSImageAreaMin filter. The center filter element              is assumed to be 0, to avoid causing a general lightening of the image.
--
-- The definition of the filter for MPSImageErode is different from vImage. (MPSErode_filter_value = 1.0f-vImageErode_filter_value.)              This allows MPSImageDilate and MPSImageErode to use the same filter, making open and close operators easier to write.              The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
-- 
-- Phantom type for @MPSImageErode@.
data MPSImageErode

instance IsObjCObject (Id MPSImageErode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageErode"

class IsMPSImageDilate a => IsMPSImageErode a where
  toMPSImageErode :: a -> Id MPSImageErode

instance IsMPSImageErode (Id MPSImageErode) where
  toMPSImageErode = unsafeCastId

instance IsMPSImageDilate (Id MPSImageErode) where
  toMPSImageDilate = unsafeCastId

instance IsMPSKernel (Id MPSImageErode) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageErode) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageErode) where
  toNSObject = unsafeCastId

-- ---------- MPSImageGaussianPyramid ----------

-- | MPSImageGaussianPyramid
--
-- A Gaussian image pyramid is constructed as follows:              The mipmap level zero is the source of the operation and is left untouched and              the subsequent mipmap levels are constructed from it recursively:
--
-- mip[ level = n + 1 ] = Downsample( filter( mip[ level = n ] ) ), where
--
-- "filter()" applies a filter with the specified convolution kernel and              "Downsample()" removes odd rows and columns from the input image.              The default convolution filter kernel for this operation is
--
-- k = w w^T, where w = [ 1/16,  1/4,  3/8,  1/4,  1/16 ]^T,
--
-- but the user may also tweak this kernel with a centerWeight parameter: 'a':
--
-- k = w w^T, where w = [ (1/4 - a/2),  1/4,  a,  1/4,  (1/4 - a/2) ]^T
--
-- or the user can provide a completely custom kernel.
--
-- This procedure is continued until every mipmap level present in the image texture are              filled with the pyramid levels.
--
-- In case of the Gaussian pyramid the user must run the operation in-place using:              MPSUnaryImageKernel::encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:,              where the fallback allocator is ignored.
-- 
-- Phantom type for @MPSImageGaussianPyramid@.
data MPSImageGaussianPyramid

instance IsObjCObject (Id MPSImageGaussianPyramid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageGaussianPyramid"

class IsMPSImagePyramid a => IsMPSImageGaussianPyramid a where
  toMPSImageGaussianPyramid :: a -> Id MPSImageGaussianPyramid

instance IsMPSImageGaussianPyramid (Id MPSImageGaussianPyramid) where
  toMPSImageGaussianPyramid = unsafeCastId

instance IsMPSImagePyramid (Id MPSImageGaussianPyramid) where
  toMPSImagePyramid = unsafeCastId

instance IsMPSKernel (Id MPSImageGaussianPyramid) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageGaussianPyramid) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageGaussianPyramid) where
  toNSObject = unsafeCastId

-- ---------- MPSImageLaplacianPyramid ----------

-- | MPSImageLaplacianPyramid
--
-- Laplacian pyramid levels are constructed as difference between the current source level and 2x interpolated version of the              half-resolution source level immediately above it.
--
-- LaplacianMipLevel[l] := GaussianMipLevel[l] – Interpolate(GaussianMipLevel[l + 1])
--
-- The Interpolate function is the classical 2x signal interpolation procedure applied                  to all color channels of the source mip-level in both dimensions.                  It is logically equivalent to the following two-step process :                      1) Zero-stuffing (sometimes called "upsampling").                         It is the process of interleaving source pixel values with zero values:                         dst.at(x, y) := src.at(x, y) if even(x) and even(y) else 0                      2) Filtering (sometimes called "interpolation").                         It is the same procedure as implemented by the MPSImageConvolution class,                         using filter weights provided by the initializer methods inherited from MPSImagePyramid.
--
-- The source for Laplacian pyramid construction is typically produced              by the Gaussian pyramid algorithm -- a closely related image processing technique,              but the Laplacian pyramid construction itself makes no assumptions neither about               the data stored in the source texture nor about the interpolation filter weights,              so Gaussian pyramid is just a conventional name for the source texture.
--
-- Please refer to the classical "The Laplacian Pyramid as a Compact Image Code" whitepaper               by Burt & Anderson, originally published in 532 IEEE TRANSACTIONS ON COMMUNICATIONS, VOL. COM-3l, NO. 4, APRIL 1983              for more detailed discussion.
--
-- Since the subtraction operation extends the value range of LaplacianMipLevelRaw              relative to the value range of GaussianMipLevel (even for the case of              normalized interpolation filter), in order to avoid unwanted range clamping              when working with normalized texture types, laplacianBias and laplacianScale class properties              specify point-wise linear mapping of the LaplacianMipLevelRaw result data              into the value range of the destination texture :                  LaplacianRangeScale(pixel, laplacianBias, laplacianScale) := laplacianBias + pixel * laplacianScale,                  LaplacianMipLevelStored[j]                                := LaplacianRangeScale(LaplacianMipLevel[j], laplacianBias, laplacianScale),                  with the default values being laplacianBias = 0.0, laplacianScale = 1.0
--
-- Limitations of the current software revision :                 1) In-place operation is not supported, e.g. source and destination textures need                     to have separate storage and can't be aliased.                 2) The number of channels, bit depth and resolution of the source and destination textures need to match.                 3) Values of the offset and clipRect properties are fixed to the defaults provided by MPSUnaryImageKernel                     (from which they are inherited), corresponding to no offset applied to the source and unbounded region of interest                    in every destination mip-level; all updates to these properties are ignored.
-- 
-- Phantom type for @MPSImageLaplacianPyramid@.
data MPSImageLaplacianPyramid

instance IsObjCObject (Id MPSImageLaplacianPyramid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageLaplacianPyramid"

class IsMPSImagePyramid a => IsMPSImageLaplacianPyramid a where
  toMPSImageLaplacianPyramid :: a -> Id MPSImageLaplacianPyramid

instance IsMPSImageLaplacianPyramid (Id MPSImageLaplacianPyramid) where
  toMPSImageLaplacianPyramid = unsafeCastId

instance IsMPSImagePyramid (Id MPSImageLaplacianPyramid) where
  toMPSImagePyramid = unsafeCastId

instance IsMPSKernel (Id MPSImageLaplacianPyramid) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageLaplacianPyramid) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageLaplacianPyramid) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceColumnMax ----------

-- | MPSImageReduceColumnMax
--
-- The MPSImageReduceColumnMax performs a reduction operation returning the maximum value for each column of an image
-- 
-- Phantom type for @MPSImageReduceColumnMax@.
data MPSImageReduceColumnMax

instance IsObjCObject (Id MPSImageReduceColumnMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceColumnMax"

class IsMPSImageReduceUnary a => IsMPSImageReduceColumnMax a where
  toMPSImageReduceColumnMax :: a -> Id MPSImageReduceColumnMax

instance IsMPSImageReduceColumnMax (Id MPSImageReduceColumnMax) where
  toMPSImageReduceColumnMax = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceColumnMax) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceColumnMax) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceColumnMax) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceColumnMax) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceColumnMean ----------

-- | MPSImageReduceColumnMean
--
-- The MPSImageReduceColumnMean performs a reduction operation returning the mean value for each column of an image
-- 
-- Phantom type for @MPSImageReduceColumnMean@.
data MPSImageReduceColumnMean

instance IsObjCObject (Id MPSImageReduceColumnMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceColumnMean"

class IsMPSImageReduceUnary a => IsMPSImageReduceColumnMean a where
  toMPSImageReduceColumnMean :: a -> Id MPSImageReduceColumnMean

instance IsMPSImageReduceColumnMean (Id MPSImageReduceColumnMean) where
  toMPSImageReduceColumnMean = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceColumnMean) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceColumnMean) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceColumnMean) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceColumnMean) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceColumnMin ----------

-- | MPSImageReduceColumnMin
--
-- The MPSImageReduceColumnMin performs a reduction operation returning the mininmum value for each column of an image
-- 
-- Phantom type for @MPSImageReduceColumnMin@.
data MPSImageReduceColumnMin

instance IsObjCObject (Id MPSImageReduceColumnMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceColumnMin"

class IsMPSImageReduceUnary a => IsMPSImageReduceColumnMin a where
  toMPSImageReduceColumnMin :: a -> Id MPSImageReduceColumnMin

instance IsMPSImageReduceColumnMin (Id MPSImageReduceColumnMin) where
  toMPSImageReduceColumnMin = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceColumnMin) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceColumnMin) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceColumnMin) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceColumnMin) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceColumnSum ----------

-- | MPSImageReduceColumnSum
--
-- The MPSImageReduceColumnSum performs a reduction operation returning the sum for each column of an image
-- 
-- Phantom type for @MPSImageReduceColumnSum@.
data MPSImageReduceColumnSum

instance IsObjCObject (Id MPSImageReduceColumnSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceColumnSum"

class IsMPSImageReduceUnary a => IsMPSImageReduceColumnSum a where
  toMPSImageReduceColumnSum :: a -> Id MPSImageReduceColumnSum

instance IsMPSImageReduceColumnSum (Id MPSImageReduceColumnSum) where
  toMPSImageReduceColumnSum = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceColumnSum) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceColumnSum) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceColumnSum) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceColumnSum) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceRowMax ----------

-- | MPSImageReduceRowMax
--
-- The MPSImageReduceRowMax performs a reduction operation returning the maximum value for each row of an image
-- 
-- Phantom type for @MPSImageReduceRowMax@.
data MPSImageReduceRowMax

instance IsObjCObject (Id MPSImageReduceRowMax) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceRowMax"

class IsMPSImageReduceUnary a => IsMPSImageReduceRowMax a where
  toMPSImageReduceRowMax :: a -> Id MPSImageReduceRowMax

instance IsMPSImageReduceRowMax (Id MPSImageReduceRowMax) where
  toMPSImageReduceRowMax = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceRowMax) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceRowMax) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceRowMax) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceRowMax) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceRowMean ----------

-- | MPSImageReduceRowMean
--
-- The MPSImageReduceRowMean performs a reduction operation returning the mean value for each row of an image
-- 
-- Phantom type for @MPSImageReduceRowMean@.
data MPSImageReduceRowMean

instance IsObjCObject (Id MPSImageReduceRowMean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceRowMean"

class IsMPSImageReduceUnary a => IsMPSImageReduceRowMean a where
  toMPSImageReduceRowMean :: a -> Id MPSImageReduceRowMean

instance IsMPSImageReduceRowMean (Id MPSImageReduceRowMean) where
  toMPSImageReduceRowMean = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceRowMean) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceRowMean) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceRowMean) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceRowMean) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceRowMin ----------

-- | MPSImageReduceRowMin
--
-- The MPSImageReduceRowMin performs a reduction operation returning the mininmum value for each row of an image
-- 
-- Phantom type for @MPSImageReduceRowMin@.
data MPSImageReduceRowMin

instance IsObjCObject (Id MPSImageReduceRowMin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceRowMin"

class IsMPSImageReduceUnary a => IsMPSImageReduceRowMin a where
  toMPSImageReduceRowMin :: a -> Id MPSImageReduceRowMin

instance IsMPSImageReduceRowMin (Id MPSImageReduceRowMin) where
  toMPSImageReduceRowMin = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceRowMin) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceRowMin) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceRowMin) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceRowMin) where
  toNSObject = unsafeCastId

-- ---------- MPSImageReduceRowSum ----------

-- | MPSImageReduceRowSum
--
-- The MPSImageReduceRowSum performs a reduction operation returning the sum for each row of an image
-- 
-- Phantom type for @MPSImageReduceRowSum@.
data MPSImageReduceRowSum

instance IsObjCObject (Id MPSImageReduceRowSum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageReduceRowSum"

class IsMPSImageReduceUnary a => IsMPSImageReduceRowSum a where
  toMPSImageReduceRowSum :: a -> Id MPSImageReduceRowSum

instance IsMPSImageReduceRowSum (Id MPSImageReduceRowSum) where
  toMPSImageReduceRowSum = unsafeCastId

instance IsMPSImageReduceUnary (Id MPSImageReduceRowSum) where
  toMPSImageReduceUnary = unsafeCastId

instance IsMPSKernel (Id MPSImageReduceRowSum) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageReduceRowSum) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageReduceRowSum) where
  toNSObject = unsafeCastId

-- ---------- MPSImageBilinearScale ----------

-- | MPSImageBilinearScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageBilinearScale filter can be used to resample an existing image              using a bilinear filter. This is typically used to reduce the size of an image.
-- 
-- Phantom type for @MPSImageBilinearScale@.
data MPSImageBilinearScale

instance IsObjCObject (Id MPSImageBilinearScale) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageBilinearScale"

class IsMPSImageScale a => IsMPSImageBilinearScale a where
  toMPSImageBilinearScale :: a -> Id MPSImageBilinearScale

instance IsMPSImageBilinearScale (Id MPSImageBilinearScale) where
  toMPSImageBilinearScale = unsafeCastId

instance IsMPSImageScale (Id MPSImageBilinearScale) where
  toMPSImageScale = unsafeCastId

instance IsMPSKernel (Id MPSImageBilinearScale) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageBilinearScale) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageBilinearScale) where
  toNSObject = unsafeCastId

-- ---------- MPSImageLanczosScale ----------

-- | MPSImageLanczosScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageLanczosScale filter can be used to resample an existing image              using a different sampling frequency in each dimension. This can be              used to enlarge or reduce the size of an image, or change the aspect              ratio of an image.  The filter uses a Lanczos resampling algorithm              which typically produces better quality for photographs, but is slower              than linear sampling using the GPU texture units. Lanczos downsampling               does not require a low pass filter to be applied before it is used.               Because the resampling function has negative lobes, Lanczos can result               in ringing near sharp edges, making it less suitable for vector art.
-- 
-- Phantom type for @MPSImageLanczosScale@.
data MPSImageLanczosScale

instance IsObjCObject (Id MPSImageLanczosScale) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageLanczosScale"

class IsMPSImageScale a => IsMPSImageLanczosScale a where
  toMPSImageLanczosScale :: a -> Id MPSImageLanczosScale

instance IsMPSImageLanczosScale (Id MPSImageLanczosScale) where
  toMPSImageLanczosScale = unsafeCastId

instance IsMPSImageScale (Id MPSImageLanczosScale) where
  toMPSImageScale = unsafeCastId

instance IsMPSKernel (Id MPSImageLanczosScale) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageLanczosScale) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageLanczosScale) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNBinaryFullyConnectedNode ----------

-- | A MPSNNFilterNode representing a MPSCNNBinaryFullyConnected kernel
-- 
-- Phantom type for @MPSCNNBinaryFullyConnectedNode@.
data MPSCNNBinaryFullyConnectedNode

instance IsObjCObject (Id MPSCNNBinaryFullyConnectedNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNBinaryFullyConnectedNode"

class IsMPSCNNBinaryConvolutionNode a => IsMPSCNNBinaryFullyConnectedNode a where
  toMPSCNNBinaryFullyConnectedNode :: a -> Id MPSCNNBinaryFullyConnectedNode

instance IsMPSCNNBinaryFullyConnectedNode (Id MPSCNNBinaryFullyConnectedNode) where
  toMPSCNNBinaryFullyConnectedNode = unsafeCastId

instance IsMPSCNNBinaryConvolutionNode (Id MPSCNNBinaryFullyConnectedNode) where
  toMPSCNNBinaryConvolutionNode = unsafeCastId

instance IsMPSCNNConvolutionNode (Id MPSCNNBinaryFullyConnectedNode) where
  toMPSCNNConvolutionNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNBinaryFullyConnectedNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNBinaryFullyConnectedNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTransposeGradientNode ----------

-- | Phantom type for @MPSCNNConvolutionTransposeGradientNode@.
data MPSCNNConvolutionTransposeGradientNode

instance IsObjCObject (Id MPSCNNConvolutionTransposeGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTransposeGradientNode"

class IsMPSCNNConvolutionGradientNode a => IsMPSCNNConvolutionTransposeGradientNode a where
  toMPSCNNConvolutionTransposeGradientNode :: a -> Id MPSCNNConvolutionTransposeGradientNode

instance IsMPSCNNConvolutionTransposeGradientNode (Id MPSCNNConvolutionTransposeGradientNode) where
  toMPSCNNConvolutionTransposeGradientNode = unsafeCastId

instance IsMPSCNNConvolutionGradientNode (Id MPSCNNConvolutionTransposeGradientNode) where
  toMPSCNNConvolutionGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNConvolutionTransposeGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNConvolutionTransposeGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTransposeGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNFullyConnectedGradientNode ----------

-- | Phantom type for @MPSCNNFullyConnectedGradientNode@.
data MPSCNNFullyConnectedGradientNode

instance IsObjCObject (Id MPSCNNFullyConnectedGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNFullyConnectedGradientNode"

class IsMPSCNNConvolutionGradientNode a => IsMPSCNNFullyConnectedGradientNode a where
  toMPSCNNFullyConnectedGradientNode :: a -> Id MPSCNNFullyConnectedGradientNode

instance IsMPSCNNFullyConnectedGradientNode (Id MPSCNNFullyConnectedGradientNode) where
  toMPSCNNFullyConnectedGradientNode = unsafeCastId

instance IsMPSCNNConvolutionGradientNode (Id MPSCNNFullyConnectedGradientNode) where
  toMPSCNNConvolutionGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNFullyConnectedGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNFullyConnectedGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNFullyConnectedGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDilatedPoolingMaxGradientNode ----------

-- | Phantom type for @MPSCNNDilatedPoolingMaxGradientNode@.
data MPSCNNDilatedPoolingMaxGradientNode

instance IsObjCObject (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDilatedPoolingMaxGradientNode"

class IsMPSCNNPoolingGradientNode a => IsMPSCNNDilatedPoolingMaxGradientNode a where
  toMPSCNNDilatedPoolingMaxGradientNode :: a -> Id MPSCNNDilatedPoolingMaxGradientNode

instance IsMPSCNNDilatedPoolingMaxGradientNode (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toMPSCNNDilatedPoolingMaxGradientNode = unsafeCastId

instance IsMPSCNNPoolingGradientNode (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toMPSCNNPoolingGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNDilatedPoolingMaxGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingAverageGradientNode ----------

-- | Phantom type for @MPSCNNPoolingAverageGradientNode@.
data MPSCNNPoolingAverageGradientNode

instance IsObjCObject (Id MPSCNNPoolingAverageGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingAverageGradientNode"

class IsMPSCNNPoolingGradientNode a => IsMPSCNNPoolingAverageGradientNode a where
  toMPSCNNPoolingAverageGradientNode :: a -> Id MPSCNNPoolingAverageGradientNode

instance IsMPSCNNPoolingAverageGradientNode (Id MPSCNNPoolingAverageGradientNode) where
  toMPSCNNPoolingAverageGradientNode = unsafeCastId

instance IsMPSCNNPoolingGradientNode (Id MPSCNNPoolingAverageGradientNode) where
  toMPSCNNPoolingGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingAverageGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNPoolingAverageGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingAverageGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingL2NormGradientNode ----------

-- | Phantom type for @MPSCNNPoolingL2NormGradientNode@.
data MPSCNNPoolingL2NormGradientNode

instance IsObjCObject (Id MPSCNNPoolingL2NormGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingL2NormGradientNode"

class IsMPSCNNPoolingGradientNode a => IsMPSCNNPoolingL2NormGradientNode a where
  toMPSCNNPoolingL2NormGradientNode :: a -> Id MPSCNNPoolingL2NormGradientNode

instance IsMPSCNNPoolingL2NormGradientNode (Id MPSCNNPoolingL2NormGradientNode) where
  toMPSCNNPoolingL2NormGradientNode = unsafeCastId

instance IsMPSCNNPoolingGradientNode (Id MPSCNNPoolingL2NormGradientNode) where
  toMPSCNNPoolingGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingL2NormGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNPoolingL2NormGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingL2NormGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingMaxGradientNode ----------

-- | Phantom type for @MPSCNNPoolingMaxGradientNode@.
data MPSCNNPoolingMaxGradientNode

instance IsObjCObject (Id MPSCNNPoolingMaxGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingMaxGradientNode"

class IsMPSCNNPoolingGradientNode a => IsMPSCNNPoolingMaxGradientNode a where
  toMPSCNNPoolingMaxGradientNode :: a -> Id MPSCNNPoolingMaxGradientNode

instance IsMPSCNNPoolingMaxGradientNode (Id MPSCNNPoolingMaxGradientNode) where
  toMPSCNNPoolingMaxGradientNode = unsafeCastId

instance IsMPSCNNPoolingGradientNode (Id MPSCNNPoolingMaxGradientNode) where
  toMPSCNNPoolingGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSCNNPoolingMaxGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSCNNPoolingMaxGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingMaxGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNAdditionGradientNode ----------

-- | returns gradient for either primary or secondary source image from the inference pass.  Use the isSecondarySourceFilter property to indicate whether this filter is computing the gradient  for the primary or secondary source image from the inference pass.
-- 
-- Phantom type for @MPSNNAdditionGradientNode@.
data MPSNNAdditionGradientNode

instance IsObjCObject (Id MPSNNAdditionGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNAdditionGradientNode"

class IsMPSNNArithmeticGradientNode a => IsMPSNNAdditionGradientNode a where
  toMPSNNAdditionGradientNode :: a -> Id MPSNNAdditionGradientNode

instance IsMPSNNAdditionGradientNode (Id MPSNNAdditionGradientNode) where
  toMPSNNAdditionGradientNode = unsafeCastId

instance IsMPSNNArithmeticGradientNode (Id MPSNNAdditionGradientNode) where
  toMPSNNArithmeticGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNAdditionGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNAdditionGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNAdditionGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNMultiplicationGradientNode ----------

-- | returns gradient for either primary or secondary source image from the inference pass.  Use the isSecondarySourceFilter property to indicate whether this filter is computing the gradient  for the primary or secondary source image from the inference pass.
-- 
-- Phantom type for @MPSNNMultiplicationGradientNode@.
data MPSNNMultiplicationGradientNode

instance IsObjCObject (Id MPSNNMultiplicationGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNMultiplicationGradientNode"

class IsMPSNNArithmeticGradientNode a => IsMPSNNMultiplicationGradientNode a where
  toMPSNNMultiplicationGradientNode :: a -> Id MPSNNMultiplicationGradientNode

instance IsMPSNNMultiplicationGradientNode (Id MPSNNMultiplicationGradientNode) where
  toMPSNNMultiplicationGradientNode = unsafeCastId

instance IsMPSNNArithmeticGradientNode (Id MPSNNMultiplicationGradientNode) where
  toMPSNNArithmeticGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNMultiplicationGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNMultiplicationGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNMultiplicationGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSNNSubtractionGradientNode ----------

-- | returns gradient for either primary or secondary source image from the inference pass.  Use the isSecondarySourceFilter property to indicate whether this filter is computing the gradient  for the primary or secondary source image from the inference pass.
-- 
-- Phantom type for @MPSNNSubtractionGradientNode@.
data MPSNNSubtractionGradientNode

instance IsObjCObject (Id MPSNNSubtractionGradientNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNNSubtractionGradientNode"

class IsMPSNNArithmeticGradientNode a => IsMPSNNSubtractionGradientNode a where
  toMPSNNSubtractionGradientNode :: a -> Id MPSNNSubtractionGradientNode

instance IsMPSNNSubtractionGradientNode (Id MPSNNSubtractionGradientNode) where
  toMPSNNSubtractionGradientNode = unsafeCastId

instance IsMPSNNArithmeticGradientNode (Id MPSNNSubtractionGradientNode) where
  toMPSNNArithmeticGradientNode = unsafeCastId

instance IsMPSNNFilterNode (Id MPSNNSubtractionGradientNode) where
  toMPSNNFilterNode = unsafeCastId

instance IsMPSNNGradientFilterNode (Id MPSNNSubtractionGradientNode) where
  toMPSNNGradientFilterNode = unsafeCastId

instance IsNSObject (Id MPSNNSubtractionGradientNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTransposeGradientStateNode ----------

-- | Phantom type for @MPSCNNConvolutionTransposeGradientStateNode@.
data MPSCNNConvolutionTransposeGradientStateNode

instance IsObjCObject (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTransposeGradientStateNode"

class IsMPSCNNConvolutionGradientStateNode a => IsMPSCNNConvolutionTransposeGradientStateNode a where
  toMPSCNNConvolutionTransposeGradientStateNode :: a -> Id MPSCNNConvolutionTransposeGradientStateNode

instance IsMPSCNNConvolutionTransposeGradientStateNode (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toMPSCNNConvolutionTransposeGradientStateNode = unsafeCastId

instance IsMPSCNNConvolutionGradientStateNode (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toMPSCNNConvolutionGradientStateNode = unsafeCastId

instance IsMPSNNGradientStateNode (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toMPSNNGradientStateNode = unsafeCastId

instance IsMPSNNStateNode (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toMPSNNStateNode = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTransposeGradientStateNode) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNConvolutionTransposeGradientState ----------

-- | MPSCNNConvolutionTransposeGradientState
--
-- The MPSCNNConvolutionTransposeGradientState is returned by resultStateForSourceImage:sourceStates method on MPSCNNConvolutionTranspose object.              Note that resultStateForSourceImage:sourceStates:destinationImage creates the object on autoreleasepool.              It will be consumed by MPSCNNConvolutionTransposeGradient. It contains reference to MPSCNNConvolutionGradientState object that connects              MPSCNNConvolution and its corresponding MPSCNNConvolutionTranspose in forward pass of autoencoder. In an autoencoder forward pass, MPSCNNConvolutionGradientState is produced              by MPSCNNConvolution object and is used by corresponding MPSCNNConvolutionTraspose of forward pass that "undo" the corresponding MPSCNNConvolution. It is used to correctly size              destination image that is returned on left hand side by encode call MPSCNNConvolutionTranspose as well as automatically set kernelOffsetX/Y on MPSCNNConvolutionTranspose using              the offset and other properties of corresponding MPSCNNConvolution object. During training, same MPSCNNConvolutionGradientState object will be consumed by MPSCNNConvolutionGradient              object and the MPSCNNConvolutionTransposeGradientState produced by MPSCNNConvolutionTranspose's resultStateForSourceImage:sourceStates:destinationImage will be consumed by              MPSCNNConvolutionTransposeGradient object
--
-- Note that state objects are not usable across batches i.e. when batch is done you should nuke the state object and create              new one for next batch.              Weights update process for MPSCNNConvolutionTranspose is same as explained above for MPSCNNConvolution. See comments for MPSCNNConvolutionGradientState.
-- 
-- Phantom type for @MPSCNNConvolutionTransposeGradientState@.
data MPSCNNConvolutionTransposeGradientState

instance IsObjCObject (Id MPSCNNConvolutionTransposeGradientState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNConvolutionTransposeGradientState"

class IsMPSCNNConvolutionGradientState a => IsMPSCNNConvolutionTransposeGradientState a where
  toMPSCNNConvolutionTransposeGradientState :: a -> Id MPSCNNConvolutionTransposeGradientState

instance IsMPSCNNConvolutionTransposeGradientState (Id MPSCNNConvolutionTransposeGradientState) where
  toMPSCNNConvolutionTransposeGradientState = unsafeCastId

instance IsMPSCNNConvolutionGradientState (Id MPSCNNConvolutionTransposeGradientState) where
  toMPSCNNConvolutionGradientState = unsafeCastId

instance IsMPSNNGradientState (Id MPSCNNConvolutionTransposeGradientState) where
  toMPSNNGradientState = unsafeCastId

instance IsMPSState (Id MPSCNNConvolutionTransposeGradientState) where
  toMPSState = unsafeCastId

instance IsNSObject (Id MPSCNNConvolutionTransposeGradientState) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNAddGradient ----------

-- | MPSCNNAddGradient
--
-- This depends on Metal.framework.
--
-- Specifies the addition gradient operator.              This arithmetic gradient filter requires the following inputs: gradient image from              the previous layer (going backwards) and either the primary or the secondary source              image from the forward pass. You will need a separate filter for the primary and              secondary source images.
--
-- Without broadcasting, the arithmetic add gradient operation is a copy operation on              the input gradient image. It is the same operation for both the primary and secondary              source images (for x + y, d/dx(x + y) = 1, d/dy(x + y) = 1). This copy operation can              be optimized away by the graph interface.
--
-- Setting the broadcasting parameters results in a reduction operation (sum) across all              of the applicable broadcasting dimensions (rows, columns, feature channels, or any              combination thereof) to produce the destination image of the size that matches the              primary/secondary input images used in the forward pass.
-- 
-- Phantom type for @MPSCNNAddGradient@.
data MPSCNNAddGradient

instance IsObjCObject (Id MPSCNNAddGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNAddGradient"

class IsMPSCNNArithmeticGradient a => IsMPSCNNAddGradient a where
  toMPSCNNAddGradient :: a -> Id MPSCNNAddGradient

instance IsMPSCNNAddGradient (Id MPSCNNAddGradient) where
  toMPSCNNAddGradient = unsafeCastId

instance IsMPSCNNArithmeticGradient (Id MPSCNNAddGradient) where
  toMPSCNNArithmeticGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNAddGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNAddGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNAddGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNAddGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNMultiplyGradient ----------

-- | MPSCNNMultiplyGradient
--
-- This depends on Metal.framework.
--
-- Specifies the multiplication gradient operator.              This arithmetic gradient filter requires the following inputs: gradient image from              the previous layer (going backwards) and either the primary or the secondary source              image from the forward pass. You will need a separate filter for the primary and              secondary source images.
--
-- Without broadcasting, the arithmetic multiply gradient operation is an element-wise              multiplication operation between the gradient image from the previous layer (going              backwards) and:              - The secondary source image from the forward pass for the primary source filter                (for x * y, d/dx(x * y) = y).              - The primary source image from the forward pass for the secondary source filter                (for x * y, d/dy(x * y) = x).
--
-- Setting the broadcasting parameters results in a reduction operation (sum) across all              of the applicable broadcasting dimensions (rows, columns, feature channels, or any              combination thereof) to produce the destination image of the size that matches the              primary/secondary input images used in the forward pass.
-- 
-- Phantom type for @MPSCNNMultiplyGradient@.
data MPSCNNMultiplyGradient

instance IsObjCObject (Id MPSCNNMultiplyGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNMultiplyGradient"

class IsMPSCNNArithmeticGradient a => IsMPSCNNMultiplyGradient a where
  toMPSCNNMultiplyGradient :: a -> Id MPSCNNMultiplyGradient

instance IsMPSCNNMultiplyGradient (Id MPSCNNMultiplyGradient) where
  toMPSCNNMultiplyGradient = unsafeCastId

instance IsMPSCNNArithmeticGradient (Id MPSCNNMultiplyGradient) where
  toMPSCNNArithmeticGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNMultiplyGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNMultiplyGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNMultiplyGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNMultiplyGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNSubtractGradient ----------

-- | MPSCNNSubtractGradient
--
-- This depends on Metal.framework.
--
-- Specifies the subtraction gradient operator.              This arithmetic gradient filter requires the following inputs: gradient image from              the previous layer (going backwards) and either the primary or the secondary source              image from the forward pass. You will need a separate filter for the primary and              secondary source images.
--
-- Without broadcasting, the arithmetic subtract gradient operation for the primary              source image is a copy operation on the input gradient image (for x - y, d/dx(x - y) = 1).              This copy operation can be optimized away by the graph interface.
--
-- For the secondary source image, the result is a negation of the gradient image from              the previous layer (for x - y, d/dy(x - y) = -1).
--
-- Setting the broadcasting parameters results in a reduction operation (sum) across all              of the applicable broadcasting dimensions (rows, columns, feature channels, or any              combination thereof) to produce the destination image of the size that matches the              primary/secondary input images used in the forward pass.
-- 
-- Phantom type for @MPSCNNSubtractGradient@.
data MPSCNNSubtractGradient

instance IsObjCObject (Id MPSCNNSubtractGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNSubtractGradient"

class IsMPSCNNArithmeticGradient a => IsMPSCNNSubtractGradient a where
  toMPSCNNSubtractGradient :: a -> Id MPSCNNSubtractGradient

instance IsMPSCNNSubtractGradient (Id MPSCNNSubtractGradient) where
  toMPSCNNSubtractGradient = unsafeCastId

instance IsMPSCNNArithmeticGradient (Id MPSCNNSubtractGradient) where
  toMPSCNNArithmeticGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNSubtractGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNSubtractGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNSubtractGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNSubtractGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNFullyConnectedGradient ----------

-- | MPSCNNFullyConnectedGradient
--
-- This depends on Metal.framework
--
-- Compute the gradient for fully connected layer.
-- 
-- Phantom type for @MPSCNNFullyConnectedGradient@.
data MPSCNNFullyConnectedGradient

instance IsObjCObject (Id MPSCNNFullyConnectedGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNFullyConnectedGradient"

class IsMPSCNNConvolutionGradient a => IsMPSCNNFullyConnectedGradient a where
  toMPSCNNFullyConnectedGradient :: a -> Id MPSCNNFullyConnectedGradient

instance IsMPSCNNFullyConnectedGradient (Id MPSCNNFullyConnectedGradient) where
  toMPSCNNFullyConnectedGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNFullyConnectedGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNConvolutionGradient (Id MPSCNNFullyConnectedGradient) where
  toMPSCNNConvolutionGradient = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNFullyConnectedGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSKernel (Id MPSCNNFullyConnectedGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNFullyConnectedGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNDilatedPoolingMaxGradient ----------

-- | MPSCNNDilatedPoolingMaxGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the dilated max pooling filter.              For details see comments on MPSCNNPoolingMaxGradient.
-- 
-- Phantom type for @MPSCNNDilatedPoolingMaxGradient@.
data MPSCNNDilatedPoolingMaxGradient

instance IsObjCObject (Id MPSCNNDilatedPoolingMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNDilatedPoolingMaxGradient"

class IsMPSCNNPoolingGradient a => IsMPSCNNDilatedPoolingMaxGradient a where
  toMPSCNNDilatedPoolingMaxGradient :: a -> Id MPSCNNDilatedPoolingMaxGradient

instance IsMPSCNNDilatedPoolingMaxGradient (Id MPSCNNDilatedPoolingMaxGradient) where
  toMPSCNNDilatedPoolingMaxGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNDilatedPoolingMaxGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNDilatedPoolingMaxGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNPoolingGradient (Id MPSCNNDilatedPoolingMaxGradient) where
  toMPSCNNPoolingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNDilatedPoolingMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNDilatedPoolingMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingAverageGradient ----------

-- | MPSCNNPoolingAverageGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the average pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- Average pooling forward pass is defined as:
--
-- out(x) = sum_{dx  Window(x)} in(s*x+dx) / N(x), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification,              'N(x)' is effective pooling window size in pixels as specified in MPSCNNPoolingAverage,              's' is the pixel stride and in() is the source input image.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = sum_{dx  Window(x)} delta_{s*x+dx, y} / N(x), where
--
-- delta_{x,y} is the Kronecker delta symbol for which
--
-- delta_{x,y} =  {  1, when x == y                                 {  0, otherwise.
--
-- In practice this means that the gradient value for the destination image at pixel 'x' is              the sum over these contributions coming from all pooling windows that contribute              to the average pooling computation in the forward pass, multiplied by the input              gradient value in the source area of the corresponding pooling window.
--
-- Note: As average pooling is a linear operation of its inputs, the gradient does not              depend at all on the original input values, but the original input image size is needed              so that we know the limits where the input values seize to exist to inhibit accumulation              of gradient values for those pixels. Therefore, as secondary input, any correctly sized              image will produce correct results for the gradient backpropagation and hence it is              recommended to use a temporary image of correct size (see MPSTemporaryImage) for the              secondary source image parameter.
-- 
-- Phantom type for @MPSCNNPoolingAverageGradient@.
data MPSCNNPoolingAverageGradient

instance IsObjCObject (Id MPSCNNPoolingAverageGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingAverageGradient"

class IsMPSCNNPoolingGradient a => IsMPSCNNPoolingAverageGradient a where
  toMPSCNNPoolingAverageGradient :: a -> Id MPSCNNPoolingAverageGradient

instance IsMPSCNNPoolingAverageGradient (Id MPSCNNPoolingAverageGradient) where
  toMPSCNNPoolingAverageGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNPoolingAverageGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNPoolingAverageGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNPoolingGradient (Id MPSCNNPoolingAverageGradient) where
  toMPSCNNPoolingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingAverageGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingAverageGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingL2NormGradient ----------

-- | MPSCNNPoolingL2NormGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the L2-Norm pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- L2-Norm pooling forward pass is defined as:
--
-- out(x) = sqrt( sum_{dx  Window(x)} in(s*x+dx) * in(s*x+dx) ), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification and              's' is the pixel stride and in() is the source input image.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = sum_{dx  Window(x)} delta_{s*x+dx, y} in(s*x+dx) / out(x), where
--
-- delta_{x,y} is the Kronecker delta symbol for which
--
-- delta_{x,y} =  {  1, when x == y                                 {  0, otherwise,              and out(x) is the L2-norm pooling value at point 'x' defined above.
-- 
-- Phantom type for @MPSCNNPoolingL2NormGradient@.
data MPSCNNPoolingL2NormGradient

instance IsObjCObject (Id MPSCNNPoolingL2NormGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingL2NormGradient"

class IsMPSCNNPoolingGradient a => IsMPSCNNPoolingL2NormGradient a where
  toMPSCNNPoolingL2NormGradient :: a -> Id MPSCNNPoolingL2NormGradient

instance IsMPSCNNPoolingL2NormGradient (Id MPSCNNPoolingL2NormGradient) where
  toMPSCNNPoolingL2NormGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNPoolingL2NormGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNPoolingL2NormGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNPoolingGradient (Id MPSCNNPoolingL2NormGradient) where
  toMPSCNNPoolingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingL2NormGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingL2NormGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNPoolingMaxGradient ----------

-- | MPSCNNPoolingMaxGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the max pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- Dilated Max pooling forward pass is defined as:
--
-- out(x) = max_{dx  Window(x)} in(s*x+D*dx), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification,              's' is the pixel stride and in() is the source input image and D is the dilation factor.              For MPSCNNPoolingMaxGradient the dilationRate 'D' is one. NOTE: For even-sized pooling              windows with dilation rate greater than one the effective pooling window is centered              around s*x with non-even windows leaning towards top-left corner. For example if              kernel width = 2, dilation rate = 3, then the pooling considers positions '-2' and '+1'              relative to the pooling window center 's*x'.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = delta_{x_m, y}, where
--
-- delta_{x,y} is the Kronecker delta symbol (see MPSCNNPoolingAverageGradient) and x_m              is the index of the maximum value in the corresponding pooling window.
--
-- In practice this means that the gradient value for the destination image at pixel 'x' is              the sum over these contributions coming from all pooling windows that contribute              to the max pooling computation in the forward pass, multiplied by the input              gradient value in the source area of the corresponding pooling window. If there are              multiple maximal values within a single pooling window one of them is picked for the              gradient and this decision is implementation specific, which means that it can vary              between different architectures and even between different filter parameters.
--
-- Note: The gradient max pooling needs the secondary input image in order to compute              the indices of maximal values for each pooling window, but this means redundant computations.              Later we may add encode calls to MPSCNNPoolingMax that produce a state that contains the              coordinates of the maximal values to be consumed by the gradient filters.
-- 
-- Phantom type for @MPSCNNPoolingMaxGradient@.
data MPSCNNPoolingMaxGradient

instance IsObjCObject (Id MPSCNNPoolingMaxGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNPoolingMaxGradient"

class IsMPSCNNPoolingGradient a => IsMPSCNNPoolingMaxGradient a where
  toMPSCNNPoolingMaxGradient :: a -> Id MPSCNNPoolingMaxGradient

instance IsMPSCNNPoolingMaxGradient (Id MPSCNNPoolingMaxGradient) where
  toMPSCNNPoolingMaxGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNPoolingMaxGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNPoolingMaxGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNPoolingGradient (Id MPSCNNPoolingMaxGradient) where
  toMPSCNNPoolingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNPoolingMaxGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNPoolingMaxGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingBilinearGradient ----------

-- | MPSCNNUpsamplingBilinearGradient
--
-- This depends on Metal.framework.
--
-- Specifies the bilinear spatial downsampling filter.
-- 
-- Phantom type for @MPSCNNUpsamplingBilinearGradient@.
data MPSCNNUpsamplingBilinearGradient

instance IsObjCObject (Id MPSCNNUpsamplingBilinearGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingBilinearGradient"

class IsMPSCNNUpsamplingGradient a => IsMPSCNNUpsamplingBilinearGradient a where
  toMPSCNNUpsamplingBilinearGradient :: a -> Id MPSCNNUpsamplingBilinearGradient

instance IsMPSCNNUpsamplingBilinearGradient (Id MPSCNNUpsamplingBilinearGradient) where
  toMPSCNNUpsamplingBilinearGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNUpsamplingBilinearGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNUpsamplingBilinearGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNUpsamplingGradient (Id MPSCNNUpsamplingBilinearGradient) where
  toMPSCNNUpsamplingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsamplingBilinearGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingBilinearGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSCNNUpsamplingNearestGradient ----------

-- | MPSCNNUpsamplingNearestGradient
--
-- This depends on Metal.framework.
--
-- Specifies the nearest spatial downsampling filter.
-- 
-- Phantom type for @MPSCNNUpsamplingNearestGradient@.
data MPSCNNUpsamplingNearestGradient

instance IsObjCObject (Id MPSCNNUpsamplingNearestGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSCNNUpsamplingNearestGradient"

class IsMPSCNNUpsamplingGradient a => IsMPSCNNUpsamplingNearestGradient a where
  toMPSCNNUpsamplingNearestGradient :: a -> Id MPSCNNUpsamplingNearestGradient

instance IsMPSCNNUpsamplingNearestGradient (Id MPSCNNUpsamplingNearestGradient) where
  toMPSCNNUpsamplingNearestGradient = unsafeCastId

instance IsMPSCNNBinaryKernel (Id MPSCNNUpsamplingNearestGradient) where
  toMPSCNNBinaryKernel = unsafeCastId

instance IsMPSCNNGradientKernel (Id MPSCNNUpsamplingNearestGradient) where
  toMPSCNNGradientKernel = unsafeCastId

instance IsMPSCNNUpsamplingGradient (Id MPSCNNUpsamplingNearestGradient) where
  toMPSCNNUpsamplingGradient = unsafeCastId

instance IsMPSKernel (Id MPSCNNUpsamplingNearestGradient) where
  toMPSKernel = unsafeCastId

instance IsNSObject (Id MPSCNNUpsamplingNearestGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayGatherGradient ----------

-- | MPSNDArrayGatherGradient
--
-- This depends on Metal.framework.
--
-- Applies the gradient operation corresponding to a forward gather operation.
-- 
-- Phantom type for @MPSNDArrayGatherGradient@.
data MPSNDArrayGatherGradient

instance IsObjCObject (Id MPSNDArrayGatherGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayGatherGradient"

class IsMPSNDArrayBinaryPrimaryGradientKernel a => IsMPSNDArrayGatherGradient a where
  toMPSNDArrayGatherGradient :: a -> Id MPSNDArrayGatherGradient

instance IsMPSNDArrayGatherGradient (Id MPSNDArrayGatherGradient) where
  toMPSNDArrayGatherGradient = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayGatherGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayBinaryPrimaryGradientKernel (Id MPSNDArrayGatherGradient) where
  toMPSNDArrayBinaryPrimaryGradientKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayGatherGradient) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayGatherGradient) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayGatherGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayStridedSliceGradient ----------

-- | MPSNDStridedSliceGradient
--
-- This depends on Metal.framework.
--
-- Perform the gradient operation corresponding to a strided slice.
-- 
-- Phantom type for @MPSNDArrayStridedSliceGradient@.
data MPSNDArrayStridedSliceGradient

instance IsObjCObject (Id MPSNDArrayStridedSliceGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayStridedSliceGradient"

class IsMPSNDArrayUnaryGradientKernel a => IsMPSNDArrayStridedSliceGradient a where
  toMPSNDArrayStridedSliceGradient :: a -> Id MPSNDArrayStridedSliceGradient

instance IsMPSNDArrayStridedSliceGradient (Id MPSNDArrayStridedSliceGradient) where
  toMPSNDArrayStridedSliceGradient = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayStridedSliceGradient) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayStridedSliceGradient) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryGradientKernel (Id MPSNDArrayStridedSliceGradient) where
  toMPSNDArrayMultiaryGradientKernel = unsafeCastId

instance IsMPSNDArrayUnaryGradientKernel (Id MPSNDArrayStridedSliceGradient) where
  toMPSNDArrayUnaryGradientKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayStridedSliceGradient) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayGather ----------

-- | MPSNDArrayGather
--
-- This depends on Metal.framework.
--
-- Applies a gather operation along a given axis.  The encoded primary source array              contains the data and the secondary array is a 1-D MPSNDArray containing the              indices.
--
-- For each dimension other than axis                      result[i] = source[i]; 0 <= i < array slice length along dimension                  Along the specified axis                      result[i] = source[indices[i]]; 0 <= i < number of indices
-- 
-- Phantom type for @MPSNDArrayGather@.
data MPSNDArrayGather

instance IsObjCObject (Id MPSNDArrayGather) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayGather"

class IsMPSNDArrayBinaryKernel a => IsMPSNDArrayGather a where
  toMPSNDArrayGather :: a -> Id MPSNDArrayGather

instance IsMPSNDArrayGather (Id MPSNDArrayGather) where
  toMPSNDArrayGather = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayGather) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayBinaryKernel (Id MPSNDArrayGather) where
  toMPSNDArrayBinaryKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayGather) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayGather) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayGather) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayQuantizedMatrixMultiplication ----------

-- | MPSNDArrayQuantizedMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A quantized matrix multiplication kernel: C = AB, where each input A and B can be quantized.
--
-- The kernel works with 2-8 inputs, order of inputs: First all LHS inputs, then all RHS inputs.              The order of inputs for LUT based LHS or RHS: 1) quantized input 2) Lookup Table.              The order of inputs for affine LHS or RHS: 1) quantized input 2) scale 3) zeropoint 4) minValue.              The full order of inputs for the encode methods is:                   `[LHS, RHS, <LHS quantization inputs>, <RHS quantization inputs>]`,              where @LHS@ is the left input (quantized or float) @RHS@ is the right input (quantized or float) and              `<LHS quantization inputs>` are the auxiliary quantization inputs for the LHS array (scales, zeropoints etc).              and `<RHS quantization inputs>` are the auxiliary quantization input for the RHS array.              The inputs are provided as a compacted `NSArray<MPSNDArray *>`, for example for computing              @C = A * B^T@ where @A@ is quantized with a LUT and @B@ is quantized with affine quantization that              uses scale and minValue the array of inputs is:              @ [ Aq, Bq^T, ALUT, BScale^T, BMin^T ] @.              NOTE: For affine scale, zeropoint and minValue must have same transposes as quantized input.
-- 
-- Phantom type for @MPSNDArrayQuantizedMatrixMultiplication@.
data MPSNDArrayQuantizedMatrixMultiplication

instance IsObjCObject (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayQuantizedMatrixMultiplication"

class IsMPSNDArrayMatrixMultiplication a => IsMPSNDArrayQuantizedMatrixMultiplication a where
  toMPSNDArrayQuantizedMatrixMultiplication :: a -> Id MPSNDArrayQuantizedMatrixMultiplication

instance IsMPSNDArrayQuantizedMatrixMultiplication (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toMPSNDArrayQuantizedMatrixMultiplication = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMatrixMultiplication (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toMPSNDArrayMatrixMultiplication = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayQuantizedMatrixMultiplication) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayIdentity ----------

-- | MPSNDArrayIdentityKernel
--
-- This depends on Metal.framework.
--
-- An efficient kernel to handle copies, transposed-copies and reshapes.
-- 
-- Phantom type for @MPSNDArrayIdentity@.
data MPSNDArrayIdentity

instance IsObjCObject (Id MPSNDArrayIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayIdentity"

class IsMPSNDArrayUnaryKernel a => IsMPSNDArrayIdentity a where
  toMPSNDArrayIdentity :: a -> Id MPSNDArrayIdentity

instance IsMPSNDArrayIdentity (Id MPSNDArrayIdentity) where
  toMPSNDArrayIdentity = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayIdentity) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayIdentity) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayIdentity) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsMPSNDArrayUnaryKernel (Id MPSNDArrayIdentity) where
  toMPSNDArrayUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayIdentity) where
  toNSObject = unsafeCastId

-- ---------- MPSNDArrayStridedSlice ----------

-- | MPSNDStridedSlice
--
-- This depends on Metal.framework.
--
-- Extracts a subset of the source array using the specified slice strides.
-- 
-- Phantom type for @MPSNDArrayStridedSlice@.
data MPSNDArrayStridedSlice

instance IsObjCObject (Id MPSNDArrayStridedSlice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSNDArrayStridedSlice"

class IsMPSNDArrayUnaryKernel a => IsMPSNDArrayStridedSlice a where
  toMPSNDArrayStridedSlice :: a -> Id MPSNDArrayStridedSlice

instance IsMPSNDArrayStridedSlice (Id MPSNDArrayStridedSlice) where
  toMPSNDArrayStridedSlice = unsafeCastId

instance IsMPSKernel (Id MPSNDArrayStridedSlice) where
  toMPSKernel = unsafeCastId

instance IsMPSNDArrayMultiaryBase (Id MPSNDArrayStridedSlice) where
  toMPSNDArrayMultiaryBase = unsafeCastId

instance IsMPSNDArrayMultiaryKernel (Id MPSNDArrayStridedSlice) where
  toMPSNDArrayMultiaryKernel = unsafeCastId

instance IsMPSNDArrayUnaryKernel (Id MPSNDArrayStridedSlice) where
  toMPSNDArrayUnaryKernel = unsafeCastId

instance IsNSObject (Id MPSNDArrayStridedSlice) where
  toNSObject = unsafeCastId

-- ---------- MPSImageLaplacianPyramidAdd ----------

-- | Phantom type for @MPSImageLaplacianPyramidAdd@.
data MPSImageLaplacianPyramidAdd

instance IsObjCObject (Id MPSImageLaplacianPyramidAdd) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageLaplacianPyramidAdd"

class IsMPSImageLaplacianPyramid a => IsMPSImageLaplacianPyramidAdd a where
  toMPSImageLaplacianPyramidAdd :: a -> Id MPSImageLaplacianPyramidAdd

instance IsMPSImageLaplacianPyramidAdd (Id MPSImageLaplacianPyramidAdd) where
  toMPSImageLaplacianPyramidAdd = unsafeCastId

instance IsMPSImageLaplacianPyramid (Id MPSImageLaplacianPyramidAdd) where
  toMPSImageLaplacianPyramid = unsafeCastId

instance IsMPSImagePyramid (Id MPSImageLaplacianPyramidAdd) where
  toMPSImagePyramid = unsafeCastId

instance IsMPSKernel (Id MPSImageLaplacianPyramidAdd) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageLaplacianPyramidAdd) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageLaplacianPyramidAdd) where
  toNSObject = unsafeCastId

-- ---------- MPSImageLaplacianPyramidSubtract ----------

-- | Phantom type for @MPSImageLaplacianPyramidSubtract@.
data MPSImageLaplacianPyramidSubtract

instance IsObjCObject (Id MPSImageLaplacianPyramidSubtract) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSImageLaplacianPyramidSubtract"

class IsMPSImageLaplacianPyramid a => IsMPSImageLaplacianPyramidSubtract a where
  toMPSImageLaplacianPyramidSubtract :: a -> Id MPSImageLaplacianPyramidSubtract

instance IsMPSImageLaplacianPyramidSubtract (Id MPSImageLaplacianPyramidSubtract) where
  toMPSImageLaplacianPyramidSubtract = unsafeCastId

instance IsMPSImageLaplacianPyramid (Id MPSImageLaplacianPyramidSubtract) where
  toMPSImageLaplacianPyramid = unsafeCastId

instance IsMPSImagePyramid (Id MPSImageLaplacianPyramidSubtract) where
  toMPSImagePyramid = unsafeCastId

instance IsMPSKernel (Id MPSImageLaplacianPyramidSubtract) where
  toMPSKernel = unsafeCastId

instance IsMPSUnaryImageKernel (Id MPSImageLaplacianPyramidSubtract) where
  toMPSUnaryImageKernel = unsafeCastId

instance IsNSObject (Id MPSImageLaplacianPyramidSubtract) where
  toNSObject = unsafeCastId
