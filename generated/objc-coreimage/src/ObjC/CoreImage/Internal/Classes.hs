{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreImage.Internal.Classes (
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Quartz.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Quartz.Internal.Classes

-- ---------- CIBarcodeDescriptor ----------

-- | An abstract base class that represents a machine-readable code's attributes.
--
-- Subclasses encapsulate the formal specification and fields specific to a code type.  Each subclass is sufficient to recreate the unique symbol exactly as seen or used with a custom parser.
-- 
-- Phantom type for @CIBarcodeDescriptor@.
data CIBarcodeDescriptor

instance IsObjCObject (Id CIBarcodeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIBarcodeDescriptor"

class IsNSObject a => IsCIBarcodeDescriptor a where
  toCIBarcodeDescriptor :: a -> Id CIBarcodeDescriptor

instance IsCIBarcodeDescriptor (Id CIBarcodeDescriptor) where
  toCIBarcodeDescriptor = unsafeCastId

instance IsNSObject (Id CIBarcodeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CIColor ----------

-- | The Core Image class that defines a color object.
--
-- Use @CIColor@ instances in conjunction with other Core Image classes, such as ``CIFilter-class`` and ``CIKernel``. Many of the built-in Core Image filters have one or more @CIColor@ inputs that you can set to affect the filter's behavior.
--
-- ### Color Model
--
-- A color is defined as a N-dimensional model where each dimension's color component is represented by intensity values. A color component may also be referred to as a color channel. An RGB color model, for example,  is three-dimensional and the red, green, and blue component intensities define each unique color.
--
-- ### Color Space
--
-- A color is also defined by a color space that locates the axes of N-dimensional model within the greater volume of human perceivable colors.  Core Image uses @CGColorSpace@ instances to specify a variety of different color spaces such as sRGB, P3, BT.2020, etc. The @CGColorSpace@ also defines if the color space is coded linearly or in a non-linear perceptual curve. (For more information on @CGColorSpace@ see <doc://com.apple.documentation/documentation/coregraphics/cgcolorspace>)
--
-- ### Color Range
--
-- Standard dynamic range (SDR) color color component values range from @0.0@ to @1.0@, with @0.0@  representing an 0% of that component and @1.0@ representing 100%. In contrast, high dynamic range (HDR) color values  can be less than @0.0@ (for more saturation) or greater than @1.0@ (for more brightness).
--
-- ### Color Opacity
--
-- @CIColor@ instances also have an alpha component, which represents the opacity of the color, with 0.0 meaning completely  transparent and 1.0 meaning completely opaque. If a color does not have an explicit alpha component, Core Image  assumes that the alpha component equals 1.0. With @CIColor@ that color components values are not premultiplied.  So for example, a semi-transparent pure red @CIColor@ is represented by RGB @1.0,0.0,0.0@ and A @0.5@.  In contrast  color components values in ``CIImage`` buffers or read in ``CIKernel`` samplers are premultiplied by default.
-- 
-- Phantom type for @CIColor@.
data CIColor

instance IsObjCObject (Id CIColor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIColor"

class IsNSObject a => IsCIColor a where
  toCIColor :: a -> Id CIColor

instance IsCIColor (Id CIColor) where
  toCIColor = unsafeCastId

instance IsNSObject (Id CIColor) where
  toNSObject = unsafeCastId

-- ---------- CIContext ----------

-- | The Core Image context class provides an evaluation context for Core Image processing with Metal, OpenGL, or OpenCL.
--
-- You use a @CIContext@ instance to render a ``CIImage`` instance which represents a graph of image processing operations which are built using other Core Image classes, such as ``CIFilter-class``, ``CIKernel``, ``CIColor`` and ``CIImage``.  You can also use a @CIContext@ with the ``CIDetector`` class to analyze images — for example, to detect faces  or barcodes.
--
-- Contexts support automatic color management by performing all processing operations in a working color space. This means that unless told otherwise: * All input images are color matched from the input's color space to the working space. * All renders are color matched from the working space to the destination space. (For more information on @CGColorSpace@ see <doc://com.apple.documentation/documentation/coregraphics/cgcolorspace>)
--
-- @CIContext@ and ``CIImage`` instances are immutable, so multiple threads can use the same ``CIContext`` instance  to render ``CIImage`` instances. However, ``CIFilter-class`` instances are mutable and thus cannot be shared safely among  threads. Each thread must take case not to access or modify a ``CIFilter-class`` instance while it is being used by  another thread.
--
-- The @CIContext@ manages various internal state such as @MTLCommandQueue@ and caches for compiled kernels and intermediate buffers.  For this reason it is not recommended to create many @CIContext@ instances.  As a rule, it recommended that you create one @CIContext@ instance for each view that renders ``CIImage`` or each background task.
-- 
-- Phantom type for @CIContext@.
data CIContext

instance IsObjCObject (Id CIContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIContext"

class IsNSObject a => IsCIContext a where
  toCIContext :: a -> Id CIContext

instance IsCIContext (Id CIContext) where
  toCIContext = unsafeCastId

instance IsNSObject (Id CIContext) where
  toNSObject = unsafeCastId

-- ---------- CIDetector ----------

-- | Detects features in images.
--
-- This class potentially holds onto a lot of state. Hence it may be beneficial from a performance perspective to re-use the same CIDetector instance. Specifying a CIContext when creating a detector may have an impact on performance since this context may be used when analyzing an image.
-- 
-- Phantom type for @CIDetector@.
data CIDetector

instance IsObjCObject (Id CIDetector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIDetector"

class IsNSObject a => IsCIDetector a where
  toCIDetector :: a -> Id CIDetector

instance IsCIDetector (Id CIDetector) where
  toCIDetector = unsafeCastId

instance IsNSObject (Id CIDetector) where
  toNSObject = unsafeCastId

-- ---------- CIFeature ----------

-- | The abstract superclass for objects representing notable features detected in an image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features. See <doc://com.apple.documentation/documentation/vision/vnobservation>)
--
-- A @CIFeature@ object represents a portion of an image that a detector believes matches its criteria.  Subclasses of CIFeature holds additional information specific to the detector that discovered the feature.
-- 
-- Phantom type for @CIFeature@.
data CIFeature

instance IsObjCObject (Id CIFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIFeature"

class IsNSObject a => IsCIFeature a where
  toCIFeature :: a -> Id CIFeature

instance IsCIFeature (Id CIFeature) where
  toCIFeature = unsafeCastId

instance IsNSObject (Id CIFeature) where
  toNSObject = unsafeCastId

-- ---------- CIFilter ----------

-- | CIFilter are filter objects for Core Image that encapsulate the filter with its attributes
--
-- The CIFilter class produces a CIImage object as output. Typically, a filter takes one or more images as input. Some filters, however, generate an image based on other types of input parameters. The parameters of a CIFilter object are set and retrieved through the use of key-value pairs. You use the CIFilter object in conjunction with the CIImage, CIContext, CIVector, CIImageAccumulator, and CIColor objects to take advantage of the built-in Core Image filters when processing images. CIFilter objects are also used along with CIKernel, CISampler, and CIFilterShape objects to create custom filters.
-- 
-- Phantom type for @CIFilter@.
data CIFilter

instance IsObjCObject (Id CIFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIFilter"

class IsNSObject a => IsCIFilter a where
  toCIFilter :: a -> Id CIFilter

instance IsCIFilter (Id CIFilter) where
  toCIFilter = unsafeCastId

instance IsNSObject (Id CIFilter) where
  toNSObject = unsafeCastId

-- ---------- CIFilterGenerator ----------

-- | The goal is to CIFilters to be connected and form a single CIFilter for ease of reusability.
--
-- The CIFilterGenerator allows developers to create complex effects built out of one or more CIFilter and reuse them without changing code. The resulting CIFilterGenerator can be written into a file for which we introduce a new file type (extension). A CIFilterGenerator can be created from the API or more conveniently through an editor view that we provide. CIFilterGenerator files can be put into the Image Units folders on the system and they will be loaded when the user invokes one of the loadPlugIns methods. They will be registered by their filename or if present by an attribute in its description.
-- 
-- Phantom type for @CIFilterGenerator@.
data CIFilterGenerator

instance IsObjCObject (Id CIFilterGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIFilterGenerator"

class IsNSObject a => IsCIFilterGenerator a where
  toCIFilterGenerator :: a -> Id CIFilterGenerator

instance IsCIFilterGenerator (Id CIFilterGenerator) where
  toCIFilterGenerator = unsafeCastId

instance IsNSObject (Id CIFilterGenerator) where
  toNSObject = unsafeCastId

-- ---------- CIFilterShape ----------

-- | Phantom type for @CIFilterShape@.
data CIFilterShape

instance IsObjCObject (Id CIFilterShape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIFilterShape"

class IsNSObject a => IsCIFilterShape a where
  toCIFilterShape :: a -> Id CIFilterShape

instance IsCIFilterShape (Id CIFilterShape) where
  toCIFilterShape = unsafeCastId

instance IsNSObject (Id CIFilterShape) where
  toNSObject = unsafeCastId

-- ---------- CIImage ----------

-- | Phantom type for @CIImage@.
data CIImage

instance IsObjCObject (Id CIImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIImage"

class IsNSObject a => IsCIImage a where
  toCIImage :: a -> Id CIImage

instance IsCIImage (Id CIImage) where
  toCIImage = unsafeCastId

instance IsNSObject (Id CIImage) where
  toNSObject = unsafeCastId

-- ---------- CIImageAccumulator ----------

-- | Phantom type for @CIImageAccumulator@.
data CIImageAccumulator

instance IsObjCObject (Id CIImageAccumulator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIImageAccumulator"

class IsNSObject a => IsCIImageAccumulator a where
  toCIImageAccumulator :: a -> Id CIImageAccumulator

instance IsCIImageAccumulator (Id CIImageAccumulator) where
  toCIImageAccumulator = unsafeCastId

instance IsNSObject (Id CIImageAccumulator) where
  toNSObject = unsafeCastId

-- ---------- CIImageProcessorKernel ----------

-- | The abstract class you extend to create custom image processors that can integrate with Core Image workflows.
--
-- Unlike the ``CIKernel`` class and its other subclasses that allow you to create new image-processing effects  with the Core Image Kernel Language, the @CIImageProcessorKernel@ class provides direct access to the underlying  bitmap image data for a step in the Core Image processing pipeline. As such, you can create subclasses of this  class to integrate other image-processing technologies—such as Metal compute shaders, Metal Performance Shaders,  Accelerate vImage operations, or your own CPU-based image-processing routines—with a Core Image filter chain.
--
-- Your custom image processing operation is invoked by your subclassed image processor kernel's  ``processWithInputs:arguments:output:error:`` method. The method can accept zero, one or more @input@ objects.  Processors  that generate imagery (such as a noise or pattern generator) need no inputs, while kernels that  composite source images together require multiple inputs. The @arguments@ dictionary allows the caller to pass in  additional parameter values (such as the radius of a blur) and the @output@ contains the destination for your  image processing code to write to.
--
-- The following code shows how you can subclass @CIImageProcessorKernel@ to apply the Metal Performance Shader  <doc://com.apple.documentation/documentation/metalperformanceshaders/mpsimagethresholdbinary> kernel to a ``CIImage``:
--
-- ```swift class ThresholdImageProcessorKernel: CIImageProcessorKernel { override class func process(with inputs: [CIImageProcessorInput]?, arguments: [String : Any]?, output: CIImageProcessorOutput) throws {     guard         let commandBuffer = output.metalCommandBuffer,         let input = inputs?.first,         let sourceTexture = input.metalTexture,         let destinationTexture = output.metalTexture,         let thresholdValue = arguments?["thresholdValue"] as? Float else {             return         }
--
-- let threshold = MPSImageThresholdBinary(         device: commandBuffer.device,         thresholdValue: thresholdValue,                 maximumValue: 1.0,         linearGrayColorTransform: nil)
--
-- threshold.encode(         commandBuffer: commandBuffer,         sourceTexture: sourceTexture,         destinationTexture: destinationTexture)     } } ```
--
-- To apply to kernel to an image, the calling side invokes the image processor's @apply(withExtent:inputs:arguments:)@  method. The following code generates a new ``CIImage`` object named @result@ which contains a thresholded version of  the source image, @inputImage@.
--
-- ```swift let result = try? ThresholdImageProcessorKernel.apply(      withExtent: inputImage.extent,                 inputs: [inputImage],                 arguments: ["thresholdValue": 0.25]) ```
--
-- > Important: Core Image will concatenate kernels in a render into as fewer programs as possible, avoiding the creation of intermediate buffers. However, it is unable to do this with image processor kernels. To get the best performance,  you should use @CIImageProcessorKernel@ objects only when your algorithms can't be expressed as a ``CIKernel``.
--
-- ## Subclassing Notes
--
-- The @CIImageProcessorKernel@ class is abstract; to create a custom image processor, you define a subclass of this class.
--
-- You do not directly create instances of a custom @CIImageProcessorKernel@ subclass. Image processors must not carry or  use state specific to any single invocation of the processor, so all methods (and accessors for readonly properties)  of an image processor kernel class are class methods.
--
-- Your subclass should override at least the ``processWithInputs:arguments:output:error:`` method to perform its image processing.
--
-- If your image processor needs to work with a larger or smaller region of interest in the input image than each  corresponding region of the output image (for example, a blur filter, which samples several input pixels for  each output pixel), you should also override the ``roiForInput:arguments:outputRect:`` method.
--
-- You can also override the formatForInputAtIndex: method and outputFormat property getter to customize the input  and output pixel formats for your processor (for example, as part of a multi-step workflow where you extract a  single channel from an RGBA image, apply an effect to that channel only, then recombine the channels).
--
-- ## Using a Custom Image Processor
--
-- To apply your custom image processor class to create a ``CIImage`` object, call the  ``applyWithExtent:inputs:arguments:error:`` class method. (Do not override this method.)
-- 
-- Phantom type for @CIImageProcessorKernel@.
data CIImageProcessorKernel

instance IsObjCObject (Id CIImageProcessorKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIImageProcessorKernel"

class IsNSObject a => IsCIImageProcessorKernel a where
  toCIImageProcessorKernel :: a -> Id CIImageProcessorKernel

instance IsCIImageProcessorKernel (Id CIImageProcessorKernel) where
  toCIImageProcessorKernel = unsafeCastId

instance IsNSObject (Id CIImageProcessorKernel) where
  toNSObject = unsafeCastId

-- ---------- CIKernel ----------

-- | Phantom type for @CIKernel@.
data CIKernel

instance IsObjCObject (Id CIKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIKernel"

class IsNSObject a => IsCIKernel a where
  toCIKernel :: a -> Id CIKernel

instance IsCIKernel (Id CIKernel) where
  toCIKernel = unsafeCastId

instance IsNSObject (Id CIKernel) where
  toNSObject = unsafeCastId

-- ---------- CIPlugIn ----------

-- | The CIPlugIn class is responsible for loading Image Units.
--
-- The implementation of the CIPlugIn objects is private. An application can, however, call the 2 public class method to load plug-ins.
--
-- Loading executable CIFilter plugins is deprecated starting in macOS 10.15.
-- 
-- Phantom type for @CIPlugIn@.
data CIPlugIn

instance IsObjCObject (Id CIPlugIn) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIPlugIn"

class IsNSObject a => IsCIPlugIn a where
  toCIPlugIn :: a -> Id CIPlugIn

instance IsCIPlugIn (Id CIPlugIn) where
  toCIPlugIn = unsafeCastId

instance IsNSObject (Id CIPlugIn) where
  toNSObject = unsafeCastId

-- ---------- CIRenderDestination ----------

-- | Phantom type for @CIRenderDestination@.
data CIRenderDestination

instance IsObjCObject (Id CIRenderDestination) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIRenderDestination"

class IsNSObject a => IsCIRenderDestination a where
  toCIRenderDestination :: a -> Id CIRenderDestination

instance IsCIRenderDestination (Id CIRenderDestination) where
  toCIRenderDestination = unsafeCastId

instance IsNSObject (Id CIRenderDestination) where
  toNSObject = unsafeCastId

-- ---------- CIRenderInfo ----------

-- | Phantom type for @CIRenderInfo@.
data CIRenderInfo

instance IsObjCObject (Id CIRenderInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIRenderInfo"

class IsNSObject a => IsCIRenderInfo a where
  toCIRenderInfo :: a -> Id CIRenderInfo

instance IsCIRenderInfo (Id CIRenderInfo) where
  toCIRenderInfo = unsafeCastId

instance IsNSObject (Id CIRenderInfo) where
  toNSObject = unsafeCastId

-- ---------- CIRenderTask ----------

-- | Phantom type for @CIRenderTask@.
data CIRenderTask

instance IsObjCObject (Id CIRenderTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIRenderTask"

class IsNSObject a => IsCIRenderTask a where
  toCIRenderTask :: a -> Id CIRenderTask

instance IsCIRenderTask (Id CIRenderTask) where
  toCIRenderTask = unsafeCastId

instance IsNSObject (Id CIRenderTask) where
  toNSObject = unsafeCastId

-- ---------- CISampler ----------

-- | Phantom type for @CISampler@.
data CISampler

instance IsObjCObject (Id CISampler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CISampler"

class IsNSObject a => IsCISampler a where
  toCISampler :: a -> Id CISampler

instance IsCISampler (Id CISampler) where
  toCISampler = unsafeCastId

instance IsNSObject (Id CISampler) where
  toNSObject = unsafeCastId

-- ---------- CIVector ----------

-- | The Core Image class that defines a vector object.
--
-- A @CIVector@ can store one or more @CGFloat@ in one object. They can store a group of float values for a variety of different uses such as coordinate points, direction vectors, geometric rectangles,  transform matrices, convolution weights, or just a list a parameter values.
--
-- You use @CIVector@ objects in conjunction with other Core Image classes, such as ``CIFilter-class``  and ``CIKernel``.  Many of the built-in Core Image filters have one or more @CIVector@ inputs that  you can set to affect the filter's behavior.
-- 
-- Phantom type for @CIVector@.
data CIVector

instance IsObjCObject (Id CIVector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIVector"

class IsNSObject a => IsCIVector a where
  toCIVector :: a -> Id CIVector

instance IsCIVector (Id CIVector) where
  toCIVector = unsafeCastId

instance IsNSObject (Id CIVector) where
  toNSObject = unsafeCastId

-- ---------- IOSurface ----------

-- | Phantom type for @IOSurface@.
data IOSurface

instance IsObjCObject (Id IOSurface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOSurface"

class IsNSObject a => IsIOSurface a where
  toIOSurface :: a -> Id IOSurface

instance IsIOSurface (Id IOSurface) where
  toIOSurface = unsafeCastId

instance IsNSObject (Id IOSurface) where
  toNSObject = unsafeCastId

-- ---------- CIAztecCodeDescriptor ----------

-- | A concrete subclass the Core Image Barcode Descriptor that represents an Aztec code symbol.
--
-- An Aztec code symbol is a 2D barcode format defined by the ISO/IEC 24778:2008 standard.  It encodes data in concentric square rings around a central bullseye pattern.
-- 
-- Phantom type for @CIAztecCodeDescriptor@.
data CIAztecCodeDescriptor

instance IsObjCObject (Id CIAztecCodeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIAztecCodeDescriptor"

class IsCIBarcodeDescriptor a => IsCIAztecCodeDescriptor a where
  toCIAztecCodeDescriptor :: a -> Id CIAztecCodeDescriptor

instance IsCIAztecCodeDescriptor (Id CIAztecCodeDescriptor) where
  toCIAztecCodeDescriptor = unsafeCastId

instance IsCIBarcodeDescriptor (Id CIAztecCodeDescriptor) where
  toCIBarcodeDescriptor = unsafeCastId

instance IsNSObject (Id CIAztecCodeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CIDataMatrixCodeDescriptor ----------

-- | A concrete subclass the Core Image Barcode Descriptor that represents an Data Matrix code symbol.
--
-- A Data Matrix code symbol is a 2D barcode format defined by the ISO/IEC 16022:2006(E) standard.  It encodes data in square or rectangular symbol with solid lines on the left and bottom sides
-- 
-- Phantom type for @CIDataMatrixCodeDescriptor@.
data CIDataMatrixCodeDescriptor

instance IsObjCObject (Id CIDataMatrixCodeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIDataMatrixCodeDescriptor"

class IsCIBarcodeDescriptor a => IsCIDataMatrixCodeDescriptor a where
  toCIDataMatrixCodeDescriptor :: a -> Id CIDataMatrixCodeDescriptor

instance IsCIDataMatrixCodeDescriptor (Id CIDataMatrixCodeDescriptor) where
  toCIDataMatrixCodeDescriptor = unsafeCastId

instance IsCIBarcodeDescriptor (Id CIDataMatrixCodeDescriptor) where
  toCIBarcodeDescriptor = unsafeCastId

instance IsNSObject (Id CIDataMatrixCodeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CIPDF417CodeDescriptor ----------

-- | A concrete subclass of Core Image Barcode Descriptor that represents a PDF417 symbol.
--
-- PDF417 is a stacked linear barcode symbol format used predominantly in transport, ID cards,  and inventory management. Each pattern in the code comprises 4 bars and spaces, 17 units long.
--
-- Refer to the ISO/IEC 15438:2006(E) for the PDF417 symbol specification.
-- 
-- Phantom type for @CIPDF417CodeDescriptor@.
data CIPDF417CodeDescriptor

instance IsObjCObject (Id CIPDF417CodeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIPDF417CodeDescriptor"

class IsCIBarcodeDescriptor a => IsCIPDF417CodeDescriptor a where
  toCIPDF417CodeDescriptor :: a -> Id CIPDF417CodeDescriptor

instance IsCIPDF417CodeDescriptor (Id CIPDF417CodeDescriptor) where
  toCIPDF417CodeDescriptor = unsafeCastId

instance IsCIBarcodeDescriptor (Id CIPDF417CodeDescriptor) where
  toCIBarcodeDescriptor = unsafeCastId

instance IsNSObject (Id CIPDF417CodeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CIQRCodeDescriptor ----------

-- | A concrete subclass of the Core Image Barcode Descriptor that represents a square QR code symbol.
--
-- ISO/IEC 18004 defines versions from 1 to 40, where a higher symbol version indicates a  larger data-carrying capacity. QR Codes can encode text, vCard contact information, or Uniform Resource Identifiers (URI).
-- 
-- Phantom type for @CIQRCodeDescriptor@.
data CIQRCodeDescriptor

instance IsObjCObject (Id CIQRCodeDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIQRCodeDescriptor"

class IsCIBarcodeDescriptor a => IsCIQRCodeDescriptor a where
  toCIQRCodeDescriptor :: a -> Id CIQRCodeDescriptor

instance IsCIQRCodeDescriptor (Id CIQRCodeDescriptor) where
  toCIQRCodeDescriptor = unsafeCastId

instance IsCIBarcodeDescriptor (Id CIQRCodeDescriptor) where
  toCIBarcodeDescriptor = unsafeCastId

instance IsNSObject (Id CIQRCodeDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CIFaceFeature ----------

-- | Information about a face detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces this  class for identifying and analyzing image features. See @VNDetectFaceRectanglesRequest@. See <doc://com.apple.documentation/documentation/vision/vndetectfacerectanglesrequest>)
--
-- The properties of a @CIFaceFeature@ object provide information about the face’s eyes and mouth.  A face object in a video can also have properties that track its location over time, tracking ID and frame count.
-- 
-- Phantom type for @CIFaceFeature@.
data CIFaceFeature

instance IsObjCObject (Id CIFaceFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIFaceFeature"

class IsCIFeature a => IsCIFaceFeature a where
  toCIFaceFeature :: a -> Id CIFaceFeature

instance IsCIFaceFeature (Id CIFaceFeature) where
  toCIFaceFeature = unsafeCastId

instance IsCIFeature (Id CIFaceFeature) where
  toCIFeature = unsafeCastId

instance IsNSObject (Id CIFaceFeature) where
  toNSObject = unsafeCastId

-- ---------- CIQRCodeFeature ----------

-- | Information about a Quick Response code detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vndetectbarcodesrequest>)
--
-- A QR code is a two-dimensional barcode using the ISO/IEC 18004:2006 standard. The properties of  a CIQRCodeFeature object identify the corners of the barcode in the image perspective and provide  the decoded message.
--
-- To detect QR codes in an image or video, choose ``CIDetectorTypeQRCode`` type when initializing a ``CIDetector`` object.
-- 
-- Phantom type for @CIQRCodeFeature@.
data CIQRCodeFeature

instance IsObjCObject (Id CIQRCodeFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIQRCodeFeature"

class IsCIFeature a => IsCIQRCodeFeature a where
  toCIQRCodeFeature :: a -> Id CIQRCodeFeature

instance IsCIQRCodeFeature (Id CIQRCodeFeature) where
  toCIQRCodeFeature = unsafeCastId

instance IsCIFeature (Id CIQRCodeFeature) where
  toCIFeature = unsafeCastId

instance IsNSObject (Id CIQRCodeFeature) where
  toNSObject = unsafeCastId

-- ---------- CIRectangleFeature ----------

-- | Information about a rectangular region detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vndetectfacerectanglesrequest>)
--
-- A detected rectangle feature is not necessarily rectangular in the plane of the image; rather, the  feature identifies a shape that may be rectangular in space (for example a book on a desk) but which  appears as a four-sided polygon in the image. The properties of a @CIRectangleFeature@ object  identify its four corners in image coordinates.
--
-- You can use rectangle feature detection together with the @CIPerspectiveCorrection@ filter  to transform the feature to a normal orientation.
--
-- To detect rectangles in an image or video, choose ``CIDetectorTypeRectangle`` when initializing a  ``CIDetector`` object, and use the @CIDetectorAspectRatio@ and @CIDetectorFocalLength@ options to  specify the approximate shape of rectangular features to search for. The detector returns at  most one rectangle feature, the most prominent found in the image.
-- 
-- Phantom type for @CIRectangleFeature@.
data CIRectangleFeature

instance IsObjCObject (Id CIRectangleFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIRectangleFeature"

class IsCIFeature a => IsCIRectangleFeature a where
  toCIRectangleFeature :: a -> Id CIRectangleFeature

instance IsCIRectangleFeature (Id CIRectangleFeature) where
  toCIRectangleFeature = unsafeCastId

instance IsCIFeature (Id CIRectangleFeature) where
  toCIFeature = unsafeCastId

instance IsNSObject (Id CIRectangleFeature) where
  toNSObject = unsafeCastId

-- ---------- CITextFeature ----------

-- | Information about a text that was detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vnrecognizetextrequest>)
--
-- A detected text feature is not necessarily rectangular in the plane of the image; rather, the  feature identifies a shape that may be rectangular in space (for example a text on a sign) but which  appears as a four-sided polygon in the image. The properties of a @CITextFeature@ object  identify its four corners in image coordinates.
--
-- To detect text in an image or video, choose the ``CIDetectorTypeText`` type when initializing a  ``CIDetector`` object, and use the @CIDetectorImageOrientation@ option to specify the desired  orientation for finding upright text.
-- 
-- Phantom type for @CITextFeature@.
data CITextFeature

instance IsObjCObject (Id CITextFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CITextFeature"

class IsCIFeature a => IsCITextFeature a where
  toCITextFeature :: a -> Id CITextFeature

instance IsCITextFeature (Id CITextFeature) where
  toCITextFeature = unsafeCastId

instance IsCIFeature (Id CITextFeature) where
  toCIFeature = unsafeCastId

instance IsNSObject (Id CITextFeature) where
  toNSObject = unsafeCastId

-- ---------- CIRAWFilter ----------

-- | Phantom type for @CIRAWFilter@.
data CIRAWFilter

instance IsObjCObject (Id CIRAWFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIRAWFilter"

class IsCIFilter a => IsCIRAWFilter a where
  toCIRAWFilter :: a -> Id CIRAWFilter

instance IsCIRAWFilter (Id CIRAWFilter) where
  toCIRAWFilter = unsafeCastId

instance IsCIFilter (Id CIRAWFilter) where
  toCIFilter = unsafeCastId

instance IsNSObject (Id CIRAWFilter) where
  toNSObject = unsafeCastId

-- ---------- CIColorKernel ----------

-- | Phantom type for @CIColorKernel@.
data CIColorKernel

instance IsObjCObject (Id CIColorKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIColorKernel"

class IsCIKernel a => IsCIColorKernel a where
  toCIColorKernel :: a -> Id CIColorKernel

instance IsCIColorKernel (Id CIColorKernel) where
  toCIColorKernel = unsafeCastId

instance IsCIKernel (Id CIColorKernel) where
  toCIKernel = unsafeCastId

instance IsNSObject (Id CIColorKernel) where
  toNSObject = unsafeCastId

-- ---------- CIWarpKernel ----------

-- | Phantom type for @CIWarpKernel@.
data CIWarpKernel

instance IsObjCObject (Id CIWarpKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIWarpKernel"

class IsCIKernel a => IsCIWarpKernel a where
  toCIWarpKernel :: a -> Id CIWarpKernel

instance IsCIWarpKernel (Id CIWarpKernel) where
  toCIWarpKernel = unsafeCastId

instance IsCIKernel (Id CIWarpKernel) where
  toCIKernel = unsafeCastId

instance IsNSObject (Id CIWarpKernel) where
  toNSObject = unsafeCastId

-- ---------- CIBlendKernel ----------

-- | Phantom type for @CIBlendKernel@.
data CIBlendKernel

instance IsObjCObject (Id CIBlendKernel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CIBlendKernel"

class IsCIColorKernel a => IsCIBlendKernel a where
  toCIBlendKernel :: a -> Id CIBlendKernel

instance IsCIBlendKernel (Id CIBlendKernel) where
  toCIBlendKernel = unsafeCastId

instance IsCIColorKernel (Id CIBlendKernel) where
  toCIColorKernel = unsafeCastId

instance IsCIKernel (Id CIBlendKernel) where
  toCIKernel = unsafeCastId

instance IsNSObject (Id CIBlendKernel) where
  toNSObject = unsafeCastId
