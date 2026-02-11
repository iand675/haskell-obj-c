{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Vision.Internal.Classes (
    module ObjC.Vision.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.CoreML.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- VNCircle ----------

-- | VNCircle
--
-- VNCircle is two-dimensional circle represented by the center point 'center' and its radius 'radius'. Once created, VNCircle objects are immutable.
-- 
-- Phantom type for @VNCircle@.
data VNCircle

instance IsObjCObject (Id VNCircle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNCircle"

class IsNSObject a => IsVNCircle a where
  toVNCircle :: a -> Id VNCircle

instance IsVNCircle (Id VNCircle) where
  toVNCircle = unsafeCastId

instance IsNSObject (Id VNCircle) where
  toNSObject = unsafeCastId

-- ---------- VNContour ----------

-- | The VNContour class describes a contour provided by a VNContoursObservation.
--
-- VNContour objects are lightweight objects that act as a façade which allows access to a small slice of the usually much larger block of data owned by a VNContoursObservation that represents all of the contours detected in an image.				While the interface does present the notion of a hierarchy of parent/child contours, the implementation purposefully does not contain any explicit internal bookkeeping for this relationship.  Instead, contours are uniquely identified via their indexPath property.				As a side effect of this choice, repeated calls to methods that would return relational contours (e.g., -childContours or -childContourAtIndex:error:) are NOT guaranteed to return the same VNContour instances over and over again.  If this kind of parent/child object stability is an absolute requirement of the client, then they are responsible for creating the necessary data structures to represent and build that instance-stable hierarchy.
-- 
-- Phantom type for @VNContour@.
data VNContour

instance IsObjCObject (Id VNContour) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNContour"

class IsNSObject a => IsVNContour a where
  toVNContour :: a -> Id VNContour

instance IsVNContour (Id VNContour) where
  toVNContour = unsafeCastId

instance IsNSObject (Id VNContour) where
  toNSObject = unsafeCastId

-- ---------- VNCoreMLModel ----------

-- | The VNCoreMLModel uses an CoreML based model and prepares it for use with VNCoreMLRequests.
-- 
-- Phantom type for @VNCoreMLModel@.
data VNCoreMLModel

instance IsObjCObject (Id VNCoreMLModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNCoreMLModel"

class IsNSObject a => IsVNCoreMLModel a where
  toVNCoreMLModel :: a -> Id VNCoreMLModel

instance IsVNCoreMLModel (Id VNCoreMLModel) where
  toVNCoreMLModel = unsafeCastId

instance IsNSObject (Id VNCoreMLModel) where
  toNSObject = unsafeCastId

-- ---------- VNFaceLandmarkRegion ----------

-- | VNFaceLandmarkRegion
--
-- VNFaceLandmarkRegion is an immutable object acting as a collection of landmark points for defining a specific region of the face (including potentially all of the landmark points for a face). The VNFaceLandmarkRegion is an abstract base class.
-- 
-- Phantom type for @VNFaceLandmarkRegion@.
data VNFaceLandmarkRegion

instance IsObjCObject (Id VNFaceLandmarkRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFaceLandmarkRegion"

class IsNSObject a => IsVNFaceLandmarkRegion a where
  toVNFaceLandmarkRegion :: a -> Id VNFaceLandmarkRegion

instance IsVNFaceLandmarkRegion (Id VNFaceLandmarkRegion) where
  toVNFaceLandmarkRegion = unsafeCastId

instance IsNSObject (Id VNFaceLandmarkRegion) where
  toNSObject = unsafeCastId

-- ---------- VNFaceLandmarks ----------

-- | VNFaceLandmarks
--
-- VNFaceLandmarks2D is the result of a face landmarks request. It is an abstract base class.
-- 
-- Phantom type for @VNFaceLandmarks@.
data VNFaceLandmarks

instance IsObjCObject (Id VNFaceLandmarks) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFaceLandmarks"

class IsNSObject a => IsVNFaceLandmarks a where
  toVNFaceLandmarks :: a -> Id VNFaceLandmarks

instance IsVNFaceLandmarks (Id VNFaceLandmarks) where
  toVNFaceLandmarks = unsafeCastId

instance IsNSObject (Id VNFaceLandmarks) where
  toNSObject = unsafeCastId

-- ---------- VNGeometryUtils ----------

-- | Phantom type for @VNGeometryUtils@.
data VNGeometryUtils

instance IsObjCObject (Id VNGeometryUtils) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGeometryUtils"

class IsNSObject a => IsVNGeometryUtils a where
  toVNGeometryUtils :: a -> Id VNGeometryUtils

instance IsVNGeometryUtils (Id VNGeometryUtils) where
  toVNGeometryUtils = unsafeCastId

instance IsNSObject (Id VNGeometryUtils) where
  toNSObject = unsafeCastId

-- ---------- VNImageRequestHandler ----------

-- | Performs requests on a single image.
--
-- The VNImageRequestHandler is created with an image that is used to be used for the requests a client might want to schedule. The VNImageRequestHandler retains, but never modifies, the image source for its entire lifetime. The client also must not modify the content of the image source once the VNImageRequestHandler is created otherwise the results are undefined.             The VNImageRequestHandler can choose to also cache intermediate representation of the image or other request-specific information for the purposes of runtime performance.
-- 
-- Phantom type for @VNImageRequestHandler@.
data VNImageRequestHandler

instance IsObjCObject (Id VNImageRequestHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageRequestHandler"

class IsNSObject a => IsVNImageRequestHandler a where
  toVNImageRequestHandler :: a -> Id VNImageRequestHandler

instance IsVNImageRequestHandler (Id VNImageRequestHandler) where
  toVNImageRequestHandler = unsafeCastId

instance IsNSObject (Id VNImageRequestHandler) where
  toNSObject = unsafeCastId

-- ---------- VNObservation ----------

-- | VNObservation
--
-- VNObservation describes the results of performing a VNRequest. The result has a confidence score. The different types of requests will create different subclasses of VNObservation to return their results in properties of those subclasses.
-- 
-- Phantom type for @VNObservation@.
data VNObservation

instance IsObjCObject (Id VNObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNObservation"

class IsNSObject a => IsVNObservation a where
  toVNObservation :: a -> Id VNObservation

instance IsVNObservation (Id VNObservation) where
  toVNObservation = unsafeCastId

instance IsNSObject (Id VNObservation) where
  toNSObject = unsafeCastId

-- ---------- VNPoint ----------

-- | VNPoint
--
-- VNPoint represents a single, immutable, two-dimensional point in an image.
--
-- It should be noted that VNPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that need to present points which may contain additional metadata.
-- 
-- Phantom type for @VNPoint@.
data VNPoint

instance IsObjCObject (Id VNPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNPoint"

class IsNSObject a => IsVNPoint a where
  toVNPoint :: a -> Id VNPoint

instance IsVNPoint (Id VNPoint) where
  toVNPoint = unsafeCastId

instance IsNSObject (Id VNPoint) where
  toNSObject = unsafeCastId

-- ---------- VNPoint3D ----------

-- | VNPoint3D
--
-- VNPoint3D represents a single, immutable, three-dimensional point in an image.
--
-- It should be noted that VNPoint3D is not intended as an overall replacement of simd float4x4, but is used by observations that need to present points which may contain additional metadata.
-- 
-- Phantom type for @VNPoint3D@.
data VNPoint3D

instance IsObjCObject (Id VNPoint3D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNPoint3D"

class IsNSObject a => IsVNPoint3D a where
  toVNPoint3D :: a -> Id VNPoint3D

instance IsVNPoint3D (Id VNPoint3D) where
  toVNPoint3D = unsafeCastId

instance IsNSObject (Id VNPoint3D) where
  toNSObject = unsafeCastId

-- ---------- VNRecognizedText ----------

-- | VNRecognizedText
--
-- VNRecognizedText A block of recognized text. There can be multiple VNRecognizedText objects returned in a VNRecognizedTextObservation - one for each candidate.
-- 
-- Phantom type for @VNRecognizedText@.
data VNRecognizedText

instance IsObjCObject (Id VNRecognizedText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedText"

class IsNSObject a => IsVNRecognizedText a where
  toVNRecognizedText :: a -> Id VNRecognizedText

instance IsVNRecognizedText (Id VNRecognizedText) where
  toVNRecognizedText = unsafeCastId

instance IsNSObject (Id VNRecognizedText) where
  toNSObject = unsafeCastId

-- ---------- VNRequest ----------

-- | VNRequest
--
-- VNRequest objects describe the operation to be performed as well as act as the recipient of the operation's resultant observations.
--
-- VNRequest objects are instantiated in a pre-configured nominal state. Prior to sending a VNRequest to a request handler to perform a desired operation, the default configuration can be changed by modifying the values of VNRequest properties. The VNRequest class itself acts as a base class and is not meant to be directly instantiated.
-- 
-- Phantom type for @VNRequest@.
data VNRequest

instance IsObjCObject (Id VNRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRequest"

class IsNSObject a => IsVNRequest a where
  toVNRequest :: a -> Id VNRequest

instance IsVNRequest (Id VNRequest) where
  toVNRequest = unsafeCastId

instance IsNSObject (Id VNRequest) where
  toNSObject = unsafeCastId

-- ---------- VNSequenceRequestHandler ----------

-- | Performs requests on a sequence of images.
--
-- The VNSequenceRequestHandler is created without any specific image source.  The -performRequests:on<ImageSource>:error: methods will retain the image source for no longer than the lifetime of the call.             The VNSequenceRequestHandler can choose to also cache state information related to the previously-processed image sources.
-- 
-- Phantom type for @VNSequenceRequestHandler@.
data VNSequenceRequestHandler

instance IsObjCObject (Id VNSequenceRequestHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNSequenceRequestHandler"

class IsNSObject a => IsVNSequenceRequestHandler a where
  toVNSequenceRequestHandler :: a -> Id VNSequenceRequestHandler

instance IsVNSequenceRequestHandler (Id VNSequenceRequestHandler) where
  toVNSequenceRequestHandler = unsafeCastId

instance IsNSObject (Id VNSequenceRequestHandler) where
  toNSObject = unsafeCastId

-- ---------- VNVector ----------

-- | VNVector
--
-- VNVector is a two-dimensional vector represented its X and Y axis projections. Once created, VNVector objects are immutable.
-- 
-- Phantom type for @VNVector@.
data VNVector

instance IsObjCObject (Id VNVector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVector"

class IsNSObject a => IsVNVector a where
  toVNVector :: a -> Id VNVector

instance IsVNVector (Id VNVector) where
  toVNVector = unsafeCastId

instance IsNSObject (Id VNVector) where
  toNSObject = unsafeCastId

-- ---------- VNVideoProcessor ----------

-- | A controller object that is used to perform one or more requests on a video stream.
--
-- VNVideoProcessor handles the video decoding and buffer management, feeding the buffers to the associated requests at the best desired frame rate.
-- 
-- Phantom type for @VNVideoProcessor@.
data VNVideoProcessor

instance IsObjCObject (Id VNVideoProcessor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVideoProcessor"

class IsNSObject a => IsVNVideoProcessor a where
  toVNVideoProcessor :: a -> Id VNVideoProcessor

instance IsVNVideoProcessor (Id VNVideoProcessor) where
  toVNVideoProcessor = unsafeCastId

instance IsNSObject (Id VNVideoProcessor) where
  toNSObject = unsafeCastId

-- ---------- VNVideoProcessorCadence ----------

-- | An object that defines the cadence at which the video stream is processed.
-- 
-- Phantom type for @VNVideoProcessorCadence@.
data VNVideoProcessorCadence

instance IsObjCObject (Id VNVideoProcessorCadence) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVideoProcessorCadence"

class IsNSObject a => IsVNVideoProcessorCadence a where
  toVNVideoProcessorCadence :: a -> Id VNVideoProcessorCadence

instance IsVNVideoProcessorCadence (Id VNVideoProcessorCadence) where
  toVNVideoProcessorCadence = unsafeCastId

instance IsNSObject (Id VNVideoProcessorCadence) where
  toNSObject = unsafeCastId

-- ---------- VNVideoProcessorRequestProcessingOptions ----------

-- | Options applied to a request's processing of the video.
-- 
-- Phantom type for @VNVideoProcessorRequestProcessingOptions@.
data VNVideoProcessorRequestProcessingOptions

instance IsObjCObject (Id VNVideoProcessorRequestProcessingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVideoProcessorRequestProcessingOptions"

class IsNSObject a => IsVNVideoProcessorRequestProcessingOptions a where
  toVNVideoProcessorRequestProcessingOptions :: a -> Id VNVideoProcessorRequestProcessingOptions

instance IsVNVideoProcessorRequestProcessingOptions (Id VNVideoProcessorRequestProcessingOptions) where
  toVNVideoProcessorRequestProcessingOptions = unsafeCastId

instance IsNSObject (Id VNVideoProcessorRequestProcessingOptions) where
  toNSObject = unsafeCastId

-- ---------- VNFaceLandmarkRegion2D ----------

-- | VNFaceLandmarkRegion2D
--
-- VNFaceLandmarkRegion2D gives access to the 2D landmark points for the region. The points are stored as vector_float2 and must not be modified.
-- 
-- Phantom type for @VNFaceLandmarkRegion2D@.
data VNFaceLandmarkRegion2D

instance IsObjCObject (Id VNFaceLandmarkRegion2D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFaceLandmarkRegion2D"

class IsVNFaceLandmarkRegion a => IsVNFaceLandmarkRegion2D a where
  toVNFaceLandmarkRegion2D :: a -> Id VNFaceLandmarkRegion2D

instance IsVNFaceLandmarkRegion2D (Id VNFaceLandmarkRegion2D) where
  toVNFaceLandmarkRegion2D = unsafeCastId

instance IsNSObject (Id VNFaceLandmarkRegion2D) where
  toNSObject = unsafeCastId

instance IsVNFaceLandmarkRegion (Id VNFaceLandmarkRegion2D) where
  toVNFaceLandmarkRegion = unsafeCastId

-- ---------- VNFaceLandmarks2D ----------

-- | VNFaceLandmarks2D
--
-- VNFaceLandmarks2D is the result of a face landmarks 2D request, containing detected facial landmark points organized into VNFaceLandmarkRegion2D regions. The points are accessible as a full list, or as sub-gruops representing pre-defined facial regions.
-- 
-- Phantom type for @VNFaceLandmarks2D@.
data VNFaceLandmarks2D

instance IsObjCObject (Id VNFaceLandmarks2D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFaceLandmarks2D"

class IsVNFaceLandmarks a => IsVNFaceLandmarks2D a where
  toVNFaceLandmarks2D :: a -> Id VNFaceLandmarks2D

instance IsVNFaceLandmarks2D (Id VNFaceLandmarks2D) where
  toVNFaceLandmarks2D = unsafeCastId

instance IsNSObject (Id VNFaceLandmarks2D) where
  toNSObject = unsafeCastId

instance IsVNFaceLandmarks (Id VNFaceLandmarks2D) where
  toVNFaceLandmarks = unsafeCastId

-- ---------- VNClassificationObservation ----------

-- | VNClassificationObservation
--
-- VNObservation
--
-- VNClassificationObservation returns the classifcation in form of a string.
--
-- VNClassificationObservation is the observation returned by VNCoreMLRequests that using a model that is a classifier. A classifier produces an arrary (this can be a single entry) of classifications which are labels (identifiers) and confidence scores.
-- 
-- Phantom type for @VNClassificationObservation@.
data VNClassificationObservation

instance IsObjCObject (Id VNClassificationObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNClassificationObservation"

class IsVNObservation a => IsVNClassificationObservation a where
  toVNClassificationObservation :: a -> Id VNClassificationObservation

instance IsVNClassificationObservation (Id VNClassificationObservation) where
  toVNClassificationObservation = unsafeCastId

instance IsNSObject (Id VNClassificationObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNClassificationObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNContoursObservation ----------

-- | Phantom type for @VNContoursObservation@.
data VNContoursObservation

instance IsObjCObject (Id VNContoursObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNContoursObservation"

class IsVNObservation a => IsVNContoursObservation a where
  toVNContoursObservation :: a -> Id VNContoursObservation

instance IsVNContoursObservation (Id VNContoursObservation) where
  toVNContoursObservation = unsafeCastId

instance IsNSObject (Id VNContoursObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNContoursObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNCoreMLFeatureValueObservation ----------

-- | VNCoreMLFeatureValueObservation
--
-- VNObservation
--
-- VNCoreMLFeatureValueObservation returns the prediction of a model as an MLFeatureValue.
--
-- This is the returned observations for models that are not classifiers and that do not return an image as a prediction. The confidence for these observations is always 1.0.
-- 
-- Phantom type for @VNCoreMLFeatureValueObservation@.
data VNCoreMLFeatureValueObservation

instance IsObjCObject (Id VNCoreMLFeatureValueObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNCoreMLFeatureValueObservation"

class IsVNObservation a => IsVNCoreMLFeatureValueObservation a where
  toVNCoreMLFeatureValueObservation :: a -> Id VNCoreMLFeatureValueObservation

instance IsVNCoreMLFeatureValueObservation (Id VNCoreMLFeatureValueObservation) where
  toVNCoreMLFeatureValueObservation = unsafeCastId

instance IsNSObject (Id VNCoreMLFeatureValueObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNCoreMLFeatureValueObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNDetectedObjectObservation ----------

-- | VNDetectedObjectObservation
--
-- VNObservation
--
-- VNDetectedObjectObservation is VNObservation in an image that has a location and/or dimension. Further attributes depend on the specific detected object.
--
-- All result objects (faces, scene objects, shapes etc) must extend from this class.
-- 
-- Phantom type for @VNDetectedObjectObservation@.
data VNDetectedObjectObservation

instance IsObjCObject (Id VNDetectedObjectObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectedObjectObservation"

class IsVNObservation a => IsVNDetectedObjectObservation a where
  toVNDetectedObjectObservation :: a -> Id VNDetectedObjectObservation

instance IsVNDetectedObjectObservation (Id VNDetectedObjectObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsNSObject (Id VNDetectedObjectObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNDetectedObjectObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNFeaturePrintObservation ----------

-- | Phantom type for @VNFeaturePrintObservation@.
data VNFeaturePrintObservation

instance IsObjCObject (Id VNFeaturePrintObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFeaturePrintObservation"

class IsVNObservation a => IsVNFeaturePrintObservation a where
  toVNFeaturePrintObservation :: a -> Id VNFeaturePrintObservation

instance IsVNFeaturePrintObservation (Id VNFeaturePrintObservation) where
  toVNFeaturePrintObservation = unsafeCastId

instance IsNSObject (Id VNFeaturePrintObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNFeaturePrintObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNHorizonObservation ----------

-- | VNHorizonObservation
--
-- VNObservation
--
-- VNHorizonObservation is the result of a VNDetectHorizonRequest
--
-- Use the transform or angle to upright the image and make the detected horizon level.
-- 
-- Phantom type for @VNHorizonObservation@.
data VNHorizonObservation

instance IsObjCObject (Id VNHorizonObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHorizonObservation"

class IsVNObservation a => IsVNHorizonObservation a where
  toVNHorizonObservation :: a -> Id VNHorizonObservation

instance IsVNHorizonObservation (Id VNHorizonObservation) where
  toVNHorizonObservation = unsafeCastId

instance IsNSObject (Id VNHorizonObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNHorizonObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNImageAestheticsScoresObservation ----------

-- | VNImageAestheticsScoresObservation
--
-- VNObservation
--
-- VNImageAestheticsScoresObservation provides an overall score of aesthetic attributes for an image.
-- 
-- Phantom type for @VNImageAestheticsScoresObservation@.
data VNImageAestheticsScoresObservation

instance IsObjCObject (Id VNImageAestheticsScoresObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageAestheticsScoresObservation"

class IsVNObservation a => IsVNImageAestheticsScoresObservation a where
  toVNImageAestheticsScoresObservation :: a -> Id VNImageAestheticsScoresObservation

instance IsVNImageAestheticsScoresObservation (Id VNImageAestheticsScoresObservation) where
  toVNImageAestheticsScoresObservation = unsafeCastId

instance IsNSObject (Id VNImageAestheticsScoresObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNImageAestheticsScoresObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNImageAlignmentObservation ----------

-- | VNImageAlignmentObservation
--
-- VNObservation
--
-- VNImageAlignmentObservation is generated from an image registration. This is an abstract base class. The type of registration request used defines which subclass describes the result.
-- 
-- Phantom type for @VNImageAlignmentObservation@.
data VNImageAlignmentObservation

instance IsObjCObject (Id VNImageAlignmentObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageAlignmentObservation"

class IsVNObservation a => IsVNImageAlignmentObservation a where
  toVNImageAlignmentObservation :: a -> Id VNImageAlignmentObservation

instance IsVNImageAlignmentObservation (Id VNImageAlignmentObservation) where
  toVNImageAlignmentObservation = unsafeCastId

instance IsNSObject (Id VNImageAlignmentObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNImageAlignmentObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNInstanceMaskObservation ----------

-- | An observation resulting from an instance mask generation request. It contains an instance mask that labels instances in the mask that labels per pixel an instance.
-- 
-- Phantom type for @VNInstanceMaskObservation@.
data VNInstanceMaskObservation

instance IsObjCObject (Id VNInstanceMaskObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNInstanceMaskObservation"

class IsVNObservation a => IsVNInstanceMaskObservation a where
  toVNInstanceMaskObservation :: a -> Id VNInstanceMaskObservation

instance IsVNInstanceMaskObservation (Id VNInstanceMaskObservation) where
  toVNInstanceMaskObservation = unsafeCastId

instance IsNSObject (Id VNInstanceMaskObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNInstanceMaskObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNPixelBufferObservation ----------

-- | VNPixelBufferObservation
--
-- VNObservation
--
-- VNPixelBufferObservation returns the prediction of a model as a CVPixelBufferRef.
--
-- This is the returned observations for models that are not classifiers and return an image as a prediction. The confidence for these observations is always 1.0.
-- 
-- Phantom type for @VNPixelBufferObservation@.
data VNPixelBufferObservation

instance IsObjCObject (Id VNPixelBufferObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNPixelBufferObservation"

class IsVNObservation a => IsVNPixelBufferObservation a where
  toVNPixelBufferObservation :: a -> Id VNPixelBufferObservation

instance IsVNPixelBufferObservation (Id VNPixelBufferObservation) where
  toVNPixelBufferObservation = unsafeCastId

instance IsNSObject (Id VNPixelBufferObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNPixelBufferObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNRecognizedPoints3DObservation ----------

-- | VNRecognizedPoints3D
--
-- Observation
--
-- VNObservation
--
-- VNRecognizedPointsObservation is a request result detailing points in an image.
-- 
-- Phantom type for @VNRecognizedPoints3DObservation@.
data VNRecognizedPoints3DObservation

instance IsObjCObject (Id VNRecognizedPoints3DObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedPoints3DObservation"

class IsVNObservation a => IsVNRecognizedPoints3DObservation a where
  toVNRecognizedPoints3DObservation :: a -> Id VNRecognizedPoints3DObservation

instance IsVNRecognizedPoints3DObservation (Id VNRecognizedPoints3DObservation) where
  toVNRecognizedPoints3DObservation = unsafeCastId

instance IsNSObject (Id VNRecognizedPoints3DObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNRecognizedPoints3DObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNRecognizedPointsObservation ----------

-- | VNRecognizedPointsObservation
--
-- VNObservation
--
-- VNRecognizedPointsObservation is a request result detailing points in an image.
-- 
-- Phantom type for @VNRecognizedPointsObservation@.
data VNRecognizedPointsObservation

instance IsObjCObject (Id VNRecognizedPointsObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedPointsObservation"

class IsVNObservation a => IsVNRecognizedPointsObservation a where
  toVNRecognizedPointsObservation :: a -> Id VNRecognizedPointsObservation

instance IsVNRecognizedPointsObservation (Id VNRecognizedPointsObservation) where
  toVNRecognizedPointsObservation = unsafeCastId

instance IsNSObject (Id VNRecognizedPointsObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNRecognizedPointsObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNTrajectoryObservation ----------

-- | VNTrajectoryObservation
--
-- VNObservation
--
-- The VNTrajectoryObservation describes a detected trajectory with the points on the trajectory and the equation describing the trajectory. The observation also reprorts the duration describing when the trajectory was first detected (which will be in the past).
-- 
-- Phantom type for @VNTrajectoryObservation@.
data VNTrajectoryObservation

instance IsObjCObject (Id VNTrajectoryObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrajectoryObservation"

class IsVNObservation a => IsVNTrajectoryObservation a where
  toVNTrajectoryObservation :: a -> Id VNTrajectoryObservation

instance IsVNTrajectoryObservation (Id VNTrajectoryObservation) where
  toVNTrajectoryObservation = unsafeCastId

instance IsNSObject (Id VNTrajectoryObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNTrajectoryObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNDetectedPoint ----------

-- | VNDetectedPoint
--
-- VNDetectedPoint is a VNPoint with a confidence value.
--
-- It should be noted that VNDetectedPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that detect points of interest.
-- 
-- Phantom type for @VNDetectedPoint@.
data VNDetectedPoint

instance IsObjCObject (Id VNDetectedPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectedPoint"

class IsVNPoint a => IsVNDetectedPoint a where
  toVNDetectedPoint :: a -> Id VNDetectedPoint

instance IsVNDetectedPoint (Id VNDetectedPoint) where
  toVNDetectedPoint = unsafeCastId

instance IsNSObject (Id VNDetectedPoint) where
  toNSObject = unsafeCastId

instance IsVNPoint (Id VNDetectedPoint) where
  toVNPoint = unsafeCastId

-- ---------- VNRecognizedPoint3D ----------

-- | VNRecognizedPoint3D
--
-- An extension of VNPoint3D that associates an identifier to the point.
--
-- It should be noted that VNRecognizedPoint3D is not intended as an overall replacement of simd float 4x4, but is used by observations that recognize labeled points of interest.
-- 
-- Phantom type for @VNRecognizedPoint3D@.
data VNRecognizedPoint3D

instance IsObjCObject (Id VNRecognizedPoint3D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedPoint3D"

class IsVNPoint3D a => IsVNRecognizedPoint3D a where
  toVNRecognizedPoint3D :: a -> Id VNRecognizedPoint3D

instance IsVNRecognizedPoint3D (Id VNRecognizedPoint3D) where
  toVNRecognizedPoint3D = unsafeCastId

instance IsNSObject (Id VNRecognizedPoint3D) where
  toNSObject = unsafeCastId

instance IsVNPoint3D (Id VNRecognizedPoint3D) where
  toVNPoint3D = unsafeCastId

-- ---------- VNImageBasedRequest ----------

-- | A request that will process the contents of a reference image.
-- 
-- Phantom type for @VNImageBasedRequest@.
data VNImageBasedRequest

instance IsObjCObject (Id VNImageBasedRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageBasedRequest"

class IsVNRequest a => IsVNImageBasedRequest a where
  toVNImageBasedRequest :: a -> Id VNImageBasedRequest

instance IsVNImageBasedRequest (Id VNImageBasedRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsNSObject (Id VNImageBasedRequest) where
  toNSObject = unsafeCastId

instance IsVNRequest (Id VNImageBasedRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNVideoProcessorFrameRateCadence ----------

-- | An object that defines a frame-based cadence for processing the video stream.
-- 
-- Phantom type for @VNVideoProcessorFrameRateCadence@.
data VNVideoProcessorFrameRateCadence

instance IsObjCObject (Id VNVideoProcessorFrameRateCadence) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVideoProcessorFrameRateCadence"

class IsVNVideoProcessorCadence a => IsVNVideoProcessorFrameRateCadence a where
  toVNVideoProcessorFrameRateCadence :: a -> Id VNVideoProcessorFrameRateCadence

instance IsVNVideoProcessorFrameRateCadence (Id VNVideoProcessorFrameRateCadence) where
  toVNVideoProcessorFrameRateCadence = unsafeCastId

instance IsNSObject (Id VNVideoProcessorFrameRateCadence) where
  toNSObject = unsafeCastId

instance IsVNVideoProcessorCadence (Id VNVideoProcessorFrameRateCadence) where
  toVNVideoProcessorCadence = unsafeCastId

-- ---------- VNVideoProcessorTimeIntervalCadence ----------

-- | An object that defines a time-based cadence for processing the video stream.
-- 
-- Phantom type for @VNVideoProcessorTimeIntervalCadence@.
data VNVideoProcessorTimeIntervalCadence

instance IsObjCObject (Id VNVideoProcessorTimeIntervalCadence) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNVideoProcessorTimeIntervalCadence"

class IsVNVideoProcessorCadence a => IsVNVideoProcessorTimeIntervalCadence a where
  toVNVideoProcessorTimeIntervalCadence :: a -> Id VNVideoProcessorTimeIntervalCadence

instance IsVNVideoProcessorTimeIntervalCadence (Id VNVideoProcessorTimeIntervalCadence) where
  toVNVideoProcessorTimeIntervalCadence = unsafeCastId

instance IsNSObject (Id VNVideoProcessorTimeIntervalCadence) where
  toNSObject = unsafeCastId

instance IsVNVideoProcessorCadence (Id VNVideoProcessorTimeIntervalCadence) where
  toVNVideoProcessorCadence = unsafeCastId

-- ---------- VNFaceObservation ----------

-- | VNFaceObservation
--
-- VNDetectedObjectObservation
--
-- VNFaceObservation is the result of a face detection request or derivatives like a face landmark request.
--
-- The properties filled in this obervation depend on the request being performed. For instance, if just a VNDetectFaceRectanglesRequest was performed the landmarks will not be populated. VNFaceObservation are also used as inputs to other request as defined by the VNFaceObservationAccepting protocol. An example would be the VNDetectFaceLandmarksRequest. This can be helpful for instance if the face rectangles in an image are not derived from a VNDetectFaceRectanglesRequest but instead come from other sources like EXIF or other face detectors. In that case the client of the API creates a VNFaceObservation with the boundingBox (in normalized coordinates) that were based on those detected faces.
-- 
-- Phantom type for @VNFaceObservation@.
data VNFaceObservation

instance IsObjCObject (Id VNFaceObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNFaceObservation"

class IsVNDetectedObjectObservation a => IsVNFaceObservation a where
  toVNFaceObservation :: a -> Id VNFaceObservation

instance IsVNFaceObservation (Id VNFaceObservation) where
  toVNFaceObservation = unsafeCastId

instance IsNSObject (Id VNFaceObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNFaceObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNFaceObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNHumanObservation ----------

-- | VNHumanObservation
--
-- VNDetectedObjectObservation
--
-- VNHumanObservation is the result of a Human rectangles detection request
-- 
-- Phantom type for @VNHumanObservation@.
data VNHumanObservation

instance IsObjCObject (Id VNHumanObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHumanObservation"

class IsVNDetectedObjectObservation a => IsVNHumanObservation a where
  toVNHumanObservation :: a -> Id VNHumanObservation

instance IsVNHumanObservation (Id VNHumanObservation) where
  toVNHumanObservation = unsafeCastId

instance IsNSObject (Id VNHumanObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNHumanObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNHumanObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNRecognizedObjectObservation ----------

-- | VNRecognizedObjectObservation
--
-- VNDetectedObjectObservation
--
-- VNRecognizedObjectObservation is a VNDetectedObjectObservation with an array of classifications that classify the recognized object. The confidence of the classifications sum up to 1.0. It is common practice to multiply the classification confidence with the confidence of this observation.
-- 
-- Phantom type for @VNRecognizedObjectObservation@.
data VNRecognizedObjectObservation

instance IsObjCObject (Id VNRecognizedObjectObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedObjectObservation"

class IsVNDetectedObjectObservation a => IsVNRecognizedObjectObservation a where
  toVNRecognizedObjectObservation :: a -> Id VNRecognizedObjectObservation

instance IsVNRecognizedObjectObservation (Id VNRecognizedObjectObservation) where
  toVNRecognizedObjectObservation = unsafeCastId

instance IsNSObject (Id VNRecognizedObjectObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNRecognizedObjectObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNRecognizedObjectObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNRectangleObservation ----------

-- | VNRectangleObservation
--
-- VNDetectedObjectObservation
--
-- VNRectangleObservation is the result of a rectangle detector
--
-- The VNRectangleObservation has a bounding box that encompasses the rectangle found in the image. The rectangle itself is defined by the four corner point properties. The rectangle can be rotated in or even out of plane. A common use case is to use the CIPerspectiveTransform filter to correct a detected rectangle to its 'flat upright' representation. All coordinates are normalized and the coordinates can be outside the image.
-- 
-- Phantom type for @VNRectangleObservation@.
data VNRectangleObservation

instance IsObjCObject (Id VNRectangleObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRectangleObservation"

class IsVNDetectedObjectObservation a => IsVNRectangleObservation a where
  toVNRectangleObservation :: a -> Id VNRectangleObservation

instance IsVNRectangleObservation (Id VNRectangleObservation) where
  toVNRectangleObservation = unsafeCastId

instance IsNSObject (Id VNRectangleObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNRectangleObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNRectangleObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNImageHomographicAlignmentObservation ----------

-- | VNImageHomographicAlignmentObservation
--
-- VNImageAlignmentObservation
--
-- An observation describing the results of performing a homographic image alignment.
-- 
-- Phantom type for @VNImageHomographicAlignmentObservation@.
data VNImageHomographicAlignmentObservation

instance IsObjCObject (Id VNImageHomographicAlignmentObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageHomographicAlignmentObservation"

class IsVNImageAlignmentObservation a => IsVNImageHomographicAlignmentObservation a where
  toVNImageHomographicAlignmentObservation :: a -> Id VNImageHomographicAlignmentObservation

instance IsVNImageHomographicAlignmentObservation (Id VNImageHomographicAlignmentObservation) where
  toVNImageHomographicAlignmentObservation = unsafeCastId

instance IsNSObject (Id VNImageHomographicAlignmentObservation) where
  toNSObject = unsafeCastId

instance IsVNImageAlignmentObservation (Id VNImageHomographicAlignmentObservation) where
  toVNImageAlignmentObservation = unsafeCastId

instance IsVNObservation (Id VNImageHomographicAlignmentObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNImageTranslationAlignmentObservation ----------

-- | VNImageTranslationAlignmentObservation
--
-- VNImageAlignmentObservation
--
-- An observation describing the results of performing a translational image alignment.
-- 
-- Phantom type for @VNImageTranslationAlignmentObservation@.
data VNImageTranslationAlignmentObservation

instance IsObjCObject (Id VNImageTranslationAlignmentObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageTranslationAlignmentObservation"

class IsVNImageAlignmentObservation a => IsVNImageTranslationAlignmentObservation a where
  toVNImageTranslationAlignmentObservation :: a -> Id VNImageTranslationAlignmentObservation

instance IsVNImageTranslationAlignmentObservation (Id VNImageTranslationAlignmentObservation) where
  toVNImageTranslationAlignmentObservation = unsafeCastId

instance IsNSObject (Id VNImageTranslationAlignmentObservation) where
  toNSObject = unsafeCastId

instance IsVNImageAlignmentObservation (Id VNImageTranslationAlignmentObservation) where
  toVNImageAlignmentObservation = unsafeCastId

instance IsVNObservation (Id VNImageTranslationAlignmentObservation) where
  toVNObservation = unsafeCastId

-- ---------- VNSaliencyImageObservation ----------

-- | VNSaliencyImageObservation
--
-- VNPixelBufferObservation
--
-- VNSaliencyImageObservation provides a grayscale "heat" map of important areas of an image.
--
-- In the revision1, the "heat" map is a OneComponent32Float pixel format CVPixelBuffer.
-- 
-- Phantom type for @VNSaliencyImageObservation@.
data VNSaliencyImageObservation

instance IsObjCObject (Id VNSaliencyImageObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNSaliencyImageObservation"

class IsVNPixelBufferObservation a => IsVNSaliencyImageObservation a where
  toVNSaliencyImageObservation :: a -> Id VNSaliencyImageObservation

instance IsVNSaliencyImageObservation (Id VNSaliencyImageObservation) where
  toVNSaliencyImageObservation = unsafeCastId

instance IsNSObject (Id VNSaliencyImageObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNSaliencyImageObservation) where
  toVNObservation = unsafeCastId

instance IsVNPixelBufferObservation (Id VNSaliencyImageObservation) where
  toVNPixelBufferObservation = unsafeCastId

-- ---------- VNHumanBodyPose3DObservation ----------

-- | Phantom type for @VNHumanBodyPose3DObservation@.
data VNHumanBodyPose3DObservation

instance IsObjCObject (Id VNHumanBodyPose3DObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHumanBodyPose3DObservation"

class IsVNRecognizedPoints3DObservation a => IsVNHumanBodyPose3DObservation a where
  toVNHumanBodyPose3DObservation :: a -> Id VNHumanBodyPose3DObservation

instance IsVNHumanBodyPose3DObservation (Id VNHumanBodyPose3DObservation) where
  toVNHumanBodyPose3DObservation = unsafeCastId

instance IsNSObject (Id VNHumanBodyPose3DObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNHumanBodyPose3DObservation) where
  toVNObservation = unsafeCastId

instance IsVNRecognizedPoints3DObservation (Id VNHumanBodyPose3DObservation) where
  toVNRecognizedPoints3DObservation = unsafeCastId

-- ---------- VNAnimalBodyPoseObservation ----------

-- | Phantom type for @VNAnimalBodyPoseObservation@.
data VNAnimalBodyPoseObservation

instance IsObjCObject (Id VNAnimalBodyPoseObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNAnimalBodyPoseObservation"

class IsVNRecognizedPointsObservation a => IsVNAnimalBodyPoseObservation a where
  toVNAnimalBodyPoseObservation :: a -> Id VNAnimalBodyPoseObservation

instance IsVNAnimalBodyPoseObservation (Id VNAnimalBodyPoseObservation) where
  toVNAnimalBodyPoseObservation = unsafeCastId

instance IsNSObject (Id VNAnimalBodyPoseObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNAnimalBodyPoseObservation) where
  toVNObservation = unsafeCastId

instance IsVNRecognizedPointsObservation (Id VNAnimalBodyPoseObservation) where
  toVNRecognizedPointsObservation = unsafeCastId

-- ---------- VNHumanBodyPoseObservation ----------

-- | Phantom type for @VNHumanBodyPoseObservation@.
data VNHumanBodyPoseObservation

instance IsObjCObject (Id VNHumanBodyPoseObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHumanBodyPoseObservation"

class IsVNRecognizedPointsObservation a => IsVNHumanBodyPoseObservation a where
  toVNHumanBodyPoseObservation :: a -> Id VNHumanBodyPoseObservation

instance IsVNHumanBodyPoseObservation (Id VNHumanBodyPoseObservation) where
  toVNHumanBodyPoseObservation = unsafeCastId

instance IsNSObject (Id VNHumanBodyPoseObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNHumanBodyPoseObservation) where
  toVNObservation = unsafeCastId

instance IsVNRecognizedPointsObservation (Id VNHumanBodyPoseObservation) where
  toVNRecognizedPointsObservation = unsafeCastId

-- ---------- VNHumanHandPoseObservation ----------

-- | Phantom type for @VNHumanHandPoseObservation@.
data VNHumanHandPoseObservation

instance IsObjCObject (Id VNHumanHandPoseObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHumanHandPoseObservation"

class IsVNRecognizedPointsObservation a => IsVNHumanHandPoseObservation a where
  toVNHumanHandPoseObservation :: a -> Id VNHumanHandPoseObservation

instance IsVNHumanHandPoseObservation (Id VNHumanHandPoseObservation) where
  toVNHumanHandPoseObservation = unsafeCastId

instance IsNSObject (Id VNHumanHandPoseObservation) where
  toNSObject = unsafeCastId

instance IsVNObservation (Id VNHumanHandPoseObservation) where
  toVNObservation = unsafeCastId

instance IsVNRecognizedPointsObservation (Id VNHumanHandPoseObservation) where
  toVNRecognizedPointsObservation = unsafeCastId

-- ---------- VNRecognizedPoint ----------

-- | VNRecognizedPoint
--
-- An extension of VNDetectedPoint that associates an identifier to the point.
--
-- It should be noted that VNRecognizedPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that recognize labeled points of interest.
-- 
-- Phantom type for @VNRecognizedPoint@.
data VNRecognizedPoint

instance IsObjCObject (Id VNRecognizedPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedPoint"

class IsVNDetectedPoint a => IsVNRecognizedPoint a where
  toVNRecognizedPoint :: a -> Id VNRecognizedPoint

instance IsVNRecognizedPoint (Id VNRecognizedPoint) where
  toVNRecognizedPoint = unsafeCastId

instance IsNSObject (Id VNRecognizedPoint) where
  toNSObject = unsafeCastId

instance IsVNDetectedPoint (Id VNRecognizedPoint) where
  toVNDetectedPoint = unsafeCastId

instance IsVNPoint (Id VNRecognizedPoint) where
  toVNPoint = unsafeCastId

-- ---------- VNHumanBodyRecognizedPoint3D ----------

-- | Phantom type for @VNHumanBodyRecognizedPoint3D@.
data VNHumanBodyRecognizedPoint3D

instance IsObjCObject (Id VNHumanBodyRecognizedPoint3D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHumanBodyRecognizedPoint3D"

class IsVNRecognizedPoint3D a => IsVNHumanBodyRecognizedPoint3D a where
  toVNHumanBodyRecognizedPoint3D :: a -> Id VNHumanBodyRecognizedPoint3D

instance IsVNHumanBodyRecognizedPoint3D (Id VNHumanBodyRecognizedPoint3D) where
  toVNHumanBodyRecognizedPoint3D = unsafeCastId

instance IsNSObject (Id VNHumanBodyRecognizedPoint3D) where
  toNSObject = unsafeCastId

instance IsVNPoint3D (Id VNHumanBodyRecognizedPoint3D) where
  toVNPoint3D = unsafeCastId

instance IsVNRecognizedPoint3D (Id VNHumanBodyRecognizedPoint3D) where
  toVNRecognizedPoint3D = unsafeCastId

-- ---------- VNCalculateImageAestheticsScoresRequest ----------

-- | Analyzes an image for aesthetically pleasing attributes and returns a VNImageAestheticsScoresObservation.             This observation calculates an overall aeshetically pleasing score for the image and checks for utility images.
-- 
-- Phantom type for @VNCalculateImageAestheticsScoresRequest@.
data VNCalculateImageAestheticsScoresRequest

instance IsObjCObject (Id VNCalculateImageAestheticsScoresRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNCalculateImageAestheticsScoresRequest"

class IsVNImageBasedRequest a => IsVNCalculateImageAestheticsScoresRequest a where
  toVNCalculateImageAestheticsScoresRequest :: a -> Id VNCalculateImageAestheticsScoresRequest

instance IsVNCalculateImageAestheticsScoresRequest (Id VNCalculateImageAestheticsScoresRequest) where
  toVNCalculateImageAestheticsScoresRequest = unsafeCastId

instance IsNSObject (Id VNCalculateImageAestheticsScoresRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNCalculateImageAestheticsScoresRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNCalculateImageAestheticsScoresRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNClassifyImageRequest ----------

-- | A request for classifying an image.
--
-- This request will produce a collection of VNClassificationObservation objects which describe an image.
-- 
-- Phantom type for @VNClassifyImageRequest@.
data VNClassifyImageRequest

instance IsObjCObject (Id VNClassifyImageRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNClassifyImageRequest"

class IsVNImageBasedRequest a => IsVNClassifyImageRequest a where
  toVNClassifyImageRequest :: a -> Id VNClassifyImageRequest

instance IsVNClassifyImageRequest (Id VNClassifyImageRequest) where
  toVNClassifyImageRequest = unsafeCastId

instance IsNSObject (Id VNClassifyImageRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNClassifyImageRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNClassifyImageRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNCoreMLRequest ----------

-- | The VNCoreMLRequest uses a VNCoreMLModel, that is based on a CoreML MLModel object, to run predictions with that model. Depending on the model the returned             observation is either a VNClassificationObservation for classifier models, VNPixelBufferObservations for image-to-image models, VNRecognizedObjectObservation for object recognition models or VNCoreMLFeatureValueObservation for everything else. All -[VNObservation confidence] values are forwarded from relevant models as is, and do not conform to a common [0, 1] range rule
-- 
-- Phantom type for @VNCoreMLRequest@.
data VNCoreMLRequest

instance IsObjCObject (Id VNCoreMLRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNCoreMLRequest"

class IsVNImageBasedRequest a => IsVNCoreMLRequest a where
  toVNCoreMLRequest :: a -> Id VNCoreMLRequest

instance IsVNCoreMLRequest (Id VNCoreMLRequest) where
  toVNCoreMLRequest = unsafeCastId

instance IsNSObject (Id VNCoreMLRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNCoreMLRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNCoreMLRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectAnimalBodyPoseRequest ----------

-- | Detects specific landmark points on animal bodies.
--
-- This request will produce a collection of VNAnimalBodyPoseObservation objects which describe the pose of each detected animal body.
-- 
-- Phantom type for @VNDetectAnimalBodyPoseRequest@.
data VNDetectAnimalBodyPoseRequest

instance IsObjCObject (Id VNDetectAnimalBodyPoseRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectAnimalBodyPoseRequest"

class IsVNImageBasedRequest a => IsVNDetectAnimalBodyPoseRequest a where
  toVNDetectAnimalBodyPoseRequest :: a -> Id VNDetectAnimalBodyPoseRequest

instance IsVNDetectAnimalBodyPoseRequest (Id VNDetectAnimalBodyPoseRequest) where
  toVNDetectAnimalBodyPoseRequest = unsafeCastId

instance IsNSObject (Id VNDetectAnimalBodyPoseRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectAnimalBodyPoseRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectAnimalBodyPoseRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectBarcodesRequest ----------

-- | A request that detects barcodes in an image.
--
-- This request will return zero or more VNBarcodeObservation objects objects which describe the barcodes detected in an image.
-- 
-- Phantom type for @VNDetectBarcodesRequest@.
data VNDetectBarcodesRequest

instance IsObjCObject (Id VNDetectBarcodesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectBarcodesRequest"

class IsVNImageBasedRequest a => IsVNDetectBarcodesRequest a where
  toVNDetectBarcodesRequest :: a -> Id VNDetectBarcodesRequest

instance IsVNDetectBarcodesRequest (Id VNDetectBarcodesRequest) where
  toVNDetectBarcodesRequest = unsafeCastId

instance IsNSObject (Id VNDetectBarcodesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectBarcodesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectBarcodesRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectContoursRequest ----------

-- | A request that will detect the contours for the edges in an image.
--
-- This request will produce a VNContoursObservation which describes the contours.
-- 
-- Phantom type for @VNDetectContoursRequest@.
data VNDetectContoursRequest

instance IsObjCObject (Id VNDetectContoursRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectContoursRequest"

class IsVNImageBasedRequest a => IsVNDetectContoursRequest a where
  toVNDetectContoursRequest :: a -> Id VNDetectContoursRequest

instance IsVNDetectContoursRequest (Id VNDetectContoursRequest) where
  toVNDetectContoursRequest = unsafeCastId

instance IsNSObject (Id VNDetectContoursRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectContoursRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectContoursRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectDocumentSegmentationRequest ----------

-- | Performs document detection and segmentation in an image.
-- 
-- Phantom type for @VNDetectDocumentSegmentationRequest@.
data VNDetectDocumentSegmentationRequest

instance IsObjCObject (Id VNDetectDocumentSegmentationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectDocumentSegmentationRequest"

class IsVNImageBasedRequest a => IsVNDetectDocumentSegmentationRequest a where
  toVNDetectDocumentSegmentationRequest :: a -> Id VNDetectDocumentSegmentationRequest

instance IsVNDetectDocumentSegmentationRequest (Id VNDetectDocumentSegmentationRequest) where
  toVNDetectDocumentSegmentationRequest = unsafeCastId

instance IsNSObject (Id VNDetectDocumentSegmentationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectDocumentSegmentationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectDocumentSegmentationRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectFaceCaptureQualityRequest ----------

-- | A request that will produce or update a VNFaceObservation's property faceCaptureQuality with a float value. faceCaptureQuality is a float (wrapped by a NSNumber) that represents the capture quality of a given face in a photo. The float will be a value between 0 and 1, with 1 being the highest face capture quality and 0 being the lowest. If the request fails or the face observation has never been processed, the property faceCaptureQuality will be nil.
--
-- This request will generate VNFaceObservation objects with the face quality variable populated with information .
-- 
-- Phantom type for @VNDetectFaceCaptureQualityRequest@.
data VNDetectFaceCaptureQualityRequest

instance IsObjCObject (Id VNDetectFaceCaptureQualityRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectFaceCaptureQualityRequest"

class IsVNImageBasedRequest a => IsVNDetectFaceCaptureQualityRequest a where
  toVNDetectFaceCaptureQualityRequest :: a -> Id VNDetectFaceCaptureQualityRequest

instance IsVNDetectFaceCaptureQualityRequest (Id VNDetectFaceCaptureQualityRequest) where
  toVNDetectFaceCaptureQualityRequest = unsafeCastId

instance IsNSObject (Id VNDetectFaceCaptureQualityRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectFaceCaptureQualityRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectFaceCaptureQualityRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectFaceLandmarksRequest ----------

-- | A request that will produce face landmark information.
--
-- This request will generate VNFaceObservation objects with the landmarks property populated with information describing face landmarks. If VNFaceObservations are provided via the VNFaceObservationAccepting protocol without the landmarks property populated, new observations will be created as copies of the input VNFaceObservations with the landmarks property populated. If the landmarks property has already been populated, the original VNFaceObservations will be returned. If no VNFaceObservations are provided, face detection will be run first.
-- 
-- Phantom type for @VNDetectFaceLandmarksRequest@.
data VNDetectFaceLandmarksRequest

instance IsObjCObject (Id VNDetectFaceLandmarksRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectFaceLandmarksRequest"

class IsVNImageBasedRequest a => IsVNDetectFaceLandmarksRequest a where
  toVNDetectFaceLandmarksRequest :: a -> Id VNDetectFaceLandmarksRequest

instance IsVNDetectFaceLandmarksRequest (Id VNDetectFaceLandmarksRequest) where
  toVNDetectFaceLandmarksRequest = unsafeCastId

instance IsNSObject (Id VNDetectFaceLandmarksRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectFaceLandmarksRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectFaceLandmarksRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectFaceRectanglesRequest ----------

-- | A request that will detect faces in an image.
--
-- This request will generate VNFaceObservation objects with a defined boundingBox.
-- 
-- Phantom type for @VNDetectFaceRectanglesRequest@.
data VNDetectFaceRectanglesRequest

instance IsObjCObject (Id VNDetectFaceRectanglesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectFaceRectanglesRequest"

class IsVNImageBasedRequest a => IsVNDetectFaceRectanglesRequest a where
  toVNDetectFaceRectanglesRequest :: a -> Id VNDetectFaceRectanglesRequest

instance IsVNDetectFaceRectanglesRequest (Id VNDetectFaceRectanglesRequest) where
  toVNDetectFaceRectanglesRequest = unsafeCastId

instance IsNSObject (Id VNDetectFaceRectanglesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectFaceRectanglesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectFaceRectanglesRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectHorizonRequest ----------

-- | Determine what the horizon tilt of an image is.
--
-- If the horizon tilt is detected in an image, the request will provide a VNHorizonObservation in the results which describe how to transform the image so that the horizon line becomes level.
-- 
-- Phantom type for @VNDetectHorizonRequest@.
data VNDetectHorizonRequest

instance IsObjCObject (Id VNDetectHorizonRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectHorizonRequest"

class IsVNImageBasedRequest a => IsVNDetectHorizonRequest a where
  toVNDetectHorizonRequest :: a -> Id VNDetectHorizonRequest

instance IsVNDetectHorizonRequest (Id VNDetectHorizonRequest) where
  toVNDetectHorizonRequest = unsafeCastId

instance IsNSObject (Id VNDetectHorizonRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectHorizonRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectHorizonRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectHumanBodyPoseRequest ----------

-- | Detects specific landmark points on human bodies.
--
-- This request will produce a collection of VNHumanBodyPoseObservation objects which describe the pose of each detected human body.
-- 
-- Phantom type for @VNDetectHumanBodyPoseRequest@.
data VNDetectHumanBodyPoseRequest

instance IsObjCObject (Id VNDetectHumanBodyPoseRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectHumanBodyPoseRequest"

class IsVNImageBasedRequest a => IsVNDetectHumanBodyPoseRequest a where
  toVNDetectHumanBodyPoseRequest :: a -> Id VNDetectHumanBodyPoseRequest

instance IsVNDetectHumanBodyPoseRequest (Id VNDetectHumanBodyPoseRequest) where
  toVNDetectHumanBodyPoseRequest = unsafeCastId

instance IsNSObject (Id VNDetectHumanBodyPoseRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectHumanBodyPoseRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectHumanBodyPoseRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectHumanHandPoseRequest ----------

-- | Detects specific landmark points on human hands.
--
-- This request will produce a collection of VNRecognizedPointsObservation objects which describe the pose of each detected human hand.
-- 
-- Phantom type for @VNDetectHumanHandPoseRequest@.
data VNDetectHumanHandPoseRequest

instance IsObjCObject (Id VNDetectHumanHandPoseRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectHumanHandPoseRequest"

class IsVNImageBasedRequest a => IsVNDetectHumanHandPoseRequest a where
  toVNDetectHumanHandPoseRequest :: a -> Id VNDetectHumanHandPoseRequest

instance IsVNDetectHumanHandPoseRequest (Id VNDetectHumanHandPoseRequest) where
  toVNDetectHumanHandPoseRequest = unsafeCastId

instance IsNSObject (Id VNDetectHumanHandPoseRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectHumanHandPoseRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectHumanHandPoseRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectHumanRectanglesRequest ----------

-- | A request that will detect human Torsos in an image.
--
-- This request will generate VNHumanObservation objects with defined boundingBox and confidence score.
-- 
-- Phantom type for @VNDetectHumanRectanglesRequest@.
data VNDetectHumanRectanglesRequest

instance IsObjCObject (Id VNDetectHumanRectanglesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectHumanRectanglesRequest"

class IsVNImageBasedRequest a => IsVNDetectHumanRectanglesRequest a where
  toVNDetectHumanRectanglesRequest :: a -> Id VNDetectHumanRectanglesRequest

instance IsVNDetectHumanRectanglesRequest (Id VNDetectHumanRectanglesRequest) where
  toVNDetectHumanRectanglesRequest = unsafeCastId

instance IsNSObject (Id VNDetectHumanRectanglesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectHumanRectanglesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectHumanRectanglesRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectRectanglesRequest ----------

-- | A request that will detect rectangles in an image.
--
-- This request will generate VNRectangleObservation objects describing the location of rectangles detected in an image.
-- 
-- Phantom type for @VNDetectRectanglesRequest@.
data VNDetectRectanglesRequest

instance IsObjCObject (Id VNDetectRectanglesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectRectanglesRequest"

class IsVNImageBasedRequest a => IsVNDetectRectanglesRequest a where
  toVNDetectRectanglesRequest :: a -> Id VNDetectRectanglesRequest

instance IsVNDetectRectanglesRequest (Id VNDetectRectanglesRequest) where
  toVNDetectRectanglesRequest = unsafeCastId

instance IsNSObject (Id VNDetectRectanglesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectRectanglesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectRectanglesRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNDetectTextRectanglesRequest ----------

-- | A request that will detect regions of text in an image.
--
-- This request will generate VNTextObservation objects describing the locations of text detected in an image.
-- 
-- Phantom type for @VNDetectTextRectanglesRequest@.
data VNDetectTextRectanglesRequest

instance IsObjCObject (Id VNDetectTextRectanglesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectTextRectanglesRequest"

class IsVNImageBasedRequest a => IsVNDetectTextRectanglesRequest a where
  toVNDetectTextRectanglesRequest :: a -> Id VNDetectTextRectanglesRequest

instance IsVNDetectTextRectanglesRequest (Id VNDetectTextRectanglesRequest) where
  toVNDetectTextRectanglesRequest = unsafeCastId

instance IsNSObject (Id VNDetectTextRectanglesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectTextRectanglesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectTextRectanglesRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNGenerateAttentionBasedSaliencyImageRequest ----------

-- | Generates an image that identifies which part(s) of a given image is most interesting (i.e. something that a human is likely to look at - hence attention based).			The resulting observation, VNSaliencyImageObservation, encodes this data as a heat map which can be used to highlight regions of interest.
-- 
-- Phantom type for @VNGenerateAttentionBasedSaliencyImageRequest@.
data VNGenerateAttentionBasedSaliencyImageRequest

instance IsObjCObject (Id VNGenerateAttentionBasedSaliencyImageRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGenerateAttentionBasedSaliencyImageRequest"

class IsVNImageBasedRequest a => IsVNGenerateAttentionBasedSaliencyImageRequest a where
  toVNGenerateAttentionBasedSaliencyImageRequest :: a -> Id VNGenerateAttentionBasedSaliencyImageRequest

instance IsVNGenerateAttentionBasedSaliencyImageRequest (Id VNGenerateAttentionBasedSaliencyImageRequest) where
  toVNGenerateAttentionBasedSaliencyImageRequest = unsafeCastId

instance IsNSObject (Id VNGenerateAttentionBasedSaliencyImageRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGenerateAttentionBasedSaliencyImageRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGenerateAttentionBasedSaliencyImageRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNGenerateForegroundInstanceMaskRequest ----------

-- | A request that generates an instance mask of salient objects that can be separated from the background.
-- 
-- Phantom type for @VNGenerateForegroundInstanceMaskRequest@.
data VNGenerateForegroundInstanceMaskRequest

instance IsObjCObject (Id VNGenerateForegroundInstanceMaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGenerateForegroundInstanceMaskRequest"

class IsVNImageBasedRequest a => IsVNGenerateForegroundInstanceMaskRequest a where
  toVNGenerateForegroundInstanceMaskRequest :: a -> Id VNGenerateForegroundInstanceMaskRequest

instance IsVNGenerateForegroundInstanceMaskRequest (Id VNGenerateForegroundInstanceMaskRequest) where
  toVNGenerateForegroundInstanceMaskRequest = unsafeCastId

instance IsNSObject (Id VNGenerateForegroundInstanceMaskRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGenerateForegroundInstanceMaskRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGenerateForegroundInstanceMaskRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNGenerateImageFeaturePrintRequest ----------

-- | A request for generating a feature print of an image.
--
-- This request will produce a @VNFeaturePrintObservation@ object.
-- 
-- Phantom type for @VNGenerateImageFeaturePrintRequest@.
data VNGenerateImageFeaturePrintRequest

instance IsObjCObject (Id VNGenerateImageFeaturePrintRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGenerateImageFeaturePrintRequest"

class IsVNImageBasedRequest a => IsVNGenerateImageFeaturePrintRequest a where
  toVNGenerateImageFeaturePrintRequest :: a -> Id VNGenerateImageFeaturePrintRequest

instance IsVNGenerateImageFeaturePrintRequest (Id VNGenerateImageFeaturePrintRequest) where
  toVNGenerateImageFeaturePrintRequest = unsafeCastId

instance IsNSObject (Id VNGenerateImageFeaturePrintRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGenerateImageFeaturePrintRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGenerateImageFeaturePrintRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNGenerateObjectnessBasedSaliencyImageRequest ----------

-- | Generates an image that identifies which part(s) of a given image are most likely to be objects (i.e. something that a human is likely to see as an object). The resulting observation, VNSaliencyImageObservation, encodes this data as a heat map which can be used highlight regions of interest.
-- 
-- Phantom type for @VNGenerateObjectnessBasedSaliencyImageRequest@.
data VNGenerateObjectnessBasedSaliencyImageRequest

instance IsObjCObject (Id VNGenerateObjectnessBasedSaliencyImageRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGenerateObjectnessBasedSaliencyImageRequest"

class IsVNImageBasedRequest a => IsVNGenerateObjectnessBasedSaliencyImageRequest a where
  toVNGenerateObjectnessBasedSaliencyImageRequest :: a -> Id VNGenerateObjectnessBasedSaliencyImageRequest

instance IsVNGenerateObjectnessBasedSaliencyImageRequest (Id VNGenerateObjectnessBasedSaliencyImageRequest) where
  toVNGenerateObjectnessBasedSaliencyImageRequest = unsafeCastId

instance IsNSObject (Id VNGenerateObjectnessBasedSaliencyImageRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGenerateObjectnessBasedSaliencyImageRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGenerateObjectnessBasedSaliencyImageRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNGeneratePersonInstanceMaskRequest ----------

-- | A request that generates an instance mask of individual people found in the image.
-- 
-- Phantom type for @VNGeneratePersonInstanceMaskRequest@.
data VNGeneratePersonInstanceMaskRequest

instance IsObjCObject (Id VNGeneratePersonInstanceMaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGeneratePersonInstanceMaskRequest"

class IsVNImageBasedRequest a => IsVNGeneratePersonInstanceMaskRequest a where
  toVNGeneratePersonInstanceMaskRequest :: a -> Id VNGeneratePersonInstanceMaskRequest

instance IsVNGeneratePersonInstanceMaskRequest (Id VNGeneratePersonInstanceMaskRequest) where
  toVNGeneratePersonInstanceMaskRequest = unsafeCastId

instance IsNSObject (Id VNGeneratePersonInstanceMaskRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGeneratePersonInstanceMaskRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGeneratePersonInstanceMaskRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNRecognizeAnimalsRequest ----------

-- | A request that will recognize various animals in an image. The list of animals supported by the recognition algorithm can be queried by  -supportedIdentifiersAndReturnError:
--
-- This request will generate VNRecognizedObjectObservation objects with a defined boundingBox, label and confidence level.
-- 
-- Phantom type for @VNRecognizeAnimalsRequest@.
data VNRecognizeAnimalsRequest

instance IsObjCObject (Id VNRecognizeAnimalsRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizeAnimalsRequest"

class IsVNImageBasedRequest a => IsVNRecognizeAnimalsRequest a where
  toVNRecognizeAnimalsRequest :: a -> Id VNRecognizeAnimalsRequest

instance IsVNRecognizeAnimalsRequest (Id VNRecognizeAnimalsRequest) where
  toVNRecognizeAnimalsRequest = unsafeCastId

instance IsNSObject (Id VNRecognizeAnimalsRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNRecognizeAnimalsRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNRecognizeAnimalsRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNRecognizeTextRequest ----------

-- | A request that will detect regions of text and recognize the containing text in an image.
--
-- This request will generate VNRecognizedTextObservation objects describing the locations of text and the actual text recognized.
-- 
-- Phantom type for @VNRecognizeTextRequest@.
data VNRecognizeTextRequest

instance IsObjCObject (Id VNRecognizeTextRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizeTextRequest"

class IsVNImageBasedRequest a => IsVNRecognizeTextRequest a where
  toVNRecognizeTextRequest :: a -> Id VNRecognizeTextRequest

instance IsVNRecognizeTextRequest (Id VNRecognizeTextRequest) where
  toVNRecognizeTextRequest = unsafeCastId

instance IsNSObject (Id VNRecognizeTextRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNRecognizeTextRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNRecognizeTextRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNStatefulRequest ----------

-- | A request that builds evidence over time by being reused on multiple images.
--
-- The request requires the use of CMSampleBuffers with timestamps as input; otherwise, a VNErrorTimeStampNotFound error will be returned. VNStatefulRequest is used as a base class of other requests, so no objects of this class should be created directly.
-- 
-- Phantom type for @VNStatefulRequest@.
data VNStatefulRequest

instance IsObjCObject (Id VNStatefulRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNStatefulRequest"

class IsVNImageBasedRequest a => IsVNStatefulRequest a where
  toVNStatefulRequest :: a -> Id VNStatefulRequest

instance IsVNStatefulRequest (Id VNStatefulRequest) where
  toVNStatefulRequest = unsafeCastId

instance IsNSObject (Id VNStatefulRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNStatefulRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNStatefulRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNTargetedImageRequest ----------

-- | A request that requires an initial image as a starting point for its work.
-- 
-- Phantom type for @VNTargetedImageRequest@.
data VNTargetedImageRequest

instance IsObjCObject (Id VNTargetedImageRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTargetedImageRequest"

class IsVNImageBasedRequest a => IsVNTargetedImageRequest a where
  toVNTargetedImageRequest :: a -> Id VNTargetedImageRequest

instance IsVNTargetedImageRequest (Id VNTargetedImageRequest) where
  toVNTargetedImageRequest = unsafeCastId

instance IsNSObject (Id VNTargetedImageRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTargetedImageRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTargetedImageRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNTrackingRequest ----------

-- | A base class for all tracking requests.
--
-- Since this class is not meant to be directly instantiated, no initializers are available.
-- 
-- Phantom type for @VNTrackingRequest@.
data VNTrackingRequest

instance IsObjCObject (Id VNTrackingRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackingRequest"

class IsVNImageBasedRequest a => IsVNTrackingRequest a where
  toVNTrackingRequest :: a -> Id VNTrackingRequest

instance IsVNTrackingRequest (Id VNTrackingRequest) where
  toVNTrackingRequest = unsafeCastId

instance IsNSObject (Id VNTrackingRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackingRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackingRequest) where
  toVNRequest = unsafeCastId

-- ---------- VNBarcodeObservation ----------

-- | VNBarcodeObservation
--
-- VNRectangleObservation
--
-- VNBarcodeObservation Describes an area containing a barcode detected by the VNRequestNameDetectBarcodes request.
-- 
-- Phantom type for @VNBarcodeObservation@.
data VNBarcodeObservation

instance IsObjCObject (Id VNBarcodeObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNBarcodeObservation"

class IsVNRectangleObservation a => IsVNBarcodeObservation a where
  toVNBarcodeObservation :: a -> Id VNBarcodeObservation

instance IsVNBarcodeObservation (Id VNBarcodeObservation) where
  toVNBarcodeObservation = unsafeCastId

instance IsNSObject (Id VNBarcodeObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNBarcodeObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNBarcodeObservation) where
  toVNObservation = unsafeCastId

instance IsVNRectangleObservation (Id VNBarcodeObservation) where
  toVNRectangleObservation = unsafeCastId

-- ---------- VNRecognizedTextObservation ----------

-- | VNRecognizedTextObservation
--
-- VNRectangleObservation
--
-- VNRecognizedTextObservation Describes a text area detected and recognized by the VNRecognizeTextRequest request.
-- 
-- Phantom type for @VNRecognizedTextObservation@.
data VNRecognizedTextObservation

instance IsObjCObject (Id VNRecognizedTextObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNRecognizedTextObservation"

class IsVNRectangleObservation a => IsVNRecognizedTextObservation a where
  toVNRecognizedTextObservation :: a -> Id VNRecognizedTextObservation

instance IsVNRecognizedTextObservation (Id VNRecognizedTextObservation) where
  toVNRecognizedTextObservation = unsafeCastId

instance IsNSObject (Id VNRecognizedTextObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNRecognizedTextObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNRecognizedTextObservation) where
  toVNObservation = unsafeCastId

instance IsVNRectangleObservation (Id VNRecognizedTextObservation) where
  toVNRectangleObservation = unsafeCastId

-- ---------- VNTextObservation ----------

-- | VNTextObservation
--
-- VNRectangleObservation
--
-- VNTextObservation Describes a text area detected by the VNRequestNameDetectTextRectangles request.
-- 
-- Phantom type for @VNTextObservation@.
data VNTextObservation

instance IsObjCObject (Id VNTextObservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTextObservation"

class IsVNRectangleObservation a => IsVNTextObservation a where
  toVNTextObservation :: a -> Id VNTextObservation

instance IsVNTextObservation (Id VNTextObservation) where
  toVNTextObservation = unsafeCastId

instance IsNSObject (Id VNTextObservation) where
  toNSObject = unsafeCastId

instance IsVNDetectedObjectObservation (Id VNTextObservation) where
  toVNDetectedObjectObservation = unsafeCastId

instance IsVNObservation (Id VNTextObservation) where
  toVNObservation = unsafeCastId

instance IsVNRectangleObservation (Id VNTextObservation) where
  toVNRectangleObservation = unsafeCastId

-- ---------- VNDetectHumanBodyPose3DRequest ----------

-- | A request that detects specific landmark points on human bodies in 3D space relative to the camera.        When possible,@AVDepthData@ depth information is used to produce more accurate results, but the request does not require it to run.
--
-- This request generates a collection of VNHumanBodyPose3DObservation objects which describe the position of each detected body
-- 
-- Phantom type for @VNDetectHumanBodyPose3DRequest@.
data VNDetectHumanBodyPose3DRequest

instance IsObjCObject (Id VNDetectHumanBodyPose3DRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectHumanBodyPose3DRequest"

class IsVNStatefulRequest a => IsVNDetectHumanBodyPose3DRequest a where
  toVNDetectHumanBodyPose3DRequest :: a -> Id VNDetectHumanBodyPose3DRequest

instance IsVNDetectHumanBodyPose3DRequest (Id VNDetectHumanBodyPose3DRequest) where
  toVNDetectHumanBodyPose3DRequest = unsafeCastId

instance IsNSObject (Id VNDetectHumanBodyPose3DRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectHumanBodyPose3DRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectHumanBodyPose3DRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNDetectHumanBodyPose3DRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNDetectTrajectoriesRequest ----------

-- | A request that detects trajectories of shapes (even small ones) that follow a parabolic path in a sequence of images.
--
-- This request detects objects moving and (once their path follows the constraint of a parabola), a VNTrajectoryObservation will be returned with the detected points and the equation describing the parabola.
-- 
-- Phantom type for @VNDetectTrajectoriesRequest@.
data VNDetectTrajectoriesRequest

instance IsObjCObject (Id VNDetectTrajectoriesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNDetectTrajectoriesRequest"

class IsVNStatefulRequest a => IsVNDetectTrajectoriesRequest a where
  toVNDetectTrajectoriesRequest :: a -> Id VNDetectTrajectoriesRequest

instance IsVNDetectTrajectoriesRequest (Id VNDetectTrajectoriesRequest) where
  toVNDetectTrajectoriesRequest = unsafeCastId

instance IsNSObject (Id VNDetectTrajectoriesRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNDetectTrajectoriesRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNDetectTrajectoriesRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNDetectTrajectoriesRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNGeneratePersonSegmentationRequest ----------

-- | Performs person segmentation on an image generating a mask.
-- 
-- Phantom type for @VNGeneratePersonSegmentationRequest@.
data VNGeneratePersonSegmentationRequest

instance IsObjCObject (Id VNGeneratePersonSegmentationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGeneratePersonSegmentationRequest"

class IsVNStatefulRequest a => IsVNGeneratePersonSegmentationRequest a where
  toVNGeneratePersonSegmentationRequest :: a -> Id VNGeneratePersonSegmentationRequest

instance IsVNGeneratePersonSegmentationRequest (Id VNGeneratePersonSegmentationRequest) where
  toVNGeneratePersonSegmentationRequest = unsafeCastId

instance IsNSObject (Id VNGeneratePersonSegmentationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGeneratePersonSegmentationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGeneratePersonSegmentationRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNGeneratePersonSegmentationRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNTrackHomographicImageRegistrationRequest ----------

-- | An image registration request that will produce a homographic transformation that can morph one image to another.
--
-- Because this is a stateful request, it must be performed on at least two images in order to produce an observation.
-- 
-- Phantom type for @VNTrackHomographicImageRegistrationRequest@.
data VNTrackHomographicImageRegistrationRequest

instance IsObjCObject (Id VNTrackHomographicImageRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackHomographicImageRegistrationRequest"

class IsVNStatefulRequest a => IsVNTrackHomographicImageRegistrationRequest a where
  toVNTrackHomographicImageRegistrationRequest :: a -> Id VNTrackHomographicImageRegistrationRequest

instance IsVNTrackHomographicImageRegistrationRequest (Id VNTrackHomographicImageRegistrationRequest) where
  toVNTrackHomographicImageRegistrationRequest = unsafeCastId

instance IsNSObject (Id VNTrackHomographicImageRegistrationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackHomographicImageRegistrationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackHomographicImageRegistrationRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNTrackHomographicImageRegistrationRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNTrackOpticalFlowRequest ----------

-- | @VNTrackOpticalFlowRequest@ will determine directional change vectors for each pixel from a previous to current image, reporting this result with a single @VNPixelBufferObservation@.
--
-- Because this request works at the pixel level, both images must have the same dimensions in order for the request to be successfully performed.				Setting a region of interest will isolate where the change determination is performed; however, the resultant observation will still be reported				with a full resolution `VNPixelBufferObservation.
--
-- Being a stateful request, at least two images must me processed in order to produce an observation.
--
-- Optical flow requests are very resource intensive, so it is recommended that only one request at a time be created and that the handler                where the request was issued be released immediately after generating optical flows.
--
-- Example usage:
--
-- - (nullable VNPixelBufferObservation*) opticalFlowFromImage:(CVPixelBufferRef)fromImage toImage:(CVPixelBuffer)toImage error:(NSError**)error					{						VNTrackOpticalFlowRequest* request = [[VNTrackOpticalFlowRequest alloc] init];
--
-- VNImageRequestHandler* imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:fromImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:toImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- return [[request results] firstObject];					}
-- 
-- Phantom type for @VNTrackOpticalFlowRequest@.
data VNTrackOpticalFlowRequest

instance IsObjCObject (Id VNTrackOpticalFlowRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackOpticalFlowRequest"

class IsVNStatefulRequest a => IsVNTrackOpticalFlowRequest a where
  toVNTrackOpticalFlowRequest :: a -> Id VNTrackOpticalFlowRequest

instance IsVNTrackOpticalFlowRequest (Id VNTrackOpticalFlowRequest) where
  toVNTrackOpticalFlowRequest = unsafeCastId

instance IsNSObject (Id VNTrackOpticalFlowRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackOpticalFlowRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackOpticalFlowRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNTrackOpticalFlowRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNTrackTranslationalImageRegistrationRequest ----------

-- | An image registration request that will produce a translational transformation which will morph one image to another.
--
-- Because this is a stateful request, it must be performed on at least two images in order to produce an observation.
-- 
-- Phantom type for @VNTrackTranslationalImageRegistrationRequest@.
data VNTrackTranslationalImageRegistrationRequest

instance IsObjCObject (Id VNTrackTranslationalImageRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackTranslationalImageRegistrationRequest"

class IsVNStatefulRequest a => IsVNTrackTranslationalImageRegistrationRequest a where
  toVNTrackTranslationalImageRegistrationRequest :: a -> Id VNTrackTranslationalImageRegistrationRequest

instance IsVNTrackTranslationalImageRegistrationRequest (Id VNTrackTranslationalImageRegistrationRequest) where
  toVNTrackTranslationalImageRegistrationRequest = unsafeCastId

instance IsNSObject (Id VNTrackTranslationalImageRegistrationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackTranslationalImageRegistrationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackTranslationalImageRegistrationRequest) where
  toVNRequest = unsafeCastId

instance IsVNStatefulRequest (Id VNTrackTranslationalImageRegistrationRequest) where
  toVNStatefulRequest = unsafeCastId

-- ---------- VNGenerateOpticalFlowRequest ----------

-- | VNGenerateOpticalFlowRequest will determine directional change vectors for each pixel in the targeted image to transform it into the image processed        by the request handler, reporting this result with a single VNPixelBufferObservation.
--
-- Because this request works at the pixel level, both images must have the same dimensions in order for the request to be successfully performed.				Setting a region of interest will isolate where the change determination is performed; however, the resultant observation will still be reported				with a full resolution VNPixelBufferObservation.
--
-- Optical flow requests are very resource intensive, so it is recommended that only one request at a time be created and that the handler                where the request was issued be released immediately after generating optical flows.
--
-- Example usage:
--
-- - (nullable VNPixelBufferObservation*) opticalFlowFromImage:(CVPixelBufferRef)fromImage toImage:(CVPixelBuffer)toImage error:(NSError**)error					{						VNImageRequestHandler* imageRequestHandler = [[VNImageRequestHandler alloc] initWithCVPixelBuffer:fromImage options:\@{}];						VNGenerateOpticalFlowRequest* request = [[VNGenerateOpticalFlowRequest alloc] initWithTargetedCVPixelBuffer:toImage options:\@{}];						if (![imageRequestHandler performRequests:\@[ request ] error:error])						{							return nil;						}
--
-- return [[request results] firstObject];					}
-- 
-- Phantom type for @VNGenerateOpticalFlowRequest@.
data VNGenerateOpticalFlowRequest

instance IsObjCObject (Id VNGenerateOpticalFlowRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNGenerateOpticalFlowRequest"

class IsVNTargetedImageRequest a => IsVNGenerateOpticalFlowRequest a where
  toVNGenerateOpticalFlowRequest :: a -> Id VNGenerateOpticalFlowRequest

instance IsVNGenerateOpticalFlowRequest (Id VNGenerateOpticalFlowRequest) where
  toVNGenerateOpticalFlowRequest = unsafeCastId

instance IsNSObject (Id VNGenerateOpticalFlowRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNGenerateOpticalFlowRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNGenerateOpticalFlowRequest) where
  toVNRequest = unsafeCastId

instance IsVNTargetedImageRequest (Id VNGenerateOpticalFlowRequest) where
  toVNTargetedImageRequest = unsafeCastId

-- ---------- VNImageRegistrationRequest ----------

-- | A request that will calculate a transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the transformations that morph the floating image onto the reference image.
-- 
-- Phantom type for @VNImageRegistrationRequest@.
data VNImageRegistrationRequest

instance IsObjCObject (Id VNImageRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNImageRegistrationRequest"

class IsVNTargetedImageRequest a => IsVNImageRegistrationRequest a where
  toVNImageRegistrationRequest :: a -> Id VNImageRegistrationRequest

instance IsVNImageRegistrationRequest (Id VNImageRegistrationRequest) where
  toVNImageRegistrationRequest = unsafeCastId

instance IsNSObject (Id VNImageRegistrationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNImageRegistrationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNImageRegistrationRequest) where
  toVNRequest = unsafeCastId

instance IsVNTargetedImageRequest (Id VNImageRegistrationRequest) where
  toVNTargetedImageRequest = unsafeCastId

-- ---------- VNTrackObjectRequest ----------

-- | VNTrackObjectRequest tracks an object in a sequence of images.
--
-- The VNTrackObjectRequest is a general purpose object tracker. This tracker is used when the tracked entity does not have a special tracker, like VNTrackRectangleRequest. The VNTrackObjectRequest is initialized with VNDetectedObjectObservation that contains bounding box for the object of interest. This tracker is processed using one of the [VNSequenceRequestHandler performRequests:...] methods.
-- 
-- Phantom type for @VNTrackObjectRequest@.
data VNTrackObjectRequest

instance IsObjCObject (Id VNTrackObjectRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackObjectRequest"

class IsVNTrackingRequest a => IsVNTrackObjectRequest a where
  toVNTrackObjectRequest :: a -> Id VNTrackObjectRequest

instance IsVNTrackObjectRequest (Id VNTrackObjectRequest) where
  toVNTrackObjectRequest = unsafeCastId

instance IsNSObject (Id VNTrackObjectRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackObjectRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackObjectRequest) where
  toVNRequest = unsafeCastId

instance IsVNTrackingRequest (Id VNTrackObjectRequest) where
  toVNTrackingRequest = unsafeCastId

-- ---------- VNTrackRectangleRequest ----------

-- | VNTrackRectangleRequest tracks a rectangle in a sequence of images.
--
-- The VNTrackRectangleRequest is a special tracker to track rectangular shape objects. The VNTrackRectangleRequest is initialized with a VNRectangleObservation object that contains a rectangle bounding box and four corners locations. VNRectangleObservation can be obtained by running rectangle detector  (VNDetectRectanglesRequest). The VNTrackRectangleRequest is processed using one of the [VNSequenceRequestHandler performRequests:...] methods.
--
-- Note: The rectangular object doesn't have to look like a rectangle when projected into the plane of the image of interest. For example, it may look like trapezoid.
-- 
-- Phantom type for @VNTrackRectangleRequest@.
data VNTrackRectangleRequest

instance IsObjCObject (Id VNTrackRectangleRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTrackRectangleRequest"

class IsVNTrackingRequest a => IsVNTrackRectangleRequest a where
  toVNTrackRectangleRequest :: a -> Id VNTrackRectangleRequest

instance IsVNTrackRectangleRequest (Id VNTrackRectangleRequest) where
  toVNTrackRectangleRequest = unsafeCastId

instance IsNSObject (Id VNTrackRectangleRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTrackRectangleRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNRequest (Id VNTrackRectangleRequest) where
  toVNRequest = unsafeCastId

instance IsVNTrackingRequest (Id VNTrackRectangleRequest) where
  toVNTrackingRequest = unsafeCastId

-- ---------- VNHomographicImageRegistrationRequest ----------

-- | An image registration request that will calculate a homographic transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the matrix warp transform that morph the floating image onto the reference image.	            Note that the request will fail unless the pixel dimensions of the reference image do not exactly match the resolved region of interest of the floating image.
-- 
-- Phantom type for @VNHomographicImageRegistrationRequest@.
data VNHomographicImageRegistrationRequest

instance IsObjCObject (Id VNHomographicImageRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNHomographicImageRegistrationRequest"

class IsVNImageRegistrationRequest a => IsVNHomographicImageRegistrationRequest a where
  toVNHomographicImageRegistrationRequest :: a -> Id VNHomographicImageRegistrationRequest

instance IsVNHomographicImageRegistrationRequest (Id VNHomographicImageRegistrationRequest) where
  toVNHomographicImageRegistrationRequest = unsafeCastId

instance IsNSObject (Id VNHomographicImageRegistrationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNHomographicImageRegistrationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNImageRegistrationRequest (Id VNHomographicImageRegistrationRequest) where
  toVNImageRegistrationRequest = unsafeCastId

instance IsVNRequest (Id VNHomographicImageRegistrationRequest) where
  toVNRequest = unsafeCastId

instance IsVNTargetedImageRequest (Id VNHomographicImageRegistrationRequest) where
  toVNTargetedImageRequest = unsafeCastId

-- ---------- VNTranslationalImageRegistrationRequest ----------

-- | An image registration request that will calculate a translational transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the affine transformations that morph the floating image onto the reference image.
-- 
-- Phantom type for @VNTranslationalImageRegistrationRequest@.
data VNTranslationalImageRegistrationRequest

instance IsObjCObject (Id VNTranslationalImageRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VNTranslationalImageRegistrationRequest"

class IsVNImageRegistrationRequest a => IsVNTranslationalImageRegistrationRequest a where
  toVNTranslationalImageRegistrationRequest :: a -> Id VNTranslationalImageRegistrationRequest

instance IsVNTranslationalImageRegistrationRequest (Id VNTranslationalImageRegistrationRequest) where
  toVNTranslationalImageRegistrationRequest = unsafeCastId

instance IsNSObject (Id VNTranslationalImageRegistrationRequest) where
  toNSObject = unsafeCastId

instance IsVNImageBasedRequest (Id VNTranslationalImageRegistrationRequest) where
  toVNImageBasedRequest = unsafeCastId

instance IsVNImageRegistrationRequest (Id VNTranslationalImageRegistrationRequest) where
  toVNImageRegistrationRequest = unsafeCastId

instance IsVNRequest (Id VNTranslationalImageRegistrationRequest) where
  toVNRequest = unsafeCastId

instance IsVNTargetedImageRequest (Id VNTranslationalImageRegistrationRequest) where
  toVNTargetedImageRequest = unsafeCastId
