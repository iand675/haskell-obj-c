{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The VNCoreMLRequest uses a VNCoreMLModel, that is based on a CoreML MLModel object, to run predictions with that model. Depending on the model the returned             observation is either a VNClassificationObservation for classifier models, VNPixelBufferObservations for image-to-image models, VNRecognizedObjectObservation for object recognition models or VNCoreMLFeatureValueObservation for everything else. All -[VNObservation confidence] values are forwarded from relevant models as is, and do not conform to a common [0, 1] range rule
--
-- Generated bindings for @VNCoreMLRequest@.
module ObjC.Vision.VNCoreMLRequest
  ( VNCoreMLRequest
  , IsVNCoreMLRequest(..)
  , initWithModel
  , initWithModel_completionHandler
  , init_
  , initWithCompletionHandler
  , model
  , imageCropAndScaleOption
  , setImageCropAndScaleOption
  , initWithModelSelector
  , initWithModel_completionHandlerSelector
  , initSelector
  , initWithCompletionHandlerSelector
  , modelSelector
  , imageCropAndScaleOptionSelector
  , setImageCropAndScaleOptionSelector

  -- * Enum types
  , VNImageCropAndScaleOption(VNImageCropAndScaleOption)
  , pattern VNImageCropAndScaleOptionCenterCrop
  , pattern VNImageCropAndScaleOptionScaleFit
  , pattern VNImageCropAndScaleOptionScaleFill
  , pattern VNImageCropAndScaleOptionScaleFitRotate90CCW
  , pattern VNImageCropAndScaleOptionScaleFillRotate90CCW

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

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a new request with a model.
--
-- @model@ — The VNCoreMLModel to be used.
--
-- ObjC selector: @- initWithModel:@
initWithModel :: (IsVNCoreMLRequest vnCoreMLRequest, IsVNCoreMLModel model) => vnCoreMLRequest -> model -> IO (Id VNCoreMLRequest)
initWithModel vnCoreMLRequest  model =
withObjCPtr model $ \raw_model ->
    sendMsg vnCoreMLRequest (mkSelector "initWithModel:") (retPtr retVoid) [argPtr (castPtr raw_model :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a model.
--
-- @model@ — The VNCoreMLModel to be used.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithModel:completionHandler:@
initWithModel_completionHandler :: (IsVNCoreMLRequest vnCoreMLRequest, IsVNCoreMLModel model) => vnCoreMLRequest -> model -> Ptr () -> IO (Id VNCoreMLRequest)
initWithModel_completionHandler vnCoreMLRequest  model completionHandler =
withObjCPtr model $ \raw_model ->
    sendMsg vnCoreMLRequest (mkSelector "initWithModel:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_model :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO (Id VNCoreMLRequest)
init_ vnCoreMLRequest  =
  sendMsg vnCoreMLRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> Ptr () -> IO (Id VNCoreMLRequest)
initWithCompletionHandler vnCoreMLRequest  completionHandler =
  sendMsg vnCoreMLRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | The model from CoreML wrapped in a VNCoreMLModel.
--
-- ObjC selector: @- model@
model :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO (Id VNCoreMLModel)
model vnCoreMLRequest  =
  sendMsg vnCoreMLRequest (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageCropAndScaleOption@
imageCropAndScaleOption :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO VNImageCropAndScaleOption
imageCropAndScaleOption vnCoreMLRequest  =
  fmap (coerce :: CULong -> VNImageCropAndScaleOption) $ sendMsg vnCoreMLRequest (mkSelector "imageCropAndScaleOption") retCULong []

-- | @- setImageCropAndScaleOption:@
setImageCropAndScaleOption :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> VNImageCropAndScaleOption -> IO ()
setImageCropAndScaleOption vnCoreMLRequest  value =
  sendMsg vnCoreMLRequest (mkSelector "setImageCropAndScaleOption:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithModel:@
initWithModelSelector :: Selector
initWithModelSelector = mkSelector "initWithModel:"

-- | @Selector@ for @initWithModel:completionHandler:@
initWithModel_completionHandlerSelector :: Selector
initWithModel_completionHandlerSelector = mkSelector "initWithModel:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @imageCropAndScaleOption@
imageCropAndScaleOptionSelector :: Selector
imageCropAndScaleOptionSelector = mkSelector "imageCropAndScaleOption"

-- | @Selector@ for @setImageCropAndScaleOption:@
setImageCropAndScaleOptionSelector :: Selector
setImageCropAndScaleOptionSelector = mkSelector "setImageCropAndScaleOption:"

