{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , imageCropAndScaleOptionSelector
  , initSelector
  , initWithCompletionHandlerSelector
  , initWithModelSelector
  , initWithModel_completionHandlerSelector
  , modelSelector
  , setImageCropAndScaleOptionSelector

  -- * Enum types
  , VNImageCropAndScaleOption(VNImageCropAndScaleOption)
  , pattern VNImageCropAndScaleOptionCenterCrop
  , pattern VNImageCropAndScaleOptionScaleFit
  , pattern VNImageCropAndScaleOptionScaleFill
  , pattern VNImageCropAndScaleOptionScaleFitRotate90CCW
  , pattern VNImageCropAndScaleOptionScaleFillRotate90CCW

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithModel vnCoreMLRequest model =
  sendOwnedMessage vnCoreMLRequest initWithModelSelector (toVNCoreMLModel model)

-- | Create a new request with a model.
--
-- @model@ — The VNCoreMLModel to be used.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithModel:completionHandler:@
initWithModel_completionHandler :: (IsVNCoreMLRequest vnCoreMLRequest, IsVNCoreMLModel model) => vnCoreMLRequest -> model -> Ptr () -> IO (Id VNCoreMLRequest)
initWithModel_completionHandler vnCoreMLRequest model completionHandler =
  sendOwnedMessage vnCoreMLRequest initWithModel_completionHandlerSelector (toVNCoreMLModel model) completionHandler

-- | @- init@
init_ :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO (Id VNCoreMLRequest)
init_ vnCoreMLRequest =
  sendOwnedMessage vnCoreMLRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> Ptr () -> IO (Id VNCoreMLRequest)
initWithCompletionHandler vnCoreMLRequest completionHandler =
  sendOwnedMessage vnCoreMLRequest initWithCompletionHandlerSelector completionHandler

-- | The model from CoreML wrapped in a VNCoreMLModel.
--
-- ObjC selector: @- model@
model :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO (Id VNCoreMLModel)
model vnCoreMLRequest =
  sendMessage vnCoreMLRequest modelSelector

-- | @- imageCropAndScaleOption@
imageCropAndScaleOption :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> IO VNImageCropAndScaleOption
imageCropAndScaleOption vnCoreMLRequest =
  sendMessage vnCoreMLRequest imageCropAndScaleOptionSelector

-- | @- setImageCropAndScaleOption:@
setImageCropAndScaleOption :: IsVNCoreMLRequest vnCoreMLRequest => vnCoreMLRequest -> VNImageCropAndScaleOption -> IO ()
setImageCropAndScaleOption vnCoreMLRequest value =
  sendMessage vnCoreMLRequest setImageCropAndScaleOptionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithModel:@
initWithModelSelector :: Selector '[Id VNCoreMLModel] (Id VNCoreMLRequest)
initWithModelSelector = mkSelector "initWithModel:"

-- | @Selector@ for @initWithModel:completionHandler:@
initWithModel_completionHandlerSelector :: Selector '[Id VNCoreMLModel, Ptr ()] (Id VNCoreMLRequest)
initWithModel_completionHandlerSelector = mkSelector "initWithModel:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNCoreMLRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNCoreMLRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id VNCoreMLModel)
modelSelector = mkSelector "model"

-- | @Selector@ for @imageCropAndScaleOption@
imageCropAndScaleOptionSelector :: Selector '[] VNImageCropAndScaleOption
imageCropAndScaleOptionSelector = mkSelector "imageCropAndScaleOption"

-- | @Selector@ for @setImageCropAndScaleOption:@
setImageCropAndScaleOptionSelector :: Selector '[VNImageCropAndScaleOption] ()
setImageCropAndScaleOptionSelector = mkSelector "setImageCropAndScaleOption:"

