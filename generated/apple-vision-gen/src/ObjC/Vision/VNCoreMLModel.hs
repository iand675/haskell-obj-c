{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The VNCoreMLModel uses an CoreML based model and prepares it for use with VNCoreMLRequests.
--
-- Generated bindings for @VNCoreMLModel@.
module ObjC.Vision.VNCoreMLModel
  ( VNCoreMLModel
  , IsVNCoreMLModel(..)
  , init_
  , modelForMLModel_error
  , inputImageFeatureName
  , setInputImageFeatureName
  , featureProvider
  , setFeatureProvider
  , featureProviderSelector
  , initSelector
  , inputImageFeatureNameSelector
  , modelForMLModel_errorSelector
  , setFeatureProviderSelector
  , setInputImageFeatureNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> IO (Id VNCoreMLModel)
init_ vnCoreMLModel =
  sendOwnedMessage vnCoreMLModel initSelector

-- | Create a model container to be used with VNCoreMLRequest based on a Core ML model. This can fail if the model is not supported. Examples for a model that is not supported is a model that does not take an image as any of its inputs.
--
-- @model@ — The MLModel from CoreML to be used.
--
-- @error@ — Returns the error code and description, if the model is not supported.
--
-- ObjC selector: @+ modelForMLModel:error:@
modelForMLModel_error :: (IsMLModel model, IsNSError error_) => model -> error_ -> IO (Id VNCoreMLModel)
modelForMLModel_error model error_ =
  do
    cls' <- getRequiredClass "VNCoreMLModel"
    sendClassMessage cls' modelForMLModel_errorSelector (toMLModel model) (toNSError error_)

-- | The name of the MLFeatureValue that Vision will set from the VNRequestHandler. Vision will use the first input it finds by default but it can be set to another featureName instead.
--
-- ObjC selector: @- inputImageFeatureName@
inputImageFeatureName :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> IO (Id NSString)
inputImageFeatureName vnCoreMLModel =
  sendMessage vnCoreMLModel inputImageFeatureNameSelector

-- | The name of the MLFeatureValue that Vision will set from the VNRequestHandler. Vision will use the first input it finds by default but it can be set to another featureName instead.
--
-- ObjC selector: @- setInputImageFeatureName:@
setInputImageFeatureName :: (IsVNCoreMLModel vnCoreMLModel, IsNSString value) => vnCoreMLModel -> value -> IO ()
setInputImageFeatureName vnCoreMLModel value =
  sendMessage vnCoreMLModel setInputImageFeatureNameSelector (toNSString value)

-- | An optional object conforming to the MLFeatureProvider protocol that is used by the model during the predict call to support inputs that are not supplied by Vision. Vision will provide the image for the inputImageFeatureName from the the VNRequestHandler. A feature provider is necessary for models that have more than one input and require those parameters to be set. Models that only have one image input will not use the feature provider as that input will be set by Vision.
--
-- ObjC selector: @- featureProvider@
featureProvider :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> IO RawId
featureProvider vnCoreMLModel =
  sendMessage vnCoreMLModel featureProviderSelector

-- | An optional object conforming to the MLFeatureProvider protocol that is used by the model during the predict call to support inputs that are not supplied by Vision. Vision will provide the image for the inputImageFeatureName from the the VNRequestHandler. A feature provider is necessary for models that have more than one input and require those parameters to be set. Models that only have one image input will not use the feature provider as that input will be set by Vision.
--
-- ObjC selector: @- setFeatureProvider:@
setFeatureProvider :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> RawId -> IO ()
setFeatureProvider vnCoreMLModel value =
  sendMessage vnCoreMLModel setFeatureProviderSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNCoreMLModel)
initSelector = mkSelector "init"

-- | @Selector@ for @modelForMLModel:error:@
modelForMLModel_errorSelector :: Selector '[Id MLModel, Id NSError] (Id VNCoreMLModel)
modelForMLModel_errorSelector = mkSelector "modelForMLModel:error:"

-- | @Selector@ for @inputImageFeatureName@
inputImageFeatureNameSelector :: Selector '[] (Id NSString)
inputImageFeatureNameSelector = mkSelector "inputImageFeatureName"

-- | @Selector@ for @setInputImageFeatureName:@
setInputImageFeatureNameSelector :: Selector '[Id NSString] ()
setInputImageFeatureNameSelector = mkSelector "setInputImageFeatureName:"

-- | @Selector@ for @featureProvider@
featureProviderSelector :: Selector '[] RawId
featureProviderSelector = mkSelector "featureProvider"

-- | @Selector@ for @setFeatureProvider:@
setFeatureProviderSelector :: Selector '[RawId] ()
setFeatureProviderSelector = mkSelector "setFeatureProvider:"

