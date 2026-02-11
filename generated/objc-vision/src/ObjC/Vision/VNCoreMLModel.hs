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
  , initSelector
  , modelForMLModel_errorSelector
  , inputImageFeatureNameSelector
  , setInputImageFeatureNameSelector


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
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> IO (Id VNCoreMLModel)
init_ vnCoreMLModel  =
  sendMsg vnCoreMLModel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr model $ \raw_model ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "modelForMLModel:error:") (retPtr retVoid) [argPtr (castPtr raw_model :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | The name of the MLFeatureValue that Vision will set from the VNRequestHandler. Vision will use the first input it finds by default but it can be set to another featureName instead.
--
-- ObjC selector: @- inputImageFeatureName@
inputImageFeatureName :: IsVNCoreMLModel vnCoreMLModel => vnCoreMLModel -> IO (Id NSString)
inputImageFeatureName vnCoreMLModel  =
  sendMsg vnCoreMLModel (mkSelector "inputImageFeatureName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the MLFeatureValue that Vision will set from the VNRequestHandler. Vision will use the first input it finds by default but it can be set to another featureName instead.
--
-- ObjC selector: @- setInputImageFeatureName:@
setInputImageFeatureName :: (IsVNCoreMLModel vnCoreMLModel, IsNSString value) => vnCoreMLModel -> value -> IO ()
setInputImageFeatureName vnCoreMLModel  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnCoreMLModel (mkSelector "setInputImageFeatureName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @modelForMLModel:error:@
modelForMLModel_errorSelector :: Selector
modelForMLModel_errorSelector = mkSelector "modelForMLModel:error:"

-- | @Selector@ for @inputImageFeatureName@
inputImageFeatureNameSelector :: Selector
inputImageFeatureNameSelector = mkSelector "inputImageFeatureName"

-- | @Selector@ for @setInputImageFeatureName:@
setInputImageFeatureNameSelector :: Selector
setInputImageFeatureNameSelector = mkSelector "setInputImageFeatureName:"

