{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLModel
--
-- Construct a model and evaluate on a specific set of input features. Inputs and outputs are accessed via the MLFeatureProvider protocol. Returns a model or nil if there is an error.
--
-- Generated bindings for @MLModel@.
module ObjC.CoreML.MLModel
  ( MLModel
  , IsMLModel(..)
  , modelWithContentsOfURL_error
  , modelWithContentsOfURL_configuration_error
  , loadContentsOfURL_configuration_completionHandler
  , predictionFromFeatures_error
  , predictionFromFeatures_options_error
  , predictionsFromBatch_error
  , predictionsFromBatch_options_error
  , parameterValueForKey_error
  , loadModelAsset_configuration_completionHandler
  , newState
  , predictionFromFeatures_usingState_error
  , predictionFromFeatures_usingState_options_error
  , compileModelAtURL_error
  , compileModelAtURL_completionHandler
  , modelDescription
  , configuration
  , modelWithContentsOfURL_errorSelector
  , modelWithContentsOfURL_configuration_errorSelector
  , loadContentsOfURL_configuration_completionHandlerSelector
  , predictionFromFeatures_errorSelector
  , predictionFromFeatures_options_errorSelector
  , predictionsFromBatch_errorSelector
  , predictionsFromBatch_options_errorSelector
  , parameterValueForKey_errorSelector
  , loadModelAsset_configuration_completionHandlerSelector
  , newStateSelector
  , predictionFromFeatures_usingState_errorSelector
  , predictionFromFeatures_usingState_options_errorSelector
  , compileModelAtURL_errorSelector
  , compileModelAtURL_completionHandlerSelector
  , modelDescriptionSelector
  , configurationSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Construct a model with a default MLModelConfiguration object
--
-- ObjC selector: @+ modelWithContentsOfURL:error:@
modelWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id MLModel)
modelWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "modelWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Construct a model given the location of its on-disk representation. Returns nil on error.
--
-- ObjC selector: @+ modelWithContentsOfURL:configuration:error:@
modelWithContentsOfURL_configuration_error :: (IsNSURL url, IsMLModelConfiguration configuration, IsNSError error_) => url -> configuration -> error_ -> IO (Id MLModel)
modelWithContentsOfURL_configuration_error url configuration error_ =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr url $ \raw_url ->
      withObjCPtr configuration $ \raw_configuration ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "modelWithContentsOfURL:configuration:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Construct a model asynchronously given the location of its on-disk representation and configuration.
--
-- Model loading may take time when the model content is not immediately available (e.g. encrypted model). Use this factory method especially when the caller is on the main thread.
--
-- @url@ — the location of its on-disk representation (.mlmodelc directory).
--
-- @configuration@ — The model configuration
--
-- @handler@ — When the model load completes successfully or unsuccessfully, the completion handler is invoked with a valid MLModel instance or NSError object.
--
-- ObjC selector: @+ loadContentsOfURL:configuration:completionHandler:@
loadContentsOfURL_configuration_completionHandler :: (IsNSURL url, IsMLModelConfiguration configuration) => url -> configuration -> Ptr () -> IO ()
loadContentsOfURL_configuration_completionHandler url configuration handler =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr url $ \raw_url ->
      withObjCPtr configuration $ \raw_configuration ->
        sendClassMsg cls' (mkSelector "loadContentsOfURL:configuration:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Run a prediction on a model synchronously.
--
-- This is a convenience overload method of @prediction(from:options:)@ that uses the default prediction options.
--
-- - Parameters   - input: The input features to make a prediction from.   - error: The output parameter to be filled with error information on failure. - Returns: The output features from the prediction.
--
-- ObjC selector: @- predictionFromFeatures:error:@
predictionFromFeatures_error :: (IsMLModel mlModel, IsNSError error_) => mlModel -> RawId -> error_ -> IO RawId
predictionFromFeatures_error mlModel  input error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionFromFeatures:error:") (retPtr retVoid) [argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Run a prediction on a model synchronously
--
-- - Parameters   - input: The input features to make a prediction from.   - options: Prediction options to modify how the prediction is run.   - error: The output parameter to be filled with error information on failure. - Returns: The output features from the prediction.
--
-- ObjC selector: @- predictionFromFeatures:options:error:@
predictionFromFeatures_options_error :: (IsMLModel mlModel, IsMLPredictionOptions options, IsNSError error_) => mlModel -> RawId -> options -> error_ -> IO RawId
predictionFromFeatures_options_error mlModel  input options error_ =
withObjCPtr options $ \raw_options ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionFromFeatures:options:error:") (retPtr retVoid) [argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Batch prediction without explicit options
--
-- ObjC selector: @- predictionsFromBatch:error:@
predictionsFromBatch_error :: (IsMLModel mlModel, IsNSError error_) => mlModel -> RawId -> error_ -> IO RawId
predictionsFromBatch_error mlModel  inputBatch error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionsFromBatch:error:") (retPtr retVoid) [argPtr (castPtr (unRawId inputBatch) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Batch prediction with explicit options
--
-- ObjC selector: @- predictionsFromBatch:options:error:@
predictionsFromBatch_options_error :: (IsMLModel mlModel, IsMLPredictionOptions options, IsNSError error_) => mlModel -> RawId -> options -> error_ -> IO RawId
predictionsFromBatch_options_error mlModel  inputBatch options error_ =
withObjCPtr options $ \raw_options ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionsFromBatch:options:error:") (retPtr retVoid) [argPtr (castPtr (unRawId inputBatch) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Provides value for the given parameter. Returns nil on error.
--
-- ObjC selector: @- parameterValueForKey:error:@
parameterValueForKey_error :: (IsMLModel mlModel, IsMLParameterKey key, IsNSError error_) => mlModel -> key -> error_ -> IO RawId
parameterValueForKey_error mlModel  key error_ =
withObjCPtr key $ \raw_key ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "parameterValueForKey:error:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Construct a model asynchronously from a compiled model asset.
--
-- @asset@ — Compiled model asset derived from in-memory or on-disk Core ML model
--
-- @configuration@ — Model configuration that hold options for loading a model
--
-- @handler@ — When the model load completes successfully or unsuccessfully, the completion handler is invoked with a valid MLModel instance or NSError object.
--
-- ObjC selector: @+ loadModelAsset:configuration:completionHandler:@
loadModelAsset_configuration_completionHandler :: (IsMLModelAsset asset, IsMLModelConfiguration configuration) => asset -> configuration -> Ptr () -> IO ()
loadModelAsset_configuration_completionHandler asset configuration handler =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr configuration $ \raw_configuration ->
        sendClassMsg cls' (mkSelector "loadModelAsset:configuration:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Creates a new state object.
--
-- Core ML framework will allocate the state buffers declared in the model.
--
-- The allocated state buffers are initialized to zeros. To initialize with different values, use @.withMultiArray(for:)@ to get the mutable @MLMultiArray@-view to the state buffer.
--
-- It returns an empty state when the model is stateless. One can use the empty state with stateful prediction functions such as @prediction(from:using:)@ and those predictions will be stateless. This simplifies the call site which may or may not use a stateful model.
--
-- ```swift // Create state that contains two state buffers: s1 and s2. // Then, initialize s1 to 1.0 and s2 to 2.0. let state = model.newState() state.withMultiArray(for: "s1") { stateMultiArray in     stateMultiArray[0] = 1.0 } state.withMultiArray(for: "s2") { stateMultiArray in     stateMultiArray[0] = 2.0 } ```
--
-- ObjC selector: @- newState@
newState :: IsMLModel mlModel => mlModel -> IO (Id MLState)
newState mlModel  =
  sendMsg mlModel (mkSelector "newState") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Run a stateful prediction synchronously.
--
-- Use this method to run predictions on a stateful model.
--
-- ```swift let state = model.newState() let prediction = try model.prediction(from: inputFeatures, using: state) ```
--
-- - Parameters:  - inputFeatures: The input features as declared in the model description.  - state: The state object created by @newState()@ method.  - error: The output parameter to receive an error information on failure.
--
-- ObjC selector: @- predictionFromFeatures:usingState:error:@
predictionFromFeatures_usingState_error :: (IsMLModel mlModel, IsMLState state, IsNSError error_) => mlModel -> RawId -> state -> error_ -> IO RawId
predictionFromFeatures_usingState_error mlModel  inputFeatures state error_ =
withObjCPtr state $ \raw_state ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionFromFeatures:usingState:error:") (retPtr retVoid) [argPtr (castPtr (unRawId inputFeatures) :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Run a stateful prediction synchronously with options.
--
-- Use this method to run predictions on a stateful model.
--
-- ```swift let state = model.newState() let prediction = try model.prediction(from: inputFeatures, using: state, options: predictionOptions) ```
--
-- - Parameters:  - inputFeatures: The input features as declared in the model description.  - state: The state object created by @newState()@ method.  - options: The prediction options.  - error: The output parameter to receive an error information on failure.
--
-- ObjC selector: @- predictionFromFeatures:usingState:options:error:@
predictionFromFeatures_usingState_options_error :: (IsMLModel mlModel, IsMLState state, IsMLPredictionOptions options, IsNSError error_) => mlModel -> RawId -> state -> options -> error_ -> IO RawId
predictionFromFeatures_usingState_options_error mlModel  inputFeatures state options error_ =
withObjCPtr state $ \raw_state ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg mlModel (mkSelector "predictionFromFeatures:usingState:options:error:") (retPtr retVoid) [argPtr (castPtr (unRawId inputFeatures) :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Compile a .mlmodel for this device
--
-- @modelURL@ — URL file path to .mlmodel file you wish to compile
--
-- @error@ — Any errors are surfaced here
--
-- a URL to the compiled .mlmodelc directory if successful The model is compiled to a temporary location on disk You must move the compiled model to a permanent location if you wish to keep it
--
-- The returned model can be loaded using:
--
-- [MLModel modelWithContentsOfURL:error:]
--
-- ObjC selector: @+ compileModelAtURL:error:@
compileModelAtURL_error :: (IsNSURL modelURL, IsNSError error_) => modelURL -> error_ -> IO (Id NSURL)
compileModelAtURL_error modelURL error_ =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "compileModelAtURL:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Compile a .mlmodel or .mlpackage for this device. Perform the compilation asynchronously.
--
-- @modelURL@ — URL file path to .mlmodel file you wish to compile
--
-- @handler@ — When the model compilation completes successfully  the completion handler is invoked with a valid URL to the compiled .mlmodelc directory. On failure, signified by nil  compiledModelURL, the NSError object is populated.
--
-- The model is compiled to a temporary location in the file system. You must move the compiled model to a permanent location if you wish to keep it. Then the model can be loaded using the returned URL:
--
-- [MLModel modelWithContentsOfURL:error:]
--
-- ObjC selector: @+ compileModelAtURL:completionHandler:@
compileModelAtURL_completionHandler :: IsNSURL modelURL => modelURL -> Ptr () -> IO ()
compileModelAtURL_completionHandler modelURL handler =
  do
    cls' <- getRequiredClass "MLModel"
    withObjCPtr modelURL $ \raw_modelURL ->
      sendClassMsg cls' (mkSelector "compileModelAtURL:completionHandler:") retVoid [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | A model holds a description of its required inputs and expected outputs.
--
-- ObjC selector: @- modelDescription@
modelDescription :: IsMLModel mlModel => mlModel -> IO (Id MLModelDescription)
modelDescription mlModel  =
  sendMsg mlModel (mkSelector "modelDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The load-time parameters used to instantiate this MLModel object.
--
-- ObjC selector: @- configuration@
configuration :: IsMLModel mlModel => mlModel -> IO (Id MLModelConfiguration)
configuration mlModel  =
  sendMsg mlModel (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modelWithContentsOfURL:error:@
modelWithContentsOfURL_errorSelector :: Selector
modelWithContentsOfURL_errorSelector = mkSelector "modelWithContentsOfURL:error:"

-- | @Selector@ for @modelWithContentsOfURL:configuration:error:@
modelWithContentsOfURL_configuration_errorSelector :: Selector
modelWithContentsOfURL_configuration_errorSelector = mkSelector "modelWithContentsOfURL:configuration:error:"

-- | @Selector@ for @loadContentsOfURL:configuration:completionHandler:@
loadContentsOfURL_configuration_completionHandlerSelector :: Selector
loadContentsOfURL_configuration_completionHandlerSelector = mkSelector "loadContentsOfURL:configuration:completionHandler:"

-- | @Selector@ for @predictionFromFeatures:error:@
predictionFromFeatures_errorSelector :: Selector
predictionFromFeatures_errorSelector = mkSelector "predictionFromFeatures:error:"

-- | @Selector@ for @predictionFromFeatures:options:error:@
predictionFromFeatures_options_errorSelector :: Selector
predictionFromFeatures_options_errorSelector = mkSelector "predictionFromFeatures:options:error:"

-- | @Selector@ for @predictionsFromBatch:error:@
predictionsFromBatch_errorSelector :: Selector
predictionsFromBatch_errorSelector = mkSelector "predictionsFromBatch:error:"

-- | @Selector@ for @predictionsFromBatch:options:error:@
predictionsFromBatch_options_errorSelector :: Selector
predictionsFromBatch_options_errorSelector = mkSelector "predictionsFromBatch:options:error:"

-- | @Selector@ for @parameterValueForKey:error:@
parameterValueForKey_errorSelector :: Selector
parameterValueForKey_errorSelector = mkSelector "parameterValueForKey:error:"

-- | @Selector@ for @loadModelAsset:configuration:completionHandler:@
loadModelAsset_configuration_completionHandlerSelector :: Selector
loadModelAsset_configuration_completionHandlerSelector = mkSelector "loadModelAsset:configuration:completionHandler:"

-- | @Selector@ for @newState@
newStateSelector :: Selector
newStateSelector = mkSelector "newState"

-- | @Selector@ for @predictionFromFeatures:usingState:error:@
predictionFromFeatures_usingState_errorSelector :: Selector
predictionFromFeatures_usingState_errorSelector = mkSelector "predictionFromFeatures:usingState:error:"

-- | @Selector@ for @predictionFromFeatures:usingState:options:error:@
predictionFromFeatures_usingState_options_errorSelector :: Selector
predictionFromFeatures_usingState_options_errorSelector = mkSelector "predictionFromFeatures:usingState:options:error:"

-- | @Selector@ for @compileModelAtURL:error:@
compileModelAtURL_errorSelector :: Selector
compileModelAtURL_errorSelector = mkSelector "compileModelAtURL:error:"

-- | @Selector@ for @compileModelAtURL:completionHandler:@
compileModelAtURL_completionHandlerSelector :: Selector
compileModelAtURL_completionHandlerSelector = mkSelector "compileModelAtURL:completionHandler:"

-- | @Selector@ for @modelDescription@
modelDescriptionSelector :: Selector
modelDescriptionSelector = mkSelector "modelDescription"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

