{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the structure of a model.
--
-- ``` // Load the model structure. [MLModelStructure loadContentsOfURL:modelURL completionHandler:^(MLModelStructure * _Nullable modelStructure, NSError * _Nullable error) {    if (!modelStructure) {        // Handle error.        return;    }    if (modelStructure.neuralNetwork) {        // Examine Neural network model.    } else if (modelStructure.program) {        // Examine ML Program model.    } else if (modelStructure.pipeline) {        // Examine Pipeline model.    } else {        // The model type is something else.    } }]; ```
--
-- Generated bindings for @MLModelStructure@.
module ObjC.CoreML.MLModelStructure
  ( MLModelStructure
  , IsMLModelStructure(..)
  , init_
  , new
  , loadContentsOfURL_completionHandler
  , loadModelAsset_completionHandler
  , neuralNetwork
  , program
  , pipeline
  , initSelector
  , newSelector
  , loadContentsOfURL_completionHandlerSelector
  , loadModelAsset_completionHandlerSelector
  , neuralNetworkSelector
  , programSelector
  , pipelineSelector


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

-- | @- init@
init_ :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructure)
init_ mlModelStructure  =
  sendMsg mlModelStructure (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructure)
new  =
  do
    cls' <- getRequiredClass "MLModelStructure"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Construct the model structure asynchronously given the location of its on-disk representation.
--
-- @url@ — The location of its on-disk representation (.mlmodelc directory).
--
-- @handler@ — When the model structure is constructed successfully or unsuccessfully, the completion handler is invoked with a valid MLModelStructure instance or NSError object.
--
-- ObjC selector: @+ loadContentsOfURL:completionHandler:@
loadContentsOfURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
loadContentsOfURL_completionHandler url handler =
  do
    cls' <- getRequiredClass "MLModelStructure"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "loadContentsOfURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Construct the model structure asynchronously  given the model asset.
--
-- @asset@ — The model asset.
--
-- @handler@ — When the model structure is constructed successfully or unsuccessfully, the completion handler is invoked with a valid MLModelStructure instance or NSError object.
--
-- ObjC selector: @+ loadModelAsset:completionHandler:@
loadModelAsset_completionHandler :: IsMLModelAsset asset => asset -> Ptr () -> IO ()
loadModelAsset_completionHandler asset handler =
  do
    cls' <- getRequiredClass "MLModelStructure"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "loadModelAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | If the model is of NeuralNetwork type then it is the structure of the NeuralNetwork otherwise @nil@.
--
-- ObjC selector: @- neuralNetwork@
neuralNetwork :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructureNeuralNetwork)
neuralNetwork mlModelStructure  =
  sendMsg mlModelStructure (mkSelector "neuralNetwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the model is of ML Program type then it is the structure of the ML Program otherwise @nil@.
--
-- ObjC selector: @- program@
program :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructureProgram)
program mlModelStructure  =
  sendMsg mlModelStructure (mkSelector "program") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the model is of Pipeline type then it is the structure of the Pipeline otherwise @nil@.
--
-- ObjC selector: @- pipeline@
pipeline :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructurePipeline)
pipeline mlModelStructure  =
  sendMsg mlModelStructure (mkSelector "pipeline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @loadContentsOfURL:completionHandler:@
loadContentsOfURL_completionHandlerSelector :: Selector
loadContentsOfURL_completionHandlerSelector = mkSelector "loadContentsOfURL:completionHandler:"

-- | @Selector@ for @loadModelAsset:completionHandler:@
loadModelAsset_completionHandlerSelector :: Selector
loadModelAsset_completionHandlerSelector = mkSelector "loadModelAsset:completionHandler:"

-- | @Selector@ for @neuralNetwork@
neuralNetworkSelector :: Selector
neuralNetworkSelector = mkSelector "neuralNetwork"

-- | @Selector@ for @program@
programSelector :: Selector
programSelector = mkSelector "program"

-- | @Selector@ for @pipeline@
pipelineSelector :: Selector
pipelineSelector = mkSelector "pipeline"

