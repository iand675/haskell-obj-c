{-# LANGUAGE DataKinds #-}
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
  , loadContentsOfURL_completionHandlerSelector
  , loadModelAsset_completionHandlerSelector
  , neuralNetworkSelector
  , newSelector
  , pipelineSelector
  , programSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructure)
init_ mlModelStructure =
  sendOwnedMessage mlModelStructure initSelector

-- | @+ new@
new :: IO (Id MLModelStructure)
new  =
  do
    cls' <- getRequiredClass "MLModelStructure"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' loadContentsOfURL_completionHandlerSelector (toNSURL url) handler

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
    sendClassMessage cls' loadModelAsset_completionHandlerSelector (toMLModelAsset asset) handler

-- | If the model is of NeuralNetwork type then it is the structure of the NeuralNetwork otherwise @nil@.
--
-- ObjC selector: @- neuralNetwork@
neuralNetwork :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructureNeuralNetwork)
neuralNetwork mlModelStructure =
  sendMessage mlModelStructure neuralNetworkSelector

-- | If the model is of ML Program type then it is the structure of the ML Program otherwise @nil@.
--
-- ObjC selector: @- program@
program :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructureProgram)
program mlModelStructure =
  sendMessage mlModelStructure programSelector

-- | If the model is of Pipeline type then it is the structure of the Pipeline otherwise @nil@.
--
-- ObjC selector: @- pipeline@
pipeline :: IsMLModelStructure mlModelStructure => mlModelStructure -> IO (Id MLModelStructurePipeline)
pipeline mlModelStructure =
  sendMessage mlModelStructure pipelineSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructure)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructure)
newSelector = mkSelector "new"

-- | @Selector@ for @loadContentsOfURL:completionHandler:@
loadContentsOfURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
loadContentsOfURL_completionHandlerSelector = mkSelector "loadContentsOfURL:completionHandler:"

-- | @Selector@ for @loadModelAsset:completionHandler:@
loadModelAsset_completionHandlerSelector :: Selector '[Id MLModelAsset, Ptr ()] ()
loadModelAsset_completionHandlerSelector = mkSelector "loadModelAsset:completionHandler:"

-- | @Selector@ for @neuralNetwork@
neuralNetworkSelector :: Selector '[] (Id MLModelStructureNeuralNetwork)
neuralNetworkSelector = mkSelector "neuralNetwork"

-- | @Selector@ for @program@
programSelector :: Selector '[] (Id MLModelStructureProgram)
programSelector = mkSelector "program"

-- | @Selector@ for @pipeline@
pipelineSelector :: Selector '[] (Id MLModelStructurePipeline)
pipelineSelector = mkSelector "pipeline"

