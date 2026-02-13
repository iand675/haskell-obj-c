{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a layer in a NeuralNetwork.
--
-- Generated bindings for @MLModelStructureNeuralNetworkLayer@.
module ObjC.CoreML.MLModelStructureNeuralNetworkLayer
  ( MLModelStructureNeuralNetworkLayer
  , IsMLModelStructureNeuralNetworkLayer(..)
  , init_
  , new
  , name
  , type_
  , inputNames
  , outputNames
  , initSelector
  , inputNamesSelector
  , nameSelector
  , newSelector
  , outputNamesSelector
  , typeSelector


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
init_ :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id MLModelStructureNeuralNetworkLayer)
init_ mlModelStructureNeuralNetworkLayer =
  sendOwnedMessage mlModelStructureNeuralNetworkLayer initSelector

-- | @+ new@
new :: IO (Id MLModelStructureNeuralNetworkLayer)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureNeuralNetworkLayer"
    sendOwnedClassMessage cls' newSelector

-- | The layer name.
--
-- ObjC selector: @- name@
name :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSString)
name mlModelStructureNeuralNetworkLayer =
  sendMessage mlModelStructureNeuralNetworkLayer nameSelector

-- | The type of the layer, e,g, "elementwise", "pooling", etc.
--
-- ObjC selector: @- type@
type_ :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSString)
type_ mlModelStructureNeuralNetworkLayer =
  sendMessage mlModelStructureNeuralNetworkLayer typeSelector

-- | The input names.
--
-- ObjC selector: @- inputNames@
inputNames :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSArray)
inputNames mlModelStructureNeuralNetworkLayer =
  sendMessage mlModelStructureNeuralNetworkLayer inputNamesSelector

-- | The output names.
--
-- ObjC selector: @- outputNames@
outputNames :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSArray)
outputNames mlModelStructureNeuralNetworkLayer =
  sendMessage mlModelStructureNeuralNetworkLayer outputNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureNeuralNetworkLayer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureNeuralNetworkLayer)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @inputNames@
inputNamesSelector :: Selector '[] (Id NSArray)
inputNamesSelector = mkSelector "inputNames"

-- | @Selector@ for @outputNames@
outputNamesSelector :: Selector '[] (Id NSArray)
outputNamesSelector = mkSelector "outputNames"

