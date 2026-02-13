{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the structure of a NeuralNetwork model.
--
-- Generated bindings for @MLModelStructureNeuralNetwork@.
module ObjC.CoreML.MLModelStructureNeuralNetwork
  ( MLModelStructureNeuralNetwork
  , IsMLModelStructureNeuralNetwork(..)
  , init_
  , new
  , layers
  , initSelector
  , layersSelector
  , newSelector


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
init_ :: IsMLModelStructureNeuralNetwork mlModelStructureNeuralNetwork => mlModelStructureNeuralNetwork -> IO (Id MLModelStructureNeuralNetwork)
init_ mlModelStructureNeuralNetwork =
  sendOwnedMessage mlModelStructureNeuralNetwork initSelector

-- | @+ new@
new :: IO (Id MLModelStructureNeuralNetwork)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureNeuralNetwork"
    sendOwnedClassMessage cls' newSelector

-- | The topologically sorted layers in the NeuralNetwork.
--
-- ObjC selector: @- layers@
layers :: IsMLModelStructureNeuralNetwork mlModelStructureNeuralNetwork => mlModelStructureNeuralNetwork -> IO (Id NSArray)
layers mlModelStructureNeuralNetwork =
  sendMessage mlModelStructureNeuralNetwork layersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureNeuralNetwork)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureNeuralNetwork)
newSelector = mkSelector "new"

-- | @Selector@ for @layers@
layersSelector :: Selector '[] (Id NSArray)
layersSelector = mkSelector "layers"

