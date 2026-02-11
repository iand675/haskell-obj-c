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
  , newSelector
  , layersSelector


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
init_ :: IsMLModelStructureNeuralNetwork mlModelStructureNeuralNetwork => mlModelStructureNeuralNetwork -> IO (Id MLModelStructureNeuralNetwork)
init_ mlModelStructureNeuralNetwork  =
  sendMsg mlModelStructureNeuralNetwork (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureNeuralNetwork)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureNeuralNetwork"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The topologically sorted layers in the NeuralNetwork.
--
-- ObjC selector: @- layers@
layers :: IsMLModelStructureNeuralNetwork mlModelStructureNeuralNetwork => mlModelStructureNeuralNetwork -> IO (Id NSArray)
layers mlModelStructureNeuralNetwork  =
  sendMsg mlModelStructureNeuralNetwork (mkSelector "layers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @layers@
layersSelector :: Selector
layersSelector = mkSelector "layers"

