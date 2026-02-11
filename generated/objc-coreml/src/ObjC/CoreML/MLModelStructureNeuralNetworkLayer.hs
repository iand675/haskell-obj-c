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
  , newSelector
  , nameSelector
  , typeSelector
  , inputNamesSelector
  , outputNamesSelector


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
init_ :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id MLModelStructureNeuralNetworkLayer)
init_ mlModelStructureNeuralNetworkLayer  =
  sendMsg mlModelStructureNeuralNetworkLayer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureNeuralNetworkLayer)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureNeuralNetworkLayer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The layer name.
--
-- ObjC selector: @- name@
name :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSString)
name mlModelStructureNeuralNetworkLayer  =
  sendMsg mlModelStructureNeuralNetworkLayer (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of the layer, e,g, "elementwise", "pooling", etc.
--
-- ObjC selector: @- type@
type_ :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSString)
type_ mlModelStructureNeuralNetworkLayer  =
  sendMsg mlModelStructureNeuralNetworkLayer (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The input names.
--
-- ObjC selector: @- inputNames@
inputNames :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSArray)
inputNames mlModelStructureNeuralNetworkLayer  =
  sendMsg mlModelStructureNeuralNetworkLayer (mkSelector "inputNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The output names.
--
-- ObjC selector: @- outputNames@
outputNames :: IsMLModelStructureNeuralNetworkLayer mlModelStructureNeuralNetworkLayer => mlModelStructureNeuralNetworkLayer -> IO (Id NSArray)
outputNames mlModelStructureNeuralNetworkLayer  =
  sendMsg mlModelStructureNeuralNetworkLayer (mkSelector "outputNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @inputNames@
inputNamesSelector :: Selector
inputNamesSelector = mkSelector "inputNames"

-- | @Selector@ for @outputNames@
outputNamesSelector :: Selector
outputNamesSelector = mkSelector "outputNames"

