{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a NeuralEngine compute device for inference of machine learning models.
--
-- Generated bindings for @MLNeuralEngineComputeDevice@.
module ObjC.CoreML.MLNeuralEngineComputeDevice
  ( MLNeuralEngineComputeDevice
  , IsMLNeuralEngineComputeDevice(..)
  , init_
  , new
  , totalCoreCount
  , initSelector
  , newSelector
  , totalCoreCountSelector


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
init_ :: IsMLNeuralEngineComputeDevice mlNeuralEngineComputeDevice => mlNeuralEngineComputeDevice -> IO (Id MLNeuralEngineComputeDevice)
init_ mlNeuralEngineComputeDevice =
  sendOwnedMessage mlNeuralEngineComputeDevice initSelector

-- | @+ new@
new :: IO (Id MLNeuralEngineComputeDevice)
new  =
  do
    cls' <- getRequiredClass "MLNeuralEngineComputeDevice"
    sendOwnedClassMessage cls' newSelector

-- | The total number of cores in the NeuralEngine.
--
-- ObjC selector: @- totalCoreCount@
totalCoreCount :: IsMLNeuralEngineComputeDevice mlNeuralEngineComputeDevice => mlNeuralEngineComputeDevice -> IO CLong
totalCoreCount mlNeuralEngineComputeDevice =
  sendMessage mlNeuralEngineComputeDevice totalCoreCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLNeuralEngineComputeDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLNeuralEngineComputeDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @totalCoreCount@
totalCoreCountSelector :: Selector '[] CLong
totalCoreCountSelector = mkSelector "totalCoreCount"

