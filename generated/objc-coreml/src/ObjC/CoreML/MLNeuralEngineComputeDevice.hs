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
init_ :: IsMLNeuralEngineComputeDevice mlNeuralEngineComputeDevice => mlNeuralEngineComputeDevice -> IO (Id MLNeuralEngineComputeDevice)
init_ mlNeuralEngineComputeDevice  =
  sendMsg mlNeuralEngineComputeDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLNeuralEngineComputeDevice)
new  =
  do
    cls' <- getRequiredClass "MLNeuralEngineComputeDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The total number of cores in the NeuralEngine.
--
-- ObjC selector: @- totalCoreCount@
totalCoreCount :: IsMLNeuralEngineComputeDevice mlNeuralEngineComputeDevice => mlNeuralEngineComputeDevice -> IO CLong
totalCoreCount mlNeuralEngineComputeDevice  =
  sendMsg mlNeuralEngineComputeDevice (mkSelector "totalCoreCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @totalCoreCount@
totalCoreCountSelector :: Selector
totalCoreCountSelector = mkSelector "totalCoreCount"

