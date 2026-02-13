{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a CPU compute device.
--
-- Generated bindings for @MLCPUComputeDevice@.
module ObjC.CoreML.MLCPUComputeDevice
  ( MLCPUComputeDevice
  , IsMLCPUComputeDevice(..)
  , init_
  , new
  , initSelector
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
init_ :: IsMLCPUComputeDevice mlcpuComputeDevice => mlcpuComputeDevice -> IO (Id MLCPUComputeDevice)
init_ mlcpuComputeDevice =
  sendOwnedMessage mlcpuComputeDevice initSelector

-- | @+ new@
new :: IO (Id MLCPUComputeDevice)
new  =
  do
    cls' <- getRequiredClass "MLCPUComputeDevice"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCPUComputeDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCPUComputeDevice)
newSelector = mkSelector "new"

