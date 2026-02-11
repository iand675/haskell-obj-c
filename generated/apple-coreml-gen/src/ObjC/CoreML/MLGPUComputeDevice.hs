{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a GPU compute device.
--
-- Generated bindings for @MLGPUComputeDevice@.
module ObjC.CoreML.MLGPUComputeDevice
  ( MLGPUComputeDevice
  , IsMLGPUComputeDevice(..)
  , init_
  , new
  , metalDevice
  , initSelector
  , newSelector
  , metalDeviceSelector


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
init_ :: IsMLGPUComputeDevice mlgpuComputeDevice => mlgpuComputeDevice -> IO (Id MLGPUComputeDevice)
init_ mlgpuComputeDevice  =
    sendMsg mlgpuComputeDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLGPUComputeDevice)
new  =
  do
    cls' <- getRequiredClass "MLGPUComputeDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The underlying metal device.
--
-- ObjC selector: @- metalDevice@
metalDevice :: IsMLGPUComputeDevice mlgpuComputeDevice => mlgpuComputeDevice -> IO RawId
metalDevice mlgpuComputeDevice  =
    fmap (RawId . castPtr) $ sendMsg mlgpuComputeDevice (mkSelector "metalDevice") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @metalDevice@
metalDeviceSelector :: Selector
metalDeviceSelector = mkSelector "metalDevice"

