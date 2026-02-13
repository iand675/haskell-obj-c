{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterActivePresetChangeEvent@.
module ObjC.Matter.MTRThermostatClusterActivePresetChangeEvent
  ( MTRThermostatClusterActivePresetChangeEvent
  , IsMTRThermostatClusterActivePresetChangeEvent(..)
  , previousPresetHandle
  , setPreviousPresetHandle
  , currentPresetHandle
  , setCurrentPresetHandle
  , currentPresetHandleSelector
  , previousPresetHandleSelector
  , setCurrentPresetHandleSelector
  , setPreviousPresetHandleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousPresetHandle@
previousPresetHandle :: IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent => mtrThermostatClusterActivePresetChangeEvent -> IO (Id NSData)
previousPresetHandle mtrThermostatClusterActivePresetChangeEvent =
  sendMessage mtrThermostatClusterActivePresetChangeEvent previousPresetHandleSelector

-- | @- setPreviousPresetHandle:@
setPreviousPresetHandle :: (IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent, IsNSData value) => mtrThermostatClusterActivePresetChangeEvent -> value -> IO ()
setPreviousPresetHandle mtrThermostatClusterActivePresetChangeEvent value =
  sendMessage mtrThermostatClusterActivePresetChangeEvent setPreviousPresetHandleSelector (toNSData value)

-- | @- currentPresetHandle@
currentPresetHandle :: IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent => mtrThermostatClusterActivePresetChangeEvent -> IO (Id NSData)
currentPresetHandle mtrThermostatClusterActivePresetChangeEvent =
  sendMessage mtrThermostatClusterActivePresetChangeEvent currentPresetHandleSelector

-- | @- setCurrentPresetHandle:@
setCurrentPresetHandle :: (IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent, IsNSData value) => mtrThermostatClusterActivePresetChangeEvent -> value -> IO ()
setCurrentPresetHandle mtrThermostatClusterActivePresetChangeEvent value =
  sendMessage mtrThermostatClusterActivePresetChangeEvent setCurrentPresetHandleSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPresetHandle@
previousPresetHandleSelector :: Selector '[] (Id NSData)
previousPresetHandleSelector = mkSelector "previousPresetHandle"

-- | @Selector@ for @setPreviousPresetHandle:@
setPreviousPresetHandleSelector :: Selector '[Id NSData] ()
setPreviousPresetHandleSelector = mkSelector "setPreviousPresetHandle:"

-- | @Selector@ for @currentPresetHandle@
currentPresetHandleSelector :: Selector '[] (Id NSData)
currentPresetHandleSelector = mkSelector "currentPresetHandle"

-- | @Selector@ for @setCurrentPresetHandle:@
setCurrentPresetHandleSelector :: Selector '[Id NSData] ()
setCurrentPresetHandleSelector = mkSelector "setCurrentPresetHandle:"

