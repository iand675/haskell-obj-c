{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A controller battery is an abstract representation of the battery level and battery status of a GCController instance.
--
-- Generated bindings for @GCDeviceBattery@.
module ObjC.GameController.GCDeviceBattery
  ( GCDeviceBattery
  , IsGCDeviceBattery(..)
  , init_
  , batteryLevel
  , batteryState
  , batteryLevelSelector
  , batteryStateSelector
  , initSelector

  -- * Enum types
  , GCDeviceBatteryState(GCDeviceBatteryState)
  , pattern GCDeviceBatteryStateUnknown
  , pattern GCDeviceBatteryStateDischarging
  , pattern GCDeviceBatteryStateCharging
  , pattern GCDeviceBatteryStateFull

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO (Id GCDeviceBattery)
init_ gcDeviceBattery =
  sendOwnedMessage gcDeviceBattery initSelector

-- | This is the battery level for controller. Battery level ranges from 0.0 (fully discharged) to 1.0 (100% charged) and defaults to 0
--
-- ObjC selector: @- batteryLevel@
batteryLevel :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO CFloat
batteryLevel gcDeviceBattery =
  sendMessage gcDeviceBattery batteryLevelSelector

-- | A battery state for controller, defaults to GCControllerBatteryStateUnknown
--
-- Note: This property might be useful if you display the information about currently connected controller for player's convenience
--
-- ObjC selector: @- batteryState@
batteryState :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO GCDeviceBatteryState
batteryState gcDeviceBattery =
  sendMessage gcDeviceBattery batteryStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCDeviceBattery)
initSelector = mkSelector "init"

-- | @Selector@ for @batteryLevel@
batteryLevelSelector :: Selector '[] CFloat
batteryLevelSelector = mkSelector "batteryLevel"

-- | @Selector@ for @batteryState@
batteryStateSelector :: Selector '[] GCDeviceBatteryState
batteryStateSelector = mkSelector "batteryState"

