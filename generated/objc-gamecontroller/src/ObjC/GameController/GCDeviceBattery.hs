{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , batteryLevelSelector
  , batteryStateSelector

  -- * Enum types
  , GCDeviceBatteryState(GCDeviceBatteryState)
  , pattern GCDeviceBatteryStateUnknown
  , pattern GCDeviceBatteryStateDischarging
  , pattern GCDeviceBatteryStateCharging
  , pattern GCDeviceBatteryStateFull

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

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO (Id GCDeviceBattery)
init_ gcDeviceBattery  =
  sendMsg gcDeviceBattery (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | This is the battery level for controller. Battery level ranges from 0.0 (fully discharged) to 1.0 (100% charged) and defaults to 0
--
-- ObjC selector: @- batteryLevel@
batteryLevel :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO CFloat
batteryLevel gcDeviceBattery  =
  sendMsg gcDeviceBattery (mkSelector "batteryLevel") retCFloat []

-- | A battery state for controller, defaults to GCControllerBatteryStateUnknown
--
-- Note: This property might be useful if you display the information about currently connected controller for player's convenience
--
-- ObjC selector: @- batteryState@
batteryState :: IsGCDeviceBattery gcDeviceBattery => gcDeviceBattery -> IO GCDeviceBatteryState
batteryState gcDeviceBattery  =
  fmap (coerce :: CLong -> GCDeviceBatteryState) $ sendMsg gcDeviceBattery (mkSelector "batteryState") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @batteryLevel@
batteryLevelSelector :: Selector
batteryLevelSelector = mkSelector "batteryLevel"

-- | @Selector@ for @batteryState@
batteryStateSelector :: Selector
batteryStateSelector = mkSelector "batteryState"

