{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The GCDualShockGamepad profile represents any supported DualShock 4 controller.
--
-- See: GCExtendedGamepad
--
-- See: GCMotion
--
-- Generated bindings for @GCDualShockGamepad@.
module ObjC.GameController.GCDualShockGamepad
  ( GCDualShockGamepad
  , IsGCDualShockGamepad(..)
  , touchpadButton
  , touchpadPrimary
  , touchpadSecondary
  , touchpadButtonSelector
  , touchpadPrimarySelector
  , touchpadSecondarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | DualShock controllers have a touchpad with a button and two-finger tracking.
--
-- ObjC selector: @- touchpadButton@
touchpadButton :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerButtonInput)
touchpadButton gcDualShockGamepad =
  sendMessage gcDualShockGamepad touchpadButtonSelector

-- | @- touchpadPrimary@
touchpadPrimary :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerDirectionPad)
touchpadPrimary gcDualShockGamepad =
  sendMessage gcDualShockGamepad touchpadPrimarySelector

-- | @- touchpadSecondary@
touchpadSecondary :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerDirectionPad)
touchpadSecondary gcDualShockGamepad =
  sendMessage gcDualShockGamepad touchpadSecondarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @touchpadButton@
touchpadButtonSelector :: Selector '[] (Id GCControllerButtonInput)
touchpadButtonSelector = mkSelector "touchpadButton"

-- | @Selector@ for @touchpadPrimary@
touchpadPrimarySelector :: Selector '[] (Id GCControllerDirectionPad)
touchpadPrimarySelector = mkSelector "touchpadPrimary"

-- | @Selector@ for @touchpadSecondary@
touchpadSecondarySelector :: Selector '[] (Id GCControllerDirectionPad)
touchpadSecondarySelector = mkSelector "touchpadSecondary"

