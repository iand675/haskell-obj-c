{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The GCDualSenseGamepad profile represents any supported DualSense controller.
--
-- See: GCExtendedGamepad
--
-- See: GCMotion
--
-- Generated bindings for @GCDualSenseGamepad@.
module ObjC.GameController.GCDualSenseGamepad
  ( GCDualSenseGamepad
  , IsGCDualSenseGamepad(..)
  , touchpadButton
  , touchpadPrimary
  , touchpadSecondary
  , leftTrigger
  , rightTrigger
  , leftTriggerSelector
  , rightTriggerSelector
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

-- | DualSense controllers have a touchpad with a button and two-finger tracking.
--
-- ObjC selector: @- touchpadButton@
touchpadButton :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerButtonInput)
touchpadButton gcDualSenseGamepad =
  sendMessage gcDualSenseGamepad touchpadButtonSelector

-- | @- touchpadPrimary@
touchpadPrimary :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerDirectionPad)
touchpadPrimary gcDualSenseGamepad =
  sendMessage gcDualSenseGamepad touchpadPrimarySelector

-- | @- touchpadSecondary@
touchpadSecondary :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerDirectionPad)
touchpadSecondary gcDualSenseGamepad =
  sendMessage gcDualSenseGamepad touchpadSecondarySelector

-- | Triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- The DualSense has adaptive triggers, allowing you to specify a dynamic resistance force that is applied when pulling the trigger. This can, for example, be used to emulate the feeling of pulling back a bow string, firing a weapon, or pulling a lever.
--
-- ObjC selector: @- leftTrigger@
leftTrigger :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCDualSenseAdaptiveTrigger)
leftTrigger gcDualSenseGamepad =
  sendMessage gcDualSenseGamepad leftTriggerSelector

-- | @- rightTrigger@
rightTrigger :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCDualSenseAdaptiveTrigger)
rightTrigger gcDualSenseGamepad =
  sendMessage gcDualSenseGamepad rightTriggerSelector

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

-- | @Selector@ for @leftTrigger@
leftTriggerSelector :: Selector '[] (Id GCDualSenseAdaptiveTrigger)
leftTriggerSelector = mkSelector "leftTrigger"

-- | @Selector@ for @rightTrigger@
rightTriggerSelector :: Selector '[] (Id GCDualSenseAdaptiveTrigger)
rightTriggerSelector = mkSelector "rightTrigger"

