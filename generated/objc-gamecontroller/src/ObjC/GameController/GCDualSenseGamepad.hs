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
  , touchpadButtonSelector
  , touchpadPrimarySelector
  , touchpadSecondarySelector
  , leftTriggerSelector
  , rightTriggerSelector


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
import ObjC.Foundation.Internal.Classes

-- | DualSense controllers have a touchpad with a button and two-finger tracking.
--
-- ObjC selector: @- touchpadButton@
touchpadButton :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerButtonInput)
touchpadButton gcDualSenseGamepad  =
  sendMsg gcDualSenseGamepad (mkSelector "touchpadButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchpadPrimary@
touchpadPrimary :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerDirectionPad)
touchpadPrimary gcDualSenseGamepad  =
  sendMsg gcDualSenseGamepad (mkSelector "touchpadPrimary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchpadSecondary@
touchpadSecondary :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCControllerDirectionPad)
touchpadSecondary gcDualSenseGamepad  =
  sendMsg gcDualSenseGamepad (mkSelector "touchpadSecondary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- The DualSense has adaptive triggers, allowing you to specify a dynamic resistance force that is applied when pulling the trigger. This can, for example, be used to emulate the feeling of pulling back a bow string, firing a weapon, or pulling a lever.
--
-- ObjC selector: @- leftTrigger@
leftTrigger :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCDualSenseAdaptiveTrigger)
leftTrigger gcDualSenseGamepad  =
  sendMsg gcDualSenseGamepad (mkSelector "leftTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightTrigger@
rightTrigger :: IsGCDualSenseGamepad gcDualSenseGamepad => gcDualSenseGamepad -> IO (Id GCDualSenseAdaptiveTrigger)
rightTrigger gcDualSenseGamepad  =
  sendMsg gcDualSenseGamepad (mkSelector "rightTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @touchpadButton@
touchpadButtonSelector :: Selector
touchpadButtonSelector = mkSelector "touchpadButton"

-- | @Selector@ for @touchpadPrimary@
touchpadPrimarySelector :: Selector
touchpadPrimarySelector = mkSelector "touchpadPrimary"

-- | @Selector@ for @touchpadSecondary@
touchpadSecondarySelector :: Selector
touchpadSecondarySelector = mkSelector "touchpadSecondary"

-- | @Selector@ for @leftTrigger@
leftTriggerSelector :: Selector
leftTriggerSelector = mkSelector "leftTrigger"

-- | @Selector@ for @rightTrigger@
rightTriggerSelector :: Selector
rightTriggerSelector = mkSelector "rightTrigger"

