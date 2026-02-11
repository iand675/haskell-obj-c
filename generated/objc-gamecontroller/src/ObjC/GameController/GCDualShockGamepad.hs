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

-- | DualShock controllers have a touchpad with a button and two-finger tracking.
--
-- ObjC selector: @- touchpadButton@
touchpadButton :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerButtonInput)
touchpadButton gcDualShockGamepad  =
  sendMsg gcDualShockGamepad (mkSelector "touchpadButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchpadPrimary@
touchpadPrimary :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerDirectionPad)
touchpadPrimary gcDualShockGamepad  =
  sendMsg gcDualShockGamepad (mkSelector "touchpadPrimary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchpadSecondary@
touchpadSecondary :: IsGCDualShockGamepad gcDualShockGamepad => gcDualShockGamepad -> IO (Id GCControllerDirectionPad)
touchpadSecondary gcDualShockGamepad  =
  sendMsg gcDualShockGamepad (mkSelector "touchpadSecondary") (retPtr retVoid) [] >>= retainedObject . castPtr

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

