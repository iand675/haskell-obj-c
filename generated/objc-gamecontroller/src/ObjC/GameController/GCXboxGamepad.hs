{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The GCXboxGamepad profile represents any supported Xbox controller.
--
-- See: GCExtendedGamepad
--
-- Generated bindings for @GCXboxGamepad@.
module ObjC.GameController.GCXboxGamepad
  ( GCXboxGamepad
  , IsGCXboxGamepad(..)
  , paddleButton1
  , paddleButton2
  , paddleButton3
  , paddleButton4
  , buttonShare
  , paddleButton1Selector
  , paddleButton2Selector
  , paddleButton3Selector
  , paddleButton4Selector
  , buttonShareSelector


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

-- | Some Xbox controller variants can support up to four additional buttons.
--
-- The standard Bluetooth-enabled Xbox Wireless Controller does not have paddle buttons
--
-- The Xbox Elite Wireless Controller has four extra digital buttons.
--
-- Note: The four extra digital buttons on the Xbox Elite Wireless Controller are only directly addressable when the controller    is on its default mapping profile. Otherwise, the paddle buttons are directly bound to other inputs on the controller.
--
-- ObjC selector: @- paddleButton1@
paddleButton1 :: IsGCXboxGamepad gcXboxGamepad => gcXboxGamepad -> IO (Id GCControllerButtonInput)
paddleButton1 gcXboxGamepad  =
  sendMsg gcXboxGamepad (mkSelector "paddleButton1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paddleButton2@
paddleButton2 :: IsGCXboxGamepad gcXboxGamepad => gcXboxGamepad -> IO (Id GCControllerButtonInput)
paddleButton2 gcXboxGamepad  =
  sendMsg gcXboxGamepad (mkSelector "paddleButton2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paddleButton3@
paddleButton3 :: IsGCXboxGamepad gcXboxGamepad => gcXboxGamepad -> IO (Id GCControllerButtonInput)
paddleButton3 gcXboxGamepad  =
  sendMsg gcXboxGamepad (mkSelector "paddleButton3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paddleButton4@
paddleButton4 :: IsGCXboxGamepad gcXboxGamepad => gcXboxGamepad -> IO (Id GCControllerButtonInput)
paddleButton4 gcXboxGamepad  =
  sendMsg gcXboxGamepad (mkSelector "paddleButton4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Some Xbox controller variants feature a Share button.
--
-- The Bluetooth-enabled Xbox Wireless Controller introduced with the Xbox Series X and Xbox Series S in 2020
--
-- has a Share button.
--
-- Note: The Share button is reserved by the system for screenshot and video recording gestures. If you wish to disable these    gestures in your app and take control of the Share button, set buttonShare.preferredSystemGestureState to    GCSystemGestureStateDisabled.
--
-- ObjC selector: @- buttonShare@
buttonShare :: IsGCXboxGamepad gcXboxGamepad => gcXboxGamepad -> IO (Id GCControllerButtonInput)
buttonShare gcXboxGamepad  =
  sendMsg gcXboxGamepad (mkSelector "buttonShare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paddleButton1@
paddleButton1Selector :: Selector
paddleButton1Selector = mkSelector "paddleButton1"

-- | @Selector@ for @paddleButton2@
paddleButton2Selector :: Selector
paddleButton2Selector = mkSelector "paddleButton2"

-- | @Selector@ for @paddleButton3@
paddleButton3Selector :: Selector
paddleButton3Selector = mkSelector "paddleButton3"

-- | @Selector@ for @paddleButton4@
paddleButton4Selector :: Selector
paddleButton4Selector = mkSelector "paddleButton4"

-- | @Selector@ for @buttonShare@
buttonShareSelector :: Selector
buttonShareSelector = mkSelector "buttonShare"

