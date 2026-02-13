{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mouse profile that represent a physical mouse object with two axis cursor, two axis scroll, left button, optional right and middle buttons and optional set of auxiliary buttons.
--
-- It only provides information about raw mouse movement deltas. For the valid cursor position at given point in time, use UIHoverGestureRecognizer and NSEvent.mouseLocation.
--
-- Generated bindings for @GCMouseInput@.
module ObjC.GameController.GCMouseInput
  ( GCMouseInput
  , IsGCMouseInput(..)
  , mouseMovedHandler
  , setMouseMovedHandler
  , scroll
  , leftButton
  , rightButton
  , middleButton
  , auxiliaryButtons
  , auxiliaryButtonsSelector
  , leftButtonSelector
  , middleButtonSelector
  , mouseMovedHandlerSelector
  , rightButtonSelector
  , scrollSelector
  , setMouseMovedHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mouseMovedHandler@
mouseMovedHandler :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Ptr ())
mouseMovedHandler gcMouseInput =
  sendMessage gcMouseInput mouseMovedHandlerSelector

-- | @- setMouseMovedHandler:@
setMouseMovedHandler :: IsGCMouseInput gcMouseInput => gcMouseInput -> Ptr () -> IO ()
setMouseMovedHandler gcMouseInput value =
  sendMessage gcMouseInput setMouseMovedHandlerSelector value

-- | Scroll is a dpad with undefined range.
--
-- ObjC selector: @- scroll@
scroll :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCDeviceCursor)
scroll gcMouseInput =
  sendMessage gcMouseInput scrollSelector

-- | Mouse buttons that can be used only as digital inputs
--
-- ObjC selector: @- leftButton@
leftButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
leftButton gcMouseInput =
  sendMessage gcMouseInput leftButtonSelector

-- | @- rightButton@
rightButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
rightButton gcMouseInput =
  sendMessage gcMouseInput rightButtonSelector

-- | @- middleButton@
middleButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
middleButton gcMouseInput =
  sendMessage gcMouseInput middleButtonSelector

-- | @- auxiliaryButtons@
auxiliaryButtons :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id NSArray)
auxiliaryButtons gcMouseInput =
  sendMessage gcMouseInput auxiliaryButtonsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mouseMovedHandler@
mouseMovedHandlerSelector :: Selector '[] (Ptr ())
mouseMovedHandlerSelector = mkSelector "mouseMovedHandler"

-- | @Selector@ for @setMouseMovedHandler:@
setMouseMovedHandlerSelector :: Selector '[Ptr ()] ()
setMouseMovedHandlerSelector = mkSelector "setMouseMovedHandler:"

-- | @Selector@ for @scroll@
scrollSelector :: Selector '[] (Id GCDeviceCursor)
scrollSelector = mkSelector "scroll"

-- | @Selector@ for @leftButton@
leftButtonSelector :: Selector '[] (Id GCControllerButtonInput)
leftButtonSelector = mkSelector "leftButton"

-- | @Selector@ for @rightButton@
rightButtonSelector :: Selector '[] (Id GCControllerButtonInput)
rightButtonSelector = mkSelector "rightButton"

-- | @Selector@ for @middleButton@
middleButtonSelector :: Selector '[] (Id GCControllerButtonInput)
middleButtonSelector = mkSelector "middleButton"

-- | @Selector@ for @auxiliaryButtons@
auxiliaryButtonsSelector :: Selector '[] (Id NSArray)
auxiliaryButtonsSelector = mkSelector "auxiliaryButtons"

