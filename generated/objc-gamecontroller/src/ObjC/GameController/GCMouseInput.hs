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
  , mouseMovedHandlerSelector
  , setMouseMovedHandlerSelector
  , scrollSelector
  , leftButtonSelector
  , rightButtonSelector
  , middleButtonSelector
  , auxiliaryButtonsSelector


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

-- | @- mouseMovedHandler@
mouseMovedHandler :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Ptr ())
mouseMovedHandler gcMouseInput  =
  fmap castPtr $ sendMsg gcMouseInput (mkSelector "mouseMovedHandler") (retPtr retVoid) []

-- | @- setMouseMovedHandler:@
setMouseMovedHandler :: IsGCMouseInput gcMouseInput => gcMouseInput -> Ptr () -> IO ()
setMouseMovedHandler gcMouseInput  value =
  sendMsg gcMouseInput (mkSelector "setMouseMovedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Scroll is a dpad with undefined range.
--
-- ObjC selector: @- scroll@
scroll :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCDeviceCursor)
scroll gcMouseInput  =
  sendMsg gcMouseInput (mkSelector "scroll") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Mouse buttons that can be used only as digital inputs
--
-- ObjC selector: @- leftButton@
leftButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
leftButton gcMouseInput  =
  sendMsg gcMouseInput (mkSelector "leftButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightButton@
rightButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
rightButton gcMouseInput  =
  sendMsg gcMouseInput (mkSelector "rightButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- middleButton@
middleButton :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id GCControllerButtonInput)
middleButton gcMouseInput  =
  sendMsg gcMouseInput (mkSelector "middleButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- auxiliaryButtons@
auxiliaryButtons :: IsGCMouseInput gcMouseInput => gcMouseInput -> IO (Id NSArray)
auxiliaryButtons gcMouseInput  =
  sendMsg gcMouseInput (mkSelector "auxiliaryButtons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mouseMovedHandler@
mouseMovedHandlerSelector :: Selector
mouseMovedHandlerSelector = mkSelector "mouseMovedHandler"

-- | @Selector@ for @setMouseMovedHandler:@
setMouseMovedHandlerSelector :: Selector
setMouseMovedHandlerSelector = mkSelector "setMouseMovedHandler:"

-- | @Selector@ for @scroll@
scrollSelector :: Selector
scrollSelector = mkSelector "scroll"

-- | @Selector@ for @leftButton@
leftButtonSelector :: Selector
leftButtonSelector = mkSelector "leftButton"

-- | @Selector@ for @rightButton@
rightButtonSelector :: Selector
rightButtonSelector = mkSelector "rightButton"

-- | @Selector@ for @middleButton@
middleButtonSelector :: Selector
middleButtonSelector = mkSelector "middleButton"

-- | @Selector@ for @auxiliaryButtons@
auxiliaryButtonsSelector :: Selector
auxiliaryButtonsSelector = mkSelector "auxiliaryButtons"

