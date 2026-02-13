{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Keyboard profile. Contains the current state of buttons specified in GCKeyCodes.h.
--
-- GCKeyboardInput is designed primarly for input polling. For the best text input experience, UIKit/AppKit usage is recommended.
--
-- Generated bindings for @GCKeyboardInput@.
module ObjC.GameController.GCKeyboardInput
  ( GCKeyboardInput
  , IsGCKeyboardInput(..)
  , buttonForKeyCode
  , keyChangedHandler
  , setKeyChangedHandler
  , anyKeyPressed
  , anyKeyPressedSelector
  , buttonForKeyCodeSelector
  , keyChangedHandlerSelector
  , setKeyChangedHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Alongside general subscript notation of GCPhysicalInputProfile keys can be accessed using this method.
--
-- [keyboard buttonForKey:GCKeyCode.UpArrow] == keyboard[GCKeyUpArrow]
--
-- @code@ — is a low level key code that can be used for accessing a keyboard button.
--
-- Note: Full list of supported key constants can be found in GCKeyCodes.h and GCKeyNames.h
--
-- ObjC selector: @- buttonForKeyCode:@
buttonForKeyCode :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> CLong -> IO (Id GCControllerButtonInput)
buttonForKeyCode gcKeyboardInput code =
  sendMessage gcKeyboardInput buttonForKeyCodeSelector code

-- | @- keyChangedHandler@
keyChangedHandler :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> IO (Ptr ())
keyChangedHandler gcKeyboardInput =
  sendMessage gcKeyboardInput keyChangedHandlerSelector

-- | @- setKeyChangedHandler:@
setKeyChangedHandler :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> Ptr () -> IO ()
setKeyChangedHandler gcKeyboardInput value =
  sendMessage gcKeyboardInput setKeyChangedHandlerSelector value

-- | Before querying any key for a value it might be useful to check if any key is actually pressed
--
-- ObjC selector: @- anyKeyPressed@
anyKeyPressed :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> IO Bool
anyKeyPressed gcKeyboardInput =
  sendMessage gcKeyboardInput anyKeyPressedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonForKeyCode:@
buttonForKeyCodeSelector :: Selector '[CLong] (Id GCControllerButtonInput)
buttonForKeyCodeSelector = mkSelector "buttonForKeyCode:"

-- | @Selector@ for @keyChangedHandler@
keyChangedHandlerSelector :: Selector '[] (Ptr ())
keyChangedHandlerSelector = mkSelector "keyChangedHandler"

-- | @Selector@ for @setKeyChangedHandler:@
setKeyChangedHandlerSelector :: Selector '[Ptr ()] ()
setKeyChangedHandlerSelector = mkSelector "setKeyChangedHandler:"

-- | @Selector@ for @anyKeyPressed@
anyKeyPressedSelector :: Selector '[] Bool
anyKeyPressedSelector = mkSelector "anyKeyPressed"

