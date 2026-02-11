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
  , buttonForKeyCodeSelector
  , keyChangedHandlerSelector
  , setKeyChangedHandlerSelector
  , anyKeyPressedSelector


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
buttonForKeyCode gcKeyboardInput  code =
  sendMsg gcKeyboardInput (mkSelector "buttonForKeyCode:") (retPtr retVoid) [argCLong (fromIntegral code)] >>= retainedObject . castPtr

-- | @- keyChangedHandler@
keyChangedHandler :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> IO (Ptr ())
keyChangedHandler gcKeyboardInput  =
  fmap castPtr $ sendMsg gcKeyboardInput (mkSelector "keyChangedHandler") (retPtr retVoid) []

-- | @- setKeyChangedHandler:@
setKeyChangedHandler :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> Ptr () -> IO ()
setKeyChangedHandler gcKeyboardInput  value =
  sendMsg gcKeyboardInput (mkSelector "setKeyChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Before querying any key for a value it might be useful to check if any key is actually pressed
--
-- ObjC selector: @- anyKeyPressed@
anyKeyPressed :: IsGCKeyboardInput gcKeyboardInput => gcKeyboardInput -> IO Bool
anyKeyPressed gcKeyboardInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcKeyboardInput (mkSelector "anyKeyPressed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonForKeyCode:@
buttonForKeyCodeSelector :: Selector
buttonForKeyCodeSelector = mkSelector "buttonForKeyCode:"

-- | @Selector@ for @keyChangedHandler@
keyChangedHandlerSelector :: Selector
keyChangedHandlerSelector = mkSelector "keyChangedHandler"

-- | @Selector@ for @setKeyChangedHandler:@
setKeyChangedHandlerSelector :: Selector
setKeyChangedHandlerSelector = mkSelector "setKeyChangedHandler:"

-- | @Selector@ for @anyKeyPressed@
anyKeyPressedSelector :: Selector
anyKeyPressedSelector = mkSelector "anyKeyPressed"

