{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GCKeyboard is available to an application that links to GameController.framework There are 2 ways to access keyboard paired to the system: 1: Querying for the coalescedKeyboard using [GCKeyboard coalescedKeyboard] 2: Registering for Connection/Disconnection notifications from NSNotificationCenter
--
-- Note: All connected keyboards are coalesced into one keyboard object, so notification about connection/disconnection will only be delivered once.
--
-- Generated bindings for @GCKeyboard@.
module ObjC.GameController.GCKeyboard
  ( GCKeyboard
  , IsGCKeyboard(..)
  , keyboardInput
  , coalescedKeyboard
  , coalescedKeyboardSelector
  , keyboardInputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unlike GCController GCKeyboard only has one input profile.
--
-- This profile allows you to query buttons and button state
--
-- ObjC selector: @- keyboardInput@
keyboardInput :: IsGCKeyboard gcKeyboard => gcKeyboard -> IO (Id GCKeyboardInput)
keyboardInput gcKeyboard =
  sendMessage gcKeyboard keyboardInputSelector

-- | Keyboard object that represents all keyboards connected to the device Should be used to query key states every time input needs to be handled
--
-- ObjC selector: @+ coalescedKeyboard@
coalescedKeyboard :: IO (Id GCKeyboard)
coalescedKeyboard  =
  do
    cls' <- getRequiredClass "GCKeyboard"
    sendClassMessage cls' coalescedKeyboardSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyboardInput@
keyboardInputSelector :: Selector '[] (Id GCKeyboardInput)
keyboardInputSelector = mkSelector "keyboardInput"

-- | @Selector@ for @coalescedKeyboard@
coalescedKeyboardSelector :: Selector '[] (Id GCKeyboard)
coalescedKeyboardSelector = mkSelector "coalescedKeyboard"

