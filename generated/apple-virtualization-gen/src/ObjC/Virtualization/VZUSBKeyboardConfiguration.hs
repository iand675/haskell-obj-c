{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a USB keyboard.
--
-- This device can be used by VZVirtualMachineView to send key events to the virtual machine.
--
-- Generated bindings for @VZUSBKeyboardConfiguration@.
module ObjC.Virtualization.VZUSBKeyboardConfiguration
  ( VZUSBKeyboardConfiguration
  , IsVZUSBKeyboardConfiguration(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZUSBKeyboardConfiguration vzusbKeyboardConfiguration => vzusbKeyboardConfiguration -> IO (Id VZUSBKeyboardConfiguration)
init_ vzusbKeyboardConfiguration =
  sendOwnedMessage vzusbKeyboardConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZUSBKeyboardConfiguration)
initSelector = mkSelector "init"

