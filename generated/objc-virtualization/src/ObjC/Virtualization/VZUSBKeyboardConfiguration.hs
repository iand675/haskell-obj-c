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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZUSBKeyboardConfiguration vzusbKeyboardConfiguration => vzusbKeyboardConfiguration -> IO (Id VZUSBKeyboardConfiguration)
init_ vzusbKeyboardConfiguration  =
  sendMsg vzusbKeyboardConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

