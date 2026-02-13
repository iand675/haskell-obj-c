{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for the USB XHCI controller.
--
-- This configuration creates a USB XHCI controller device for the guest.
--
-- Generated bindings for @VZXHCIControllerConfiguration@.
module ObjC.Virtualization.VZXHCIControllerConfiguration
  ( VZXHCIControllerConfiguration
  , IsVZXHCIControllerConfiguration(..)
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
init_ :: IsVZXHCIControllerConfiguration vzxhciControllerConfiguration => vzxhciControllerConfiguration -> IO (Id VZXHCIControllerConfiguration)
init_ vzxhciControllerConfiguration =
  sendOwnedMessage vzxhciControllerConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZXHCIControllerConfiguration)
initSelector = mkSelector "init"

