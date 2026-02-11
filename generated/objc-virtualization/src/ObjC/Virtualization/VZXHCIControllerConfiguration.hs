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
init_ :: IsVZXHCIControllerConfiguration vzxhciControllerConfiguration => vzxhciControllerConfiguration -> IO (Id VZXHCIControllerConfiguration)
init_ vzxhciControllerConfiguration  =
  sendMsg vzxhciControllerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

