{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VZSpiceAgentPortAttachment@.
module ObjC.Virtualization.VZSpiceAgentPortAttachment
  ( VZSpiceAgentPortAttachment
  , IsVZSpiceAgentPortAttachment(..)
  , init_
  , sharesClipboard
  , setSharesClipboard
  , spiceAgentPortName
  , initSelector
  , setSharesClipboardSelector
  , sharesClipboardSelector
  , spiceAgentPortNameSelector


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
init_ :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> IO (Id VZSpiceAgentPortAttachment)
init_ vzSpiceAgentPortAttachment =
  sendOwnedMessage vzSpiceAgentPortAttachment initSelector

-- | Enable the Spice agent clipboard sharing capability.
--
-- If enabled, the clipboard capability will be advertised to the Spice guest agent. Copy and paste events    will be shared between the host and the virtual machine.
--
-- This property is enabled by default.
--
-- ObjC selector: @- sharesClipboard@
sharesClipboard :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> IO Bool
sharesClipboard vzSpiceAgentPortAttachment =
  sendMessage vzSpiceAgentPortAttachment sharesClipboardSelector

-- | Enable the Spice agent clipboard sharing capability.
--
-- If enabled, the clipboard capability will be advertised to the Spice guest agent. Copy and paste events    will be shared between the host and the virtual machine.
--
-- This property is enabled by default.
--
-- ObjC selector: @- setSharesClipboard:@
setSharesClipboard :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> Bool -> IO ()
setSharesClipboard vzSpiceAgentPortAttachment value =
  sendMessage vzSpiceAgentPortAttachment setSharesClipboardSelector value

-- | The Spice agent port name.
--
-- A console port configured with this name will spawn a Spice guest agent if supported by the guest.
--
-- VZConsolePortConfiguration.attachment must be set to VZSpiceAgentPortAttachment.    VZVirtioConsolePortConfiguration.isConsole must remain false on a Spice agent port.
--
-- ObjC selector: @+ spiceAgentPortName@
spiceAgentPortName :: IO (Id NSString)
spiceAgentPortName  =
  do
    cls' <- getRequiredClass "VZSpiceAgentPortAttachment"
    sendClassMessage cls' spiceAgentPortNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSpiceAgentPortAttachment)
initSelector = mkSelector "init"

-- | @Selector@ for @sharesClipboard@
sharesClipboardSelector :: Selector '[] Bool
sharesClipboardSelector = mkSelector "sharesClipboard"

-- | @Selector@ for @setSharesClipboard:@
setSharesClipboardSelector :: Selector '[Bool] ()
setSharesClipboardSelector = mkSelector "setSharesClipboard:"

-- | @Selector@ for @spiceAgentPortName@
spiceAgentPortNameSelector :: Selector '[] (Id NSString)
spiceAgentPortNameSelector = mkSelector "spiceAgentPortName"

