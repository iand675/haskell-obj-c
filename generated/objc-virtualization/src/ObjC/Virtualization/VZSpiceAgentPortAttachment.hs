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
  , sharesClipboardSelector
  , setSharesClipboardSelector
  , spiceAgentPortNameSelector


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
init_ :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> IO (Id VZSpiceAgentPortAttachment)
init_ vzSpiceAgentPortAttachment  =
  sendMsg vzSpiceAgentPortAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Enable the Spice agent clipboard sharing capability.
--
-- If enabled, the clipboard capability will be advertised to the Spice guest agent. Copy and paste events    will be shared between the host and the virtual machine.
--
-- This property is enabled by default.
--
-- ObjC selector: @- sharesClipboard@
sharesClipboard :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> IO Bool
sharesClipboard vzSpiceAgentPortAttachment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzSpiceAgentPortAttachment (mkSelector "sharesClipboard") retCULong []

-- | Enable the Spice agent clipboard sharing capability.
--
-- If enabled, the clipboard capability will be advertised to the Spice guest agent. Copy and paste events    will be shared between the host and the virtual machine.
--
-- This property is enabled by default.
--
-- ObjC selector: @- setSharesClipboard:@
setSharesClipboard :: IsVZSpiceAgentPortAttachment vzSpiceAgentPortAttachment => vzSpiceAgentPortAttachment -> Bool -> IO ()
setSharesClipboard vzSpiceAgentPortAttachment  value =
  sendMsg vzSpiceAgentPortAttachment (mkSelector "setSharesClipboard:") retVoid [argCULong (if value then 1 else 0)]

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
    sendClassMsg cls' (mkSelector "spiceAgentPortName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sharesClipboard@
sharesClipboardSelector :: Selector
sharesClipboardSelector = mkSelector "sharesClipboard"

-- | @Selector@ for @setSharesClipboard:@
setSharesClipboardSelector :: Selector
setSharesClipboardSelector = mkSelector "setSharesClipboard:"

-- | @Selector@ for @spiceAgentPortName@
spiceAgentPortNameSelector :: Selector
spiceAgentPortNameSelector = mkSelector "spiceAgentPortName"

