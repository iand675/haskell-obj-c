{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a network device attachment.
--
-- A network device attachment defines how a virtual network device interfaces with the host system.
--
-- VZNetworkDeviceAttachment should not be instantiated directly. One of its subclasses should be used instead.
--
-- Common attachment types include:    - VZNATNetworkDeviceAttachment    - VZFileHandleNetworkDeviceAttachment
--
-- See: VZBridgedNetworkDeviceAttachment
--
-- See: VZFileHandleNetworkDeviceAttachment
--
-- See: VZNATNetworkDeviceAttachment
--
-- Generated bindings for @VZNetworkDeviceAttachment@.
module ObjC.Virtualization.VZNetworkDeviceAttachment
  ( VZNetworkDeviceAttachment
  , IsVZNetworkDeviceAttachment(..)
  , new
  , init_
  , newSelector
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

-- | @+ new@
new :: IO (Id VZNetworkDeviceAttachment)
new  =
  do
    cls' <- getRequiredClass "VZNetworkDeviceAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZNetworkDeviceAttachment vzNetworkDeviceAttachment => vzNetworkDeviceAttachment -> IO (Id VZNetworkDeviceAttachment)
init_ vzNetworkDeviceAttachment  =
  sendMsg vzNetworkDeviceAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

