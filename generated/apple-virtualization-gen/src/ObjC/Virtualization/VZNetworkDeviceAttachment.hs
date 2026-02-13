{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZNetworkDeviceAttachment)
new  =
  do
    cls' <- getRequiredClass "VZNetworkDeviceAttachment"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZNetworkDeviceAttachment vzNetworkDeviceAttachment => vzNetworkDeviceAttachment -> IO (Id VZNetworkDeviceAttachment)
init_ vzNetworkDeviceAttachment =
  sendOwnedMessage vzNetworkDeviceAttachment initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZNetworkDeviceAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZNetworkDeviceAttachment)
initSelector = mkSelector "init"

