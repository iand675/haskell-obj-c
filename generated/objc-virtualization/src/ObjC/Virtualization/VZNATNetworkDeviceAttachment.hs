{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Network device attachment using network address translation (NAT) with outside networks.
--
-- Using the NAT attachment type, the host serves as router and performs network address translation for accesses to outside networks.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- Generated bindings for @VZNATNetworkDeviceAttachment@.
module ObjC.Virtualization.VZNATNetworkDeviceAttachment
  ( VZNATNetworkDeviceAttachment
  , IsVZNATNetworkDeviceAttachment(..)
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
init_ :: IsVZNATNetworkDeviceAttachment vznatNetworkDeviceAttachment => vznatNetworkDeviceAttachment -> IO (Id VZNATNetworkDeviceAttachment)
init_ vznatNetworkDeviceAttachment  =
  sendMsg vznatNetworkDeviceAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

