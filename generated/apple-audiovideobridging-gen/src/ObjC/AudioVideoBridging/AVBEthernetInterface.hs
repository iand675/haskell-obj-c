{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVBEthernetInterface
--
-- AVBEthernetInterface is a concrete subclass of AVBInterface providing access to the AVB services of the interface.
--
-- AVBEthernetInterface is a concrete subclass of AVBInterface providing access to the AVB services of the interface.				AVBEthernetInterface objects should be created for an IEEE 802.3 ethernet based interface on which AVB functionality 				is being used.
--
-- Generated bindings for @AVBEthernetInterface@.
module ObjC.AudioVideoBridging.AVBEthernetInterface
  ( AVBEthernetInterface
  , IsAVBEthernetInterface(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

