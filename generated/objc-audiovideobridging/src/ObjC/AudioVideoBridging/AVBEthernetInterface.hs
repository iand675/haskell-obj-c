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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

