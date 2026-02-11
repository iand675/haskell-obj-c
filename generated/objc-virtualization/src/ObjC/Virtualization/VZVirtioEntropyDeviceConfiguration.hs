{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Entropy Device
--
-- The device exposes a source of entropy for the guest's random number generator.
--
-- Generated bindings for @VZVirtioEntropyDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioEntropyDeviceConfiguration
  ( VZVirtioEntropyDeviceConfiguration
  , IsVZVirtioEntropyDeviceConfiguration(..)
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
init_ :: IsVZVirtioEntropyDeviceConfiguration vzVirtioEntropyDeviceConfiguration => vzVirtioEntropyDeviceConfiguration -> IO (Id VZVirtioEntropyDeviceConfiguration)
init_ vzVirtioEntropyDeviceConfiguration  =
  sendMsg vzVirtioEntropyDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

