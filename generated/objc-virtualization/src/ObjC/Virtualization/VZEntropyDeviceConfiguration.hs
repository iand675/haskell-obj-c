{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an entropy device configuration.
--
-- VZEntropyDeviceConfiguration should not be instantiated directly.    The subclass VZVirtioEntropyDeviceConfiguration should be used instead.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- Generated bindings for @VZEntropyDeviceConfiguration@.
module ObjC.Virtualization.VZEntropyDeviceConfiguration
  ( VZEntropyDeviceConfiguration
  , IsVZEntropyDeviceConfiguration(..)
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
new :: IO (Id VZEntropyDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZEntropyDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZEntropyDeviceConfiguration vzEntropyDeviceConfiguration => vzEntropyDeviceConfiguration -> IO (Id VZEntropyDeviceConfiguration)
init_ vzEntropyDeviceConfiguration  =
  sendMsg vzEntropyDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

