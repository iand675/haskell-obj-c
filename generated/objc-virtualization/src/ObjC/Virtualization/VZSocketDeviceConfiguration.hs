{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a socket device configuration.
--
-- VZSocketDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioSocketDeviceConfiguration should be used instead.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- Generated bindings for @VZSocketDeviceConfiguration@.
module ObjC.Virtualization.VZSocketDeviceConfiguration
  ( VZSocketDeviceConfiguration
  , IsVZSocketDeviceConfiguration(..)
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
new :: IO (Id VZSocketDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZSocketDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZSocketDeviceConfiguration vzSocketDeviceConfiguration => vzSocketDeviceConfiguration -> IO (Id VZSocketDeviceConfiguration)
init_ vzSocketDeviceConfiguration  =
  sendMsg vzSocketDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

