{-# LANGUAGE DataKinds #-}
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
new :: IO (Id VZSocketDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZSocketDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZSocketDeviceConfiguration vzSocketDeviceConfiguration => vzSocketDeviceConfiguration -> IO (Id VZSocketDeviceConfiguration)
init_ vzSocketDeviceConfiguration =
  sendOwnedMessage vzSocketDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZSocketDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSocketDeviceConfiguration)
initSelector = mkSelector "init"

