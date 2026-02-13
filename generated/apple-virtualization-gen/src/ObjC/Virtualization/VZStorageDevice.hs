{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a storage device in a virtual machine.
--
-- VZStorageDevice should not be instantiated directly.    One of its subclasses like VZUSBMassStorageDevice should be used instead.
--
-- See: VZUSBMassStorageDevice
--
-- Generated bindings for @VZStorageDevice@.
module ObjC.Virtualization.VZStorageDevice
  ( VZStorageDevice
  , IsVZStorageDevice(..)
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
new :: IO (Id VZStorageDevice)
new  =
  do
    cls' <- getRequiredClass "VZStorageDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZStorageDevice vzStorageDevice => vzStorageDevice -> IO (Id VZStorageDevice)
init_ vzStorageDevice =
  sendOwnedMessage vzStorageDevice initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZStorageDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZStorageDevice)
initSelector = mkSelector "init"

