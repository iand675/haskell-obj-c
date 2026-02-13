{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a console device configuration.
--
-- VZConsoleDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsoleDeviceConfiguration should be used instead.
--
-- See: VZVirtioConsoleDeviceConfiguration
--
-- Generated bindings for @VZConsoleDeviceConfiguration@.
module ObjC.Virtualization.VZConsoleDeviceConfiguration
  ( VZConsoleDeviceConfiguration
  , IsVZConsoleDeviceConfiguration(..)
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
new :: IO (Id VZConsoleDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZConsoleDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZConsoleDeviceConfiguration vzConsoleDeviceConfiguration => vzConsoleDeviceConfiguration -> IO (Id VZConsoleDeviceConfiguration)
init_ vzConsoleDeviceConfiguration =
  sendOwnedMessage vzConsoleDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZConsoleDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZConsoleDeviceConfiguration)
initSelector = mkSelector "init"

