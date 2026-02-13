{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class of boot loader configuration.
--
-- VZVirtualMachineConfiguration requires a boot loader defining how to start the virtual machine.    VZBootLoader is the abstract base class of boot loader definitions.
--
-- Don't instantiate VZBootLoader directly, instead use its subclass VZEFIBootLoader, VZLinuxBootLoader, or VZMacOSBootLoader.
--
-- See: VZEFIBootLoader
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
--
-- Generated bindings for @VZBootLoader@.
module ObjC.Virtualization.VZBootLoader
  ( VZBootLoader
  , IsVZBootLoader(..)
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
new :: IO (Id VZBootLoader)
new  =
  do
    cls' <- getRequiredClass "VZBootLoader"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZBootLoader vzBootLoader => vzBootLoader -> IO (Id VZBootLoader)
init_ vzBootLoader =
  sendOwnedMessage vzBootLoader initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZBootLoader)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZBootLoader)
initSelector = mkSelector "init"

