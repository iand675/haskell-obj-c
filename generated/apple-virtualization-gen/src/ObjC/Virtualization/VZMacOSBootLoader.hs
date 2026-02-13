{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Boot loader configuration for booting macOS on Apple Silicon.
--
-- You must use a VZMacPlatformConfiguration in conjunction with the macOS boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZMacPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
--
-- Generated bindings for @VZMacOSBootLoader@.
module ObjC.Virtualization.VZMacOSBootLoader
  ( VZMacOSBootLoader
  , IsVZMacOSBootLoader(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a VZMacOSBootLoader.
--
-- ObjC selector: @- init@
init_ :: IsVZMacOSBootLoader vzMacOSBootLoader => vzMacOSBootLoader -> IO (Id VZMacOSBootLoader)
init_ vzMacOSBootLoader =
  sendOwnedMessage vzMacOSBootLoader initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacOSBootLoader)
initSelector = mkSelector "init"

