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

-- | Create a VZMacOSBootLoader.
--
-- ObjC selector: @- init@
init_ :: IsVZMacOSBootLoader vzMacOSBootLoader => vzMacOSBootLoader -> IO (Id VZMacOSBootLoader)
init_ vzMacOSBootLoader  =
  sendMsg vzMacOSBootLoader (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

