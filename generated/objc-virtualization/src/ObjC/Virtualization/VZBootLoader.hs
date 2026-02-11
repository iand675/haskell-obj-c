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
new :: IO (Id VZBootLoader)
new  =
  do
    cls' <- getRequiredClass "VZBootLoader"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZBootLoader vzBootLoader => vzBootLoader -> IO (Id VZBootLoader)
init_ vzBootLoader  =
  sendMsg vzBootLoader (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

