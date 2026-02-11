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
new :: IO (Id VZConsoleDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZConsoleDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZConsoleDeviceConfiguration vzConsoleDeviceConfiguration => vzConsoleDeviceConfiguration -> IO (Id VZConsoleDeviceConfiguration)
init_ vzConsoleDeviceConfiguration  =
  sendMsg vzConsoleDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

