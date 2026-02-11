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
new :: IO (Id VZStorageDevice)
new  =
  do
    cls' <- getRequiredClass "VZStorageDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZStorageDevice vzStorageDevice => vzStorageDevice -> IO (Id VZStorageDevice)
init_ vzStorageDevice  =
  sendMsg vzStorageDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

