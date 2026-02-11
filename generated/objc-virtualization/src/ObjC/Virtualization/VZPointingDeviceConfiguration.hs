{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a pointing device configuration.
--
-- VZPointingDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZUSBScreenCoordinatePointingDeviceConfiguration or VZMacTrackpadConfiguration should be used instead.
--
-- See: VZUSBScreenCoordinatePointingDeviceConfiguration
--
-- See: VZMacTrackpadConfiguration
--
-- Generated bindings for @VZPointingDeviceConfiguration@.
module ObjC.Virtualization.VZPointingDeviceConfiguration
  ( VZPointingDeviceConfiguration
  , IsVZPointingDeviceConfiguration(..)
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
new :: IO (Id VZPointingDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZPointingDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZPointingDeviceConfiguration vzPointingDeviceConfiguration => vzPointingDeviceConfiguration -> IO (Id VZPointingDeviceConfiguration)
init_ vzPointingDeviceConfiguration  =
  sendMsg vzPointingDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

