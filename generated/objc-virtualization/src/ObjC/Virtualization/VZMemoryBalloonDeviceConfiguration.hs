{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a memory balloon device configuration.
--
-- VZMemoryBalloonDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioTraditionalMemoryBalloonDeviceConfiguration should be used instead.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- Generated bindings for @VZMemoryBalloonDeviceConfiguration@.
module ObjC.Virtualization.VZMemoryBalloonDeviceConfiguration
  ( VZMemoryBalloonDeviceConfiguration
  , IsVZMemoryBalloonDeviceConfiguration(..)
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
new :: IO (Id VZMemoryBalloonDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZMemoryBalloonDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMemoryBalloonDeviceConfiguration vzMemoryBalloonDeviceConfiguration => vzMemoryBalloonDeviceConfiguration -> IO (Id VZMemoryBalloonDeviceConfiguration)
init_ vzMemoryBalloonDeviceConfiguration  =
  sendMsg vzMemoryBalloonDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

