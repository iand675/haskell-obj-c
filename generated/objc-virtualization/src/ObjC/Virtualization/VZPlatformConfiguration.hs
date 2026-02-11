{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a platform configuration.
--
-- VZPlatformConfiguration should not be instantiated directly.    One of its subclasses should be used instead.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZMacPlatformConfiguration.
--
-- Generated bindings for @VZPlatformConfiguration@.
module ObjC.Virtualization.VZPlatformConfiguration
  ( VZPlatformConfiguration
  , IsVZPlatformConfiguration(..)
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
new :: IO (Id VZPlatformConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZPlatformConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZPlatformConfiguration vzPlatformConfiguration => vzPlatformConfiguration -> IO (Id VZPlatformConfiguration)
init_ vzPlatformConfiguration  =
  sendMsg vzPlatformConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

