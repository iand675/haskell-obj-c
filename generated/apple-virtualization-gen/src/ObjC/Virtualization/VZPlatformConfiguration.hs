{-# LANGUAGE DataKinds #-}
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
new :: IO (Id VZPlatformConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZPlatformConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZPlatformConfiguration vzPlatformConfiguration => vzPlatformConfiguration -> IO (Id VZPlatformConfiguration)
init_ vzPlatformConfiguration =
  sendOwnedMessage vzPlatformConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZPlatformConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZPlatformConfiguration)
initSelector = mkSelector "init"

