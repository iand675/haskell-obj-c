{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a graphics display configuration.
--
-- VZGraphicsDisplayConfiguration should not be instantiated directly.    One of its subclasses should be used instead.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- Generated bindings for @VZGraphicsDisplayConfiguration@.
module ObjC.Virtualization.VZGraphicsDisplayConfiguration
  ( VZGraphicsDisplayConfiguration
  , IsVZGraphicsDisplayConfiguration(..)
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
new :: IO (Id VZGraphicsDisplayConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDisplayConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZGraphicsDisplayConfiguration vzGraphicsDisplayConfiguration => vzGraphicsDisplayConfiguration -> IO (Id VZGraphicsDisplayConfiguration)
init_ vzGraphicsDisplayConfiguration =
  sendOwnedMessage vzGraphicsDisplayConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZGraphicsDisplayConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZGraphicsDisplayConfiguration)
initSelector = mkSelector "init"

