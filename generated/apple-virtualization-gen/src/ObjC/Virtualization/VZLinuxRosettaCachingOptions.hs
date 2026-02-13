{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a VZLinuxRosettaCachingOptions.
--
-- VZLinuxRosettaCachingOptions define the communication mechanism between the Rosetta daemon and the Rosetta runtime.
--
-- VZLinuxRosettaCachingOptions should not be instantiated directly.    One of its subclasses like VZLinuxRosettaUnixSocketCachingOptions or VZLinuxRosettaAbstractCachingOptions should be used instead.
--
-- See: VZLinuxRosettaUnixSocketCachingOptions
--
-- See: VZLinuxRosettaAbstractCachingOptions
--
-- Generated bindings for @VZLinuxRosettaCachingOptions@.
module ObjC.Virtualization.VZLinuxRosettaCachingOptions
  ( VZLinuxRosettaCachingOptions
  , IsVZLinuxRosettaCachingOptions(..)
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
new :: IO (Id VZLinuxRosettaCachingOptions)
new  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaCachingOptions"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZLinuxRosettaCachingOptions vzLinuxRosettaCachingOptions => vzLinuxRosettaCachingOptions -> IO (Id VZLinuxRosettaCachingOptions)
init_ vzLinuxRosettaCachingOptions =
  sendOwnedMessage vzLinuxRosettaCachingOptions initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZLinuxRosettaCachingOptions)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZLinuxRosettaCachingOptions)
initSelector = mkSelector "init"

