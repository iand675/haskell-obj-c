{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to describe and configure parameters to be used in a nearby interaction session.
--
-- Generated bindings for @NIConfiguration@.
module ObjC.NearbyInteraction.NIConfiguration
  ( NIConfiguration
  , IsNIConfiguration(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIConfiguration niConfiguration => niConfiguration -> IO (Id NIConfiguration)
init_ niConfiguration =
  sendOwnedMessage niConfiguration initSelector

-- | @+ new@
new :: IO (Id NIConfiguration)
new  =
  do
    cls' <- getRequiredClass "NIConfiguration"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NIConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NIConfiguration)
newSelector = mkSelector "new"

