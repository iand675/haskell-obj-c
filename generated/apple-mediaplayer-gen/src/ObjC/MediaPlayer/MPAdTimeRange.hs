{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPAdTimeRange@.
module ObjC.MediaPlayer.MPAdTimeRange
  ( MPAdTimeRange
  , IsMPAdTimeRange(..)
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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPAdTimeRange)
new  =
  do
    cls' <- getRequiredClass "MPAdTimeRange"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPAdTimeRange mpAdTimeRange => mpAdTimeRange -> IO (Id MPAdTimeRange)
init_ mpAdTimeRange =
  sendOwnedMessage mpAdTimeRange initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPAdTimeRange)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPAdTimeRange)
initSelector = mkSelector "init"

