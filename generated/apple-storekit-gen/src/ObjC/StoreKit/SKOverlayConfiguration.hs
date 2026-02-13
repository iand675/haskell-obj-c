{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKOverlayConfiguration@.
module ObjC.StoreKit.SKOverlayConfiguration
  ( SKOverlayConfiguration
  , IsSKOverlayConfiguration(..)
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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlayConfiguration skOverlayConfiguration => skOverlayConfiguration -> IO (Id SKOverlayConfiguration)
init_ skOverlayConfiguration =
  sendOwnedMessage skOverlayConfiguration initSelector

-- | @+ new@
new :: IO (Id SKOverlayConfiguration)
new  =
  do
    cls' <- getRequiredClass "SKOverlayConfiguration"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKOverlayConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SKOverlayConfiguration)
newSelector = mkSelector "new"

