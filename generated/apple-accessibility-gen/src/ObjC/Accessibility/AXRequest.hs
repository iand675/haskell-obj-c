{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXRequest@.
module ObjC.Accessibility.AXRequest
  ( AXRequest
  , IsAXRequest(..)
  , init_
  , new
  , currentRequest
  , currentRequestSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAXRequest axRequest => axRequest -> IO (Id AXRequest)
init_ axRequest =
  sendOwnedMessage axRequest initSelector

-- | @+ new@
new :: IO (Id AXRequest)
new  =
  do
    cls' <- getRequiredClass "AXRequest"
    sendOwnedClassMessage cls' newSelector

-- | @+ currentRequest@
currentRequest :: IO (Id AXRequest)
currentRequest  =
  do
    cls' <- getRequiredClass "AXRequest"
    sendClassMessage cls' currentRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector '[] (Id AXRequest)
currentRequestSelector = mkSelector "currentRequest"

