{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to query a MessageFilter extension about how to interpret a received message.
--
-- Generated bindings for @ILMessageFilterCapabilitiesQueryRequest@.
module ObjC.IdentityLookup.ILMessageFilterCapabilitiesQueryRequest
  ( ILMessageFilterCapabilitiesQueryRequest
  , IsILMessageFilterCapabilitiesQueryRequest(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsILMessageFilterCapabilitiesQueryRequest ilMessageFilterCapabilitiesQueryRequest => ilMessageFilterCapabilitiesQueryRequest -> IO (Id ILMessageFilterCapabilitiesQueryRequest)
init_ ilMessageFilterCapabilitiesQueryRequest =
  sendOwnedMessage ilMessageFilterCapabilitiesQueryRequest initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILMessageFilterCapabilitiesQueryRequest)
initSelector = mkSelector "init"

