{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILCallClassificationRequest@.
module ObjC.IdentityLookup.ILCallClassificationRequest
  ( ILCallClassificationRequest
  , IsILCallClassificationRequest(..)
  , init_
  , callCommunications
  , callCommunicationsSelector
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
init_ :: IsILCallClassificationRequest ilCallClassificationRequest => ilCallClassificationRequest -> IO (Id ILCallClassificationRequest)
init_ ilCallClassificationRequest =
  sendOwnedMessage ilCallClassificationRequest initSelector

-- | @- callCommunications@
callCommunications :: IsILCallClassificationRequest ilCallClassificationRequest => ilCallClassificationRequest -> IO (Id NSArray)
callCommunications ilCallClassificationRequest =
  sendMessage ilCallClassificationRequest callCommunicationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILCallClassificationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @callCommunications@
callCommunicationsSelector :: Selector '[] (Id NSArray)
callCommunicationsSelector = mkSelector "callCommunications"

