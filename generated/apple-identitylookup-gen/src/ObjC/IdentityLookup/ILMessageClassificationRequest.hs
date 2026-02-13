{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILMessageClassificationRequest@.
module ObjC.IdentityLookup.ILMessageClassificationRequest
  ( ILMessageClassificationRequest
  , IsILMessageClassificationRequest(..)
  , init_
  , messageCommunications
  , initSelector
  , messageCommunicationsSelector


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
init_ :: IsILMessageClassificationRequest ilMessageClassificationRequest => ilMessageClassificationRequest -> IO (Id ILMessageClassificationRequest)
init_ ilMessageClassificationRequest =
  sendOwnedMessage ilMessageClassificationRequest initSelector

-- | @- messageCommunications@
messageCommunications :: IsILMessageClassificationRequest ilMessageClassificationRequest => ilMessageClassificationRequest -> IO (Id NSArray)
messageCommunications ilMessageClassificationRequest =
  sendMessage ilMessageClassificationRequest messageCommunicationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILMessageClassificationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @messageCommunications@
messageCommunicationsSelector :: Selector '[] (Id NSArray)
messageCommunicationsSelector = mkSelector "messageCommunications"

