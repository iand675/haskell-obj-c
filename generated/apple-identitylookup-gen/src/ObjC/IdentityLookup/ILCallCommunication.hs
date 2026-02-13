{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILCallCommunication@.
module ObjC.IdentityLookup.ILCallCommunication
  ( ILCallCommunication
  , IsILCallCommunication(..)
  , isEqualToCallCommunication
  , init_
  , initSelector
  , isEqualToCallCommunicationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToCallCommunication:@
isEqualToCallCommunication :: (IsILCallCommunication ilCallCommunication, IsILCallCommunication communication) => ilCallCommunication -> communication -> IO Bool
isEqualToCallCommunication ilCallCommunication communication =
  sendMessage ilCallCommunication isEqualToCallCommunicationSelector (toILCallCommunication communication)

-- | @- init@
init_ :: IsILCallCommunication ilCallCommunication => ilCallCommunication -> IO (Id ILCallCommunication)
init_ ilCallCommunication =
  sendOwnedMessage ilCallCommunication initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToCallCommunication:@
isEqualToCallCommunicationSelector :: Selector '[Id ILCallCommunication] Bool
isEqualToCallCommunicationSelector = mkSelector "isEqualToCallCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILCallCommunication)
initSelector = mkSelector "init"

