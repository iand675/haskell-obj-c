{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An incident of communication via some medium.
--
-- Generated bindings for @ILCommunication@.
module ObjC.IdentityLookup.ILCommunication
  ( ILCommunication
  , IsILCommunication(..)
  , isEqualToCommunication
  , init_
  , sender
  , dateReceived
  , dateReceivedSelector
  , initSelector
  , isEqualToCommunicationSelector
  , senderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToCommunication:@
isEqualToCommunication :: (IsILCommunication ilCommunication, IsILCommunication communication) => ilCommunication -> communication -> IO Bool
isEqualToCommunication ilCommunication communication =
  sendMessage ilCommunication isEqualToCommunicationSelector (toILCommunication communication)

-- | @- init@
init_ :: IsILCommunication ilCommunication => ilCommunication -> IO (Id ILCommunication)
init_ ilCommunication =
  sendOwnedMessage ilCommunication initSelector

-- | The phone number or e-mail address of the sender.  The value will be nil if the sender is unknown.
--
-- ObjC selector: @- sender@
sender :: IsILCommunication ilCommunication => ilCommunication -> IO (Id NSString)
sender ilCommunication =
  sendMessage ilCommunication senderSelector

-- | @- dateReceived@
dateReceived :: IsILCommunication ilCommunication => ilCommunication -> IO (Id NSDate)
dateReceived ilCommunication =
  sendMessage ilCommunication dateReceivedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToCommunication:@
isEqualToCommunicationSelector :: Selector '[Id ILCommunication] Bool
isEqualToCommunicationSelector = mkSelector "isEqualToCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILCommunication)
initSelector = mkSelector "init"

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id NSString)
senderSelector = mkSelector "sender"

-- | @Selector@ for @dateReceived@
dateReceivedSelector :: Selector '[] (Id NSDate)
dateReceivedSelector = mkSelector "dateReceived"

