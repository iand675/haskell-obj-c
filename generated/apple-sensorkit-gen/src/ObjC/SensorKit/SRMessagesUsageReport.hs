{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRMessagesUsageReport@.
module ObjC.SensorKit.SRMessagesUsageReport
  ( SRMessagesUsageReport
  , IsSRMessagesUsageReport(..)
  , duration
  , totalOutgoingMessages
  , totalIncomingMessages
  , totalUniqueContacts
  , durationSelector
  , totalIncomingMessagesSelector
  , totalOutgoingMessagesSelector
  , totalUniqueContactsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- duration@
duration :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CDouble
duration srMessagesUsageReport =
  sendMessage srMessagesUsageReport durationSelector

-- | @- totalOutgoingMessages@
totalOutgoingMessages :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalOutgoingMessages srMessagesUsageReport =
  sendMessage srMessagesUsageReport totalOutgoingMessagesSelector

-- | @- totalIncomingMessages@
totalIncomingMessages :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalIncomingMessages srMessagesUsageReport =
  sendMessage srMessagesUsageReport totalIncomingMessagesSelector

-- | @- totalUniqueContacts@
totalUniqueContacts :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalUniqueContacts srMessagesUsageReport =
  sendMessage srMessagesUsageReport totalUniqueContactsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @totalOutgoingMessages@
totalOutgoingMessagesSelector :: Selector '[] CLong
totalOutgoingMessagesSelector = mkSelector "totalOutgoingMessages"

-- | @Selector@ for @totalIncomingMessages@
totalIncomingMessagesSelector :: Selector '[] CLong
totalIncomingMessagesSelector = mkSelector "totalIncomingMessages"

-- | @Selector@ for @totalUniqueContacts@
totalUniqueContactsSelector :: Selector '[] CLong
totalUniqueContactsSelector = mkSelector "totalUniqueContacts"

