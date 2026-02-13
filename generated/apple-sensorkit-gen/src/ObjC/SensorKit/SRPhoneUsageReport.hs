{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhoneUsageReport@.
module ObjC.SensorKit.SRPhoneUsageReport
  ( SRPhoneUsageReport
  , IsSRPhoneUsageReport(..)
  , duration
  , totalOutgoingCalls
  , totalIncomingCalls
  , totalUniqueContacts
  , totalPhoneCallDuration
  , durationSelector
  , totalIncomingCallsSelector
  , totalOutgoingCallsSelector
  , totalPhoneCallDurationSelector
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
duration :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CDouble
duration srPhoneUsageReport =
  sendMessage srPhoneUsageReport durationSelector

-- | @- totalOutgoingCalls@
totalOutgoingCalls :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalOutgoingCalls srPhoneUsageReport =
  sendMessage srPhoneUsageReport totalOutgoingCallsSelector

-- | @- totalIncomingCalls@
totalIncomingCalls :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalIncomingCalls srPhoneUsageReport =
  sendMessage srPhoneUsageReport totalIncomingCallsSelector

-- | @- totalUniqueContacts@
totalUniqueContacts :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalUniqueContacts srPhoneUsageReport =
  sendMessage srPhoneUsageReport totalUniqueContactsSelector

-- | @- totalPhoneCallDuration@
totalPhoneCallDuration :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CDouble
totalPhoneCallDuration srPhoneUsageReport =
  sendMessage srPhoneUsageReport totalPhoneCallDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @totalOutgoingCalls@
totalOutgoingCallsSelector :: Selector '[] CLong
totalOutgoingCallsSelector = mkSelector "totalOutgoingCalls"

-- | @Selector@ for @totalIncomingCalls@
totalIncomingCallsSelector :: Selector '[] CLong
totalIncomingCallsSelector = mkSelector "totalIncomingCalls"

-- | @Selector@ for @totalUniqueContacts@
totalUniqueContactsSelector :: Selector '[] CLong
totalUniqueContactsSelector = mkSelector "totalUniqueContacts"

-- | @Selector@ for @totalPhoneCallDuration@
totalPhoneCallDurationSelector :: Selector '[] CDouble
totalPhoneCallDurationSelector = mkSelector "totalPhoneCallDuration"

