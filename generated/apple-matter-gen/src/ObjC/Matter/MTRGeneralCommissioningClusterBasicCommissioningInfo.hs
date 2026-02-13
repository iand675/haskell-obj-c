{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterBasicCommissioningInfo@.
module ObjC.Matter.MTRGeneralCommissioningClusterBasicCommissioningInfo
  ( MTRGeneralCommissioningClusterBasicCommissioningInfo
  , IsMTRGeneralCommissioningClusterBasicCommissioningInfo(..)
  , failSafeExpiryLengthSeconds
  , setFailSafeExpiryLengthSeconds
  , maxCumulativeFailsafeSeconds
  , setMaxCumulativeFailsafeSeconds
  , failSafeExpiryLengthSecondsSelector
  , maxCumulativeFailsafeSecondsSelector
  , setFailSafeExpiryLengthSecondsSelector
  , setMaxCumulativeFailsafeSecondsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- failSafeExpiryLengthSeconds@
failSafeExpiryLengthSeconds :: IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo => mtrGeneralCommissioningClusterBasicCommissioningInfo -> IO (Id NSNumber)
failSafeExpiryLengthSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo =
  sendMessage mtrGeneralCommissioningClusterBasicCommissioningInfo failSafeExpiryLengthSecondsSelector

-- | @- setFailSafeExpiryLengthSeconds:@
setFailSafeExpiryLengthSeconds :: (IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo, IsNSNumber value) => mtrGeneralCommissioningClusterBasicCommissioningInfo -> value -> IO ()
setFailSafeExpiryLengthSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo value =
  sendMessage mtrGeneralCommissioningClusterBasicCommissioningInfo setFailSafeExpiryLengthSecondsSelector (toNSNumber value)

-- | @- maxCumulativeFailsafeSeconds@
maxCumulativeFailsafeSeconds :: IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo => mtrGeneralCommissioningClusterBasicCommissioningInfo -> IO (Id NSNumber)
maxCumulativeFailsafeSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo =
  sendMessage mtrGeneralCommissioningClusterBasicCommissioningInfo maxCumulativeFailsafeSecondsSelector

-- | @- setMaxCumulativeFailsafeSeconds:@
setMaxCumulativeFailsafeSeconds :: (IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo, IsNSNumber value) => mtrGeneralCommissioningClusterBasicCommissioningInfo -> value -> IO ()
setMaxCumulativeFailsafeSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo value =
  sendMessage mtrGeneralCommissioningClusterBasicCommissioningInfo setMaxCumulativeFailsafeSecondsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @failSafeExpiryLengthSeconds@
failSafeExpiryLengthSecondsSelector :: Selector '[] (Id NSNumber)
failSafeExpiryLengthSecondsSelector = mkSelector "failSafeExpiryLengthSeconds"

-- | @Selector@ for @setFailSafeExpiryLengthSeconds:@
setFailSafeExpiryLengthSecondsSelector :: Selector '[Id NSNumber] ()
setFailSafeExpiryLengthSecondsSelector = mkSelector "setFailSafeExpiryLengthSeconds:"

-- | @Selector@ for @maxCumulativeFailsafeSeconds@
maxCumulativeFailsafeSecondsSelector :: Selector '[] (Id NSNumber)
maxCumulativeFailsafeSecondsSelector = mkSelector "maxCumulativeFailsafeSeconds"

-- | @Selector@ for @setMaxCumulativeFailsafeSeconds:@
setMaxCumulativeFailsafeSecondsSelector :: Selector '[Id NSNumber] ()
setMaxCumulativeFailsafeSecondsSelector = mkSelector "setMaxCumulativeFailsafeSeconds:"

