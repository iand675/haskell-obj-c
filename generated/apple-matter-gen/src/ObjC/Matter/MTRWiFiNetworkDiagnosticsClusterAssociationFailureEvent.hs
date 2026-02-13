{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent
  ( MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent
  , IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent(..)
  , associationFailureCause
  , setAssociationFailureCause
  , associationFailure
  , setAssociationFailure
  , status
  , setStatus
  , associationFailureCauseSelector
  , associationFailureSelector
  , setAssociationFailureCauseSelector
  , setAssociationFailureSelector
  , setStatusSelector
  , statusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- associationFailureCause@
associationFailureCause :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
associationFailureCause mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent associationFailureCauseSelector

-- | @- setAssociationFailureCause:@
setAssociationFailureCause :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setAssociationFailureCause mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent value =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent setAssociationFailureCauseSelector (toNSNumber value)

-- | @- associationFailure@
associationFailure :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
associationFailure mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent associationFailureSelector

-- | @- setAssociationFailure:@
setAssociationFailure :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setAssociationFailure mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent value =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent setAssociationFailureSelector (toNSNumber value)

-- | @- status@
status :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
status mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setStatus mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent value =
  sendMessage mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent setStatusSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @associationFailureCause@
associationFailureCauseSelector :: Selector '[] (Id NSNumber)
associationFailureCauseSelector = mkSelector "associationFailureCause"

-- | @Selector@ for @setAssociationFailureCause:@
setAssociationFailureCauseSelector :: Selector '[Id NSNumber] ()
setAssociationFailureCauseSelector = mkSelector "setAssociationFailureCause:"

-- | @Selector@ for @associationFailure@
associationFailureSelector :: Selector '[] (Id NSNumber)
associationFailureSelector = mkSelector "associationFailure"

-- | @Selector@ for @setAssociationFailure:@
setAssociationFailureSelector :: Selector '[Id NSNumber] ()
setAssociationFailureSelector = mkSelector "setAssociationFailure:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

