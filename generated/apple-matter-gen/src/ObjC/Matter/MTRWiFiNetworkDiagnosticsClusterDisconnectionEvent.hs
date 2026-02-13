{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent
  ( MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent
  , IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent(..)
  , reasonCode
  , setReasonCode
  , reasonCodeSelector
  , setReasonCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reasonCode@
reasonCode :: IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent => mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent -> IO (Id NSNumber)
reasonCode mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent =
  sendMessage mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent reasonCodeSelector

-- | @- setReasonCode:@
setReasonCode :: (IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent -> value -> IO ()
setReasonCode mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent value =
  sendMessage mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent setReasonCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reasonCode@
reasonCodeSelector :: Selector '[] (Id NSNumber)
reasonCodeSelector = mkSelector "reasonCode"

-- | @Selector@ for @setReasonCode:@
setReasonCodeSelector :: Selector '[Id NSNumber] ()
setReasonCodeSelector = mkSelector "setReasonCode:"

