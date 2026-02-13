{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent
  ( MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent
  , IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent(..)
  , connectionStatus
  , setConnectionStatus
  , connectionStatusSelector
  , setConnectionStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connectionStatus@
connectionStatus :: IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent => mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent -> IO (Id NSNumber)
connectionStatus mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent =
  sendMessage mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent connectionStatusSelector

-- | @- setConnectionStatus:@
setConnectionStatus :: (IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent -> value -> IO ()
setConnectionStatus mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent value =
  sendMessage mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent setConnectionStatusSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector '[] (Id NSNumber)
connectionStatusSelector = mkSelector "connectionStatus"

-- | @Selector@ for @setConnectionStatus:@
setConnectionStatusSelector :: Selector '[Id NSNumber] ()
setConnectionStatusSelector = mkSelector "setConnectionStatus:"

