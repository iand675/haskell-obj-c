{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent
  ( MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent
  , IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent(..)
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
connectionStatus :: IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent => mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent -> IO (Id NSNumber)
connectionStatus mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent =
  sendMessage mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent connectionStatusSelector

-- | @- setConnectionStatus:@
setConnectionStatus :: (IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent -> value -> IO ()
setConnectionStatus mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent value =
  sendMessage mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent setConnectionStatusSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector '[] (Id NSNumber)
connectionStatusSelector = mkSelector "connectionStatus"

-- | @Selector@ for @setConnectionStatus:@
setConnectionStatusSelector :: Selector '[Id NSNumber] ()
setConnectionStatusSelector = mkSelector "setConnectionStatus:"

