{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterSecurityPolicy@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterSecurityPolicy
  ( MTRThreadNetworkDiagnosticsClusterSecurityPolicy
  , IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy(..)
  , rotationTime
  , setRotationTime
  , flags
  , setFlags
  , flagsSelector
  , rotationTimeSelector
  , setFlagsSelector
  , setRotationTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rotationTime@
rotationTime :: IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> IO (Id NSNumber)
rotationTime mtrThreadNetworkDiagnosticsClusterSecurityPolicy =
  sendMessage mtrThreadNetworkDiagnosticsClusterSecurityPolicy rotationTimeSelector

-- | @- setRotationTime:@
setRotationTime :: (IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> value -> IO ()
setRotationTime mtrThreadNetworkDiagnosticsClusterSecurityPolicy value =
  sendMessage mtrThreadNetworkDiagnosticsClusterSecurityPolicy setRotationTimeSelector (toNSNumber value)

-- | @- flags@
flags :: IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> IO (Id NSNumber)
flags mtrThreadNetworkDiagnosticsClusterSecurityPolicy =
  sendMessage mtrThreadNetworkDiagnosticsClusterSecurityPolicy flagsSelector

-- | @- setFlags:@
setFlags :: (IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> value -> IO ()
setFlags mtrThreadNetworkDiagnosticsClusterSecurityPolicy value =
  sendMessage mtrThreadNetworkDiagnosticsClusterSecurityPolicy setFlagsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rotationTime@
rotationTimeSelector :: Selector '[] (Id NSNumber)
rotationTimeSelector = mkSelector "rotationTime"

-- | @Selector@ for @setRotationTime:@
setRotationTimeSelector :: Selector '[Id NSNumber] ()
setRotationTimeSelector = mkSelector "setRotationTime:"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] (Id NSNumber)
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector '[Id NSNumber] ()
setFlagsSelector = mkSelector "setFlags:"

