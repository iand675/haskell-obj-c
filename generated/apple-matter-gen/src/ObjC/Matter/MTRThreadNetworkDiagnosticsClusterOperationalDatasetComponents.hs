{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents
  ( MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents
  , IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents(..)
  , activeTimestampPresent
  , setActiveTimestampPresent
  , pendingTimestampPresent
  , setPendingTimestampPresent
  , masterKeyPresent
  , setMasterKeyPresent
  , networkNamePresent
  , setNetworkNamePresent
  , extendedPanIdPresent
  , setExtendedPanIdPresent
  , meshLocalPrefixPresent
  , setMeshLocalPrefixPresent
  , delayPresent
  , setDelayPresent
  , panIdPresent
  , setPanIdPresent
  , channelPresent
  , setChannelPresent
  , pskcPresent
  , setPskcPresent
  , securityPolicyPresent
  , setSecurityPolicyPresent
  , channelMaskPresent
  , setChannelMaskPresent
  , activeTimestampPresentSelector
  , channelMaskPresentSelector
  , channelPresentSelector
  , delayPresentSelector
  , extendedPanIdPresentSelector
  , masterKeyPresentSelector
  , meshLocalPrefixPresentSelector
  , networkNamePresentSelector
  , panIdPresentSelector
  , pendingTimestampPresentSelector
  , pskcPresentSelector
  , securityPolicyPresentSelector
  , setActiveTimestampPresentSelector
  , setChannelMaskPresentSelector
  , setChannelPresentSelector
  , setDelayPresentSelector
  , setExtendedPanIdPresentSelector
  , setMasterKeyPresentSelector
  , setMeshLocalPrefixPresentSelector
  , setNetworkNamePresentSelector
  , setPanIdPresentSelector
  , setPendingTimestampPresentSelector
  , setPskcPresentSelector
  , setSecurityPolicyPresentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- activeTimestampPresent@
activeTimestampPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
activeTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents activeTimestampPresentSelector

-- | @- setActiveTimestampPresent:@
setActiveTimestampPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setActiveTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setActiveTimestampPresentSelector (toNSNumber value)

-- | @- pendingTimestampPresent@
pendingTimestampPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
pendingTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents pendingTimestampPresentSelector

-- | @- setPendingTimestampPresent:@
setPendingTimestampPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPendingTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setPendingTimestampPresentSelector (toNSNumber value)

-- | @- masterKeyPresent@
masterKeyPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
masterKeyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents masterKeyPresentSelector

-- | @- setMasterKeyPresent:@
setMasterKeyPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setMasterKeyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setMasterKeyPresentSelector (toNSNumber value)

-- | @- networkNamePresent@
networkNamePresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
networkNamePresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents networkNamePresentSelector

-- | @- setNetworkNamePresent:@
setNetworkNamePresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setNetworkNamePresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setNetworkNamePresentSelector (toNSNumber value)

-- | @- extendedPanIdPresent@
extendedPanIdPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
extendedPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents extendedPanIdPresentSelector

-- | @- setExtendedPanIdPresent:@
setExtendedPanIdPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setExtendedPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setExtendedPanIdPresentSelector (toNSNumber value)

-- | @- meshLocalPrefixPresent@
meshLocalPrefixPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
meshLocalPrefixPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents meshLocalPrefixPresentSelector

-- | @- setMeshLocalPrefixPresent:@
setMeshLocalPrefixPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setMeshLocalPrefixPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setMeshLocalPrefixPresentSelector (toNSNumber value)

-- | @- delayPresent@
delayPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
delayPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents delayPresentSelector

-- | @- setDelayPresent:@
setDelayPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setDelayPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setDelayPresentSelector (toNSNumber value)

-- | @- panIdPresent@
panIdPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
panIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents panIdPresentSelector

-- | @- setPanIdPresent:@
setPanIdPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setPanIdPresentSelector (toNSNumber value)

-- | @- channelPresent@
channelPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
channelPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents channelPresentSelector

-- | @- setChannelPresent:@
setChannelPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setChannelPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setChannelPresentSelector (toNSNumber value)

-- | @- pskcPresent@
pskcPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
pskcPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents pskcPresentSelector

-- | @- setPskcPresent:@
setPskcPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPskcPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setPskcPresentSelector (toNSNumber value)

-- | @- securityPolicyPresent@
securityPolicyPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
securityPolicyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents securityPolicyPresentSelector

-- | @- setSecurityPolicyPresent:@
setSecurityPolicyPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setSecurityPolicyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setSecurityPolicyPresentSelector (toNSNumber value)

-- | @- channelMaskPresent@
channelMaskPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
channelMaskPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents channelMaskPresentSelector

-- | @- setChannelMaskPresent:@
setChannelMaskPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setChannelMaskPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents value =
  sendMessage mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents setChannelMaskPresentSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activeTimestampPresent@
activeTimestampPresentSelector :: Selector '[] (Id NSNumber)
activeTimestampPresentSelector = mkSelector "activeTimestampPresent"

-- | @Selector@ for @setActiveTimestampPresent:@
setActiveTimestampPresentSelector :: Selector '[Id NSNumber] ()
setActiveTimestampPresentSelector = mkSelector "setActiveTimestampPresent:"

-- | @Selector@ for @pendingTimestampPresent@
pendingTimestampPresentSelector :: Selector '[] (Id NSNumber)
pendingTimestampPresentSelector = mkSelector "pendingTimestampPresent"

-- | @Selector@ for @setPendingTimestampPresent:@
setPendingTimestampPresentSelector :: Selector '[Id NSNumber] ()
setPendingTimestampPresentSelector = mkSelector "setPendingTimestampPresent:"

-- | @Selector@ for @masterKeyPresent@
masterKeyPresentSelector :: Selector '[] (Id NSNumber)
masterKeyPresentSelector = mkSelector "masterKeyPresent"

-- | @Selector@ for @setMasterKeyPresent:@
setMasterKeyPresentSelector :: Selector '[Id NSNumber] ()
setMasterKeyPresentSelector = mkSelector "setMasterKeyPresent:"

-- | @Selector@ for @networkNamePresent@
networkNamePresentSelector :: Selector '[] (Id NSNumber)
networkNamePresentSelector = mkSelector "networkNamePresent"

-- | @Selector@ for @setNetworkNamePresent:@
setNetworkNamePresentSelector :: Selector '[Id NSNumber] ()
setNetworkNamePresentSelector = mkSelector "setNetworkNamePresent:"

-- | @Selector@ for @extendedPanIdPresent@
extendedPanIdPresentSelector :: Selector '[] (Id NSNumber)
extendedPanIdPresentSelector = mkSelector "extendedPanIdPresent"

-- | @Selector@ for @setExtendedPanIdPresent:@
setExtendedPanIdPresentSelector :: Selector '[Id NSNumber] ()
setExtendedPanIdPresentSelector = mkSelector "setExtendedPanIdPresent:"

-- | @Selector@ for @meshLocalPrefixPresent@
meshLocalPrefixPresentSelector :: Selector '[] (Id NSNumber)
meshLocalPrefixPresentSelector = mkSelector "meshLocalPrefixPresent"

-- | @Selector@ for @setMeshLocalPrefixPresent:@
setMeshLocalPrefixPresentSelector :: Selector '[Id NSNumber] ()
setMeshLocalPrefixPresentSelector = mkSelector "setMeshLocalPrefixPresent:"

-- | @Selector@ for @delayPresent@
delayPresentSelector :: Selector '[] (Id NSNumber)
delayPresentSelector = mkSelector "delayPresent"

-- | @Selector@ for @setDelayPresent:@
setDelayPresentSelector :: Selector '[Id NSNumber] ()
setDelayPresentSelector = mkSelector "setDelayPresent:"

-- | @Selector@ for @panIdPresent@
panIdPresentSelector :: Selector '[] (Id NSNumber)
panIdPresentSelector = mkSelector "panIdPresent"

-- | @Selector@ for @setPanIdPresent:@
setPanIdPresentSelector :: Selector '[Id NSNumber] ()
setPanIdPresentSelector = mkSelector "setPanIdPresent:"

-- | @Selector@ for @channelPresent@
channelPresentSelector :: Selector '[] (Id NSNumber)
channelPresentSelector = mkSelector "channelPresent"

-- | @Selector@ for @setChannelPresent:@
setChannelPresentSelector :: Selector '[Id NSNumber] ()
setChannelPresentSelector = mkSelector "setChannelPresent:"

-- | @Selector@ for @pskcPresent@
pskcPresentSelector :: Selector '[] (Id NSNumber)
pskcPresentSelector = mkSelector "pskcPresent"

-- | @Selector@ for @setPskcPresent:@
setPskcPresentSelector :: Selector '[Id NSNumber] ()
setPskcPresentSelector = mkSelector "setPskcPresent:"

-- | @Selector@ for @securityPolicyPresent@
securityPolicyPresentSelector :: Selector '[] (Id NSNumber)
securityPolicyPresentSelector = mkSelector "securityPolicyPresent"

-- | @Selector@ for @setSecurityPolicyPresent:@
setSecurityPolicyPresentSelector :: Selector '[Id NSNumber] ()
setSecurityPolicyPresentSelector = mkSelector "setSecurityPolicyPresent:"

-- | @Selector@ for @channelMaskPresent@
channelMaskPresentSelector :: Selector '[] (Id NSNumber)
channelMaskPresentSelector = mkSelector "channelMaskPresent"

-- | @Selector@ for @setChannelMaskPresent:@
setChannelMaskPresentSelector :: Selector '[Id NSNumber] ()
setChannelMaskPresentSelector = mkSelector "setChannelMaskPresent:"

