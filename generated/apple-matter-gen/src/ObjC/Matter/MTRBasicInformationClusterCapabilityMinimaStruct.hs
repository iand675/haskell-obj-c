{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterCapabilityMinimaStruct@.
module ObjC.Matter.MTRBasicInformationClusterCapabilityMinimaStruct
  ( MTRBasicInformationClusterCapabilityMinimaStruct
  , IsMTRBasicInformationClusterCapabilityMinimaStruct(..)
  , caseSessionsPerFabric
  , setCaseSessionsPerFabric
  , subscriptionsPerFabric
  , setSubscriptionsPerFabric
  , caseSessionsPerFabricSelector
  , setCaseSessionsPerFabricSelector
  , setSubscriptionsPerFabricSelector
  , subscriptionsPerFabricSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- caseSessionsPerFabric@
caseSessionsPerFabric :: IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct => mtrBasicInformationClusterCapabilityMinimaStruct -> IO (Id NSNumber)
caseSessionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct =
  sendMessage mtrBasicInformationClusterCapabilityMinimaStruct caseSessionsPerFabricSelector

-- | @- setCaseSessionsPerFabric:@
setCaseSessionsPerFabric :: (IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicInformationClusterCapabilityMinimaStruct -> value -> IO ()
setCaseSessionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct value =
  sendMessage mtrBasicInformationClusterCapabilityMinimaStruct setCaseSessionsPerFabricSelector (toNSNumber value)

-- | @- subscriptionsPerFabric@
subscriptionsPerFabric :: IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct => mtrBasicInformationClusterCapabilityMinimaStruct -> IO (Id NSNumber)
subscriptionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct =
  sendMessage mtrBasicInformationClusterCapabilityMinimaStruct subscriptionsPerFabricSelector

-- | @- setSubscriptionsPerFabric:@
setSubscriptionsPerFabric :: (IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicInformationClusterCapabilityMinimaStruct -> value -> IO ()
setSubscriptionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct value =
  sendMessage mtrBasicInformationClusterCapabilityMinimaStruct setSubscriptionsPerFabricSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @caseSessionsPerFabric@
caseSessionsPerFabricSelector :: Selector '[] (Id NSNumber)
caseSessionsPerFabricSelector = mkSelector "caseSessionsPerFabric"

-- | @Selector@ for @setCaseSessionsPerFabric:@
setCaseSessionsPerFabricSelector :: Selector '[Id NSNumber] ()
setCaseSessionsPerFabricSelector = mkSelector "setCaseSessionsPerFabric:"

-- | @Selector@ for @subscriptionsPerFabric@
subscriptionsPerFabricSelector :: Selector '[] (Id NSNumber)
subscriptionsPerFabricSelector = mkSelector "subscriptionsPerFabric"

-- | @Selector@ for @setSubscriptionsPerFabric:@
setSubscriptionsPerFabricSelector :: Selector '[Id NSNumber] ()
setSubscriptionsPerFabricSelector = mkSelector "setSubscriptionsPerFabric:"

