{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterMonitoringRegistrationStruct@.
module ObjC.Matter.MTRICDManagementClusterMonitoringRegistrationStruct
  ( MTRICDManagementClusterMonitoringRegistrationStruct
  , IsMTRICDManagementClusterMonitoringRegistrationStruct(..)
  , checkInNodeID
  , setCheckInNodeID
  , monitoredSubject
  , setMonitoredSubject
  , clientType
  , setClientType
  , fabricIndex
  , setFabricIndex
  , checkInNodeIDSelector
  , clientTypeSelector
  , fabricIndexSelector
  , monitoredSubjectSelector
  , setCheckInNodeIDSelector
  , setClientTypeSelector
  , setFabricIndexSelector
  , setMonitoredSubjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- checkInNodeID@
checkInNodeID :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
checkInNodeID mtricdManagementClusterMonitoringRegistrationStruct =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct checkInNodeIDSelector

-- | @- setCheckInNodeID:@
setCheckInNodeID :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setCheckInNodeID mtricdManagementClusterMonitoringRegistrationStruct value =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct setCheckInNodeIDSelector (toNSNumber value)

-- | @- monitoredSubject@
monitoredSubject :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
monitoredSubject mtricdManagementClusterMonitoringRegistrationStruct =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct monitoredSubjectSelector

-- | @- setMonitoredSubject:@
setMonitoredSubject :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setMonitoredSubject mtricdManagementClusterMonitoringRegistrationStruct value =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct setMonitoredSubjectSelector (toNSNumber value)

-- | @- clientType@
clientType :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
clientType mtricdManagementClusterMonitoringRegistrationStruct =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct clientTypeSelector

-- | @- setClientType:@
setClientType :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setClientType mtricdManagementClusterMonitoringRegistrationStruct value =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct setClientTypeSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
fabricIndex mtricdManagementClusterMonitoringRegistrationStruct =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setFabricIndex mtricdManagementClusterMonitoringRegistrationStruct value =
  sendMessage mtricdManagementClusterMonitoringRegistrationStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkInNodeID@
checkInNodeIDSelector :: Selector '[] (Id NSNumber)
checkInNodeIDSelector = mkSelector "checkInNodeID"

-- | @Selector@ for @setCheckInNodeID:@
setCheckInNodeIDSelector :: Selector '[Id NSNumber] ()
setCheckInNodeIDSelector = mkSelector "setCheckInNodeID:"

-- | @Selector@ for @monitoredSubject@
monitoredSubjectSelector :: Selector '[] (Id NSNumber)
monitoredSubjectSelector = mkSelector "monitoredSubject"

-- | @Selector@ for @setMonitoredSubject:@
setMonitoredSubjectSelector :: Selector '[Id NSNumber] ()
setMonitoredSubjectSelector = mkSelector "setMonitoredSubject:"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector '[] (Id NSNumber)
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @setClientType:@
setClientTypeSelector :: Selector '[Id NSNumber] ()
setClientTypeSelector = mkSelector "setClientType:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

