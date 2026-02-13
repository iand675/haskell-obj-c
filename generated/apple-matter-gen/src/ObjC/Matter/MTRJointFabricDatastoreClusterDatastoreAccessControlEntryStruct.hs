{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct(..)
  , privilege
  , setPrivilege
  , authMode
  , setAuthMode
  , subjects
  , setSubjects
  , targets
  , setTargets
  , authModeSelector
  , privilegeSelector
  , setAuthModeSelector
  , setPrivilegeSelector
  , setSubjectsSelector
  , setTargetsSelector
  , subjectsSelector
  , targetsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- privilege@
privilege :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSNumber)
privilege mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct privilegeSelector

-- | @- setPrivilege:@
setPrivilege :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setPrivilege mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct setPrivilegeSelector (toNSNumber value)

-- | @- authMode@
authMode :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSNumber)
authMode mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct authModeSelector

-- | @- setAuthMode:@
setAuthMode :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setAuthMode mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct setAuthModeSelector (toNSNumber value)

-- | @- subjects@
subjects :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSArray)
subjects mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct subjectsSelector

-- | @- setSubjects:@
setSubjects :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSArray value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setSubjects mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct setSubjectsSelector (toNSArray value)

-- | @- targets@
targets :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSArray)
targets mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct targetsSelector

-- | @- setTargets:@
setTargets :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSArray value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setTargets mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct setTargetsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @privilege@
privilegeSelector :: Selector '[] (Id NSNumber)
privilegeSelector = mkSelector "privilege"

-- | @Selector@ for @setPrivilege:@
setPrivilegeSelector :: Selector '[Id NSNumber] ()
setPrivilegeSelector = mkSelector "setPrivilege:"

-- | @Selector@ for @authMode@
authModeSelector :: Selector '[] (Id NSNumber)
authModeSelector = mkSelector "authMode"

-- | @Selector@ for @setAuthMode:@
setAuthModeSelector :: Selector '[Id NSNumber] ()
setAuthModeSelector = mkSelector "setAuthMode:"

-- | @Selector@ for @subjects@
subjectsSelector :: Selector '[] (Id NSArray)
subjectsSelector = mkSelector "subjects"

-- | @Selector@ for @setSubjects:@
setSubjectsSelector :: Selector '[Id NSArray] ()
setSubjectsSelector = mkSelector "setSubjects:"

-- | @Selector@ for @targets@
targetsSelector :: Selector '[] (Id NSArray)
targetsSelector = mkSelector "targets"

-- | @Selector@ for @setTargets:@
setTargetsSelector :: Selector '[Id NSArray] ()
setTargetsSelector = mkSelector "setTargets:"

