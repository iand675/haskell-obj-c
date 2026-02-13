{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntryStruct
  ( MTRAccessControlClusterAccessControlEntryStruct
  , IsMTRAccessControlClusterAccessControlEntryStruct(..)
  , privilege
  , setPrivilege
  , authMode
  , setAuthMode
  , subjects
  , setSubjects
  , targets
  , setTargets
  , fabricIndex
  , setFabricIndex
  , authModeSelector
  , fabricIndexSelector
  , privilegeSelector
  , setAuthModeSelector
  , setFabricIndexSelector
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
privilege :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
privilege mtrAccessControlClusterAccessControlEntryStruct =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct privilegeSelector

-- | @- setPrivilege:@
setPrivilege :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setPrivilege mtrAccessControlClusterAccessControlEntryStruct value =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct setPrivilegeSelector (toNSNumber value)

-- | @- authMode@
authMode :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
authMode mtrAccessControlClusterAccessControlEntryStruct =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct authModeSelector

-- | @- setAuthMode:@
setAuthMode :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setAuthMode mtrAccessControlClusterAccessControlEntryStruct value =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct setAuthModeSelector (toNSNumber value)

-- | @- subjects@
subjects :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSArray)
subjects mtrAccessControlClusterAccessControlEntryStruct =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct subjectsSelector

-- | @- setSubjects:@
setSubjects :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setSubjects mtrAccessControlClusterAccessControlEntryStruct value =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct setSubjectsSelector (toNSArray value)

-- | @- targets@
targets :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSArray)
targets mtrAccessControlClusterAccessControlEntryStruct =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct targetsSelector

-- | @- setTargets:@
setTargets :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setTargets mtrAccessControlClusterAccessControlEntryStruct value =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct setTargetsSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntryStruct =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntryStruct value =
  sendMessage mtrAccessControlClusterAccessControlEntryStruct setFabricIndexSelector (toNSNumber value)

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

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

