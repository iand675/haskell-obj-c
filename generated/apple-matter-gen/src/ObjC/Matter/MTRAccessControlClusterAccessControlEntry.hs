{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntry@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntry
  ( MTRAccessControlClusterAccessControlEntry
  , IsMTRAccessControlClusterAccessControlEntry(..)
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
privilege :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
privilege mtrAccessControlClusterAccessControlEntry =
  sendMessage mtrAccessControlClusterAccessControlEntry privilegeSelector

-- | @- setPrivilege:@
setPrivilege :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setPrivilege mtrAccessControlClusterAccessControlEntry value =
  sendMessage mtrAccessControlClusterAccessControlEntry setPrivilegeSelector (toNSNumber value)

-- | @- authMode@
authMode :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
authMode mtrAccessControlClusterAccessControlEntry =
  sendMessage mtrAccessControlClusterAccessControlEntry authModeSelector

-- | @- setAuthMode:@
setAuthMode :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setAuthMode mtrAccessControlClusterAccessControlEntry value =
  sendMessage mtrAccessControlClusterAccessControlEntry setAuthModeSelector (toNSNumber value)

-- | @- subjects@
subjects :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSArray)
subjects mtrAccessControlClusterAccessControlEntry =
  sendMessage mtrAccessControlClusterAccessControlEntry subjectsSelector

-- | @- setSubjects:@
setSubjects :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSArray value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setSubjects mtrAccessControlClusterAccessControlEntry value =
  sendMessage mtrAccessControlClusterAccessControlEntry setSubjectsSelector (toNSArray value)

-- | @- targets@
targets :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSArray)
targets mtrAccessControlClusterAccessControlEntry =
  sendMessage mtrAccessControlClusterAccessControlEntry targetsSelector

-- | @- setTargets:@
setTargets :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSArray value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setTargets mtrAccessControlClusterAccessControlEntry value =
  sendMessage mtrAccessControlClusterAccessControlEntry setTargetsSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntry =
  sendMessage mtrAccessControlClusterAccessControlEntry fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntry value =
  sendMessage mtrAccessControlClusterAccessControlEntry setFabricIndexSelector (toNSNumber value)

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

