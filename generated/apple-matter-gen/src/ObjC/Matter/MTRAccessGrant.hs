{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An access grant, which can be represented as an entry in the Matter Access Control cluster.
--
-- Generated bindings for @MTRAccessGrant@.
module ObjC.Matter.MTRAccessGrant
  ( MTRAccessGrant
  , IsMTRAccessGrant(..)
  , init_
  , new
  , accessGrantForNodeID_privilege
  , accessGrantForCASEAuthenticatedTag_privilege
  , accessGrantForGroupID_privilege
  , accessGrantForAllNodesWithPrivilege
  , subjectID
  , grantedPrivilege
  , authenticationMode
  , accessGrantForAllNodesWithPrivilegeSelector
  , accessGrantForCASEAuthenticatedTag_privilegeSelector
  , accessGrantForGroupID_privilegeSelector
  , accessGrantForNodeID_privilegeSelector
  , authenticationModeSelector
  , grantedPrivilegeSelector
  , initSelector
  , newSelector
  , subjectIDSelector

  -- * Enum types
  , MTRAccessControlEntryAuthMode(MTRAccessControlEntryAuthMode)
  , pattern MTRAccessControlEntryAuthModePASE
  , pattern MTRAccessControlEntryAuthModeCASE
  , pattern MTRAccessControlEntryAuthModeGroup
  , MTRAccessControlEntryPrivilege(MTRAccessControlEntryPrivilege)
  , pattern MTRAccessControlEntryPrivilegeView
  , pattern MTRAccessControlEntryPrivilegeProxyView
  , pattern MTRAccessControlEntryPrivilegeOperate
  , pattern MTRAccessControlEntryPrivilegeManage
  , pattern MTRAccessControlEntryPrivilegeAdminister

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO (Id MTRAccessGrant)
init_ mtrAccessGrant =
  sendOwnedMessage mtrAccessGrant initSelector

-- | @+ new@
new :: IO (Id MTRAccessGrant)
new  =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendOwnedClassMessage cls' newSelector

-- | Grant access at the provided level to a specific node on the fabric.  The provided nodeID must be an operational node identifier.
--
-- ObjC selector: @+ accessGrantForNodeID:privilege:@
accessGrantForNodeID_privilege :: IsNSNumber nodeID => nodeID -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForNodeID_privilege nodeID privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMessage cls' accessGrantForNodeID_privilegeSelector (toNSNumber nodeID) privilege

-- | Grant access to any node on the fabric that has a matching CASE Authenticated Tag in its operational certificate.  The provided caseAuthenticatedTag must be a 32-bit unsigned integer with lower 16 bits not 0, per the Matter specification.
--
-- ObjC selector: @+ accessGrantForCASEAuthenticatedTag:privilege:@
accessGrantForCASEAuthenticatedTag_privilege :: IsNSNumber caseAuthenticatedTag => caseAuthenticatedTag -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForCASEAuthenticatedTag_privilege caseAuthenticatedTag privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMessage cls' accessGrantForCASEAuthenticatedTag_privilegeSelector (toNSNumber caseAuthenticatedTag) privilege

-- | Grant access to any node on the fabric that is communicating with us via group messages sent to the given group.  The provided groupID must be a valid group identifier in the range 1-65535.
--
-- ObjC selector: @+ accessGrantForGroupID:privilege:@
accessGrantForGroupID_privilege :: IsNSNumber groupID => groupID -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForGroupID_privilege groupID privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMessage cls' accessGrantForGroupID_privilegeSelector (toNSNumber groupID) privilege

-- | Grant access to any node on the fabric, as long as it's communicating with us over a unicast authenticated channel.
--
-- ObjC selector: @+ accessGrantForAllNodesWithPrivilege:@
accessGrantForAllNodesWithPrivilege :: MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForAllNodesWithPrivilege privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMessage cls' accessGrantForAllNodesWithPrivilegeSelector privilege

-- | The matter access control subject ID that access has been granted for.  Nil when access has been granted for all subjects (e.g. via initForAllNodesWithPrivilege).
--
-- ObjC selector: @- subjectID@
subjectID :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO (Id NSNumber)
subjectID mtrAccessGrant =
  sendMessage mtrAccessGrant subjectIDSelector

-- | The privilege that has been granted
--
-- ObjC selector: @- grantedPrivilege@
grantedPrivilege :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO MTRAccessControlEntryPrivilege
grantedPrivilege mtrAccessGrant =
  sendMessage mtrAccessGrant grantedPrivilegeSelector

-- | The type of authentication mode the access grant is for. MTRAccessControlEntryAuthModeCASE for unicast messages and MTRAccessControlEntryAuthModeGroup for groupcast ones.
--
-- ObjC selector: @- authenticationMode@
authenticationMode :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO MTRAccessControlEntryAuthMode
authenticationMode mtrAccessGrant =
  sendMessage mtrAccessGrant authenticationModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRAccessGrant)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRAccessGrant)
newSelector = mkSelector "new"

-- | @Selector@ for @accessGrantForNodeID:privilege:@
accessGrantForNodeID_privilegeSelector :: Selector '[Id NSNumber, MTRAccessControlEntryPrivilege] (Id MTRAccessGrant)
accessGrantForNodeID_privilegeSelector = mkSelector "accessGrantForNodeID:privilege:"

-- | @Selector@ for @accessGrantForCASEAuthenticatedTag:privilege:@
accessGrantForCASEAuthenticatedTag_privilegeSelector :: Selector '[Id NSNumber, MTRAccessControlEntryPrivilege] (Id MTRAccessGrant)
accessGrantForCASEAuthenticatedTag_privilegeSelector = mkSelector "accessGrantForCASEAuthenticatedTag:privilege:"

-- | @Selector@ for @accessGrantForGroupID:privilege:@
accessGrantForGroupID_privilegeSelector :: Selector '[Id NSNumber, MTRAccessControlEntryPrivilege] (Id MTRAccessGrant)
accessGrantForGroupID_privilegeSelector = mkSelector "accessGrantForGroupID:privilege:"

-- | @Selector@ for @accessGrantForAllNodesWithPrivilege:@
accessGrantForAllNodesWithPrivilegeSelector :: Selector '[MTRAccessControlEntryPrivilege] (Id MTRAccessGrant)
accessGrantForAllNodesWithPrivilegeSelector = mkSelector "accessGrantForAllNodesWithPrivilege:"

-- | @Selector@ for @subjectID@
subjectIDSelector :: Selector '[] (Id NSNumber)
subjectIDSelector = mkSelector "subjectID"

-- | @Selector@ for @grantedPrivilege@
grantedPrivilegeSelector :: Selector '[] MTRAccessControlEntryPrivilege
grantedPrivilegeSelector = mkSelector "grantedPrivilege"

-- | @Selector@ for @authenticationMode@
authenticationModeSelector :: Selector '[] MTRAccessControlEntryAuthMode
authenticationModeSelector = mkSelector "authenticationMode"

