{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFAuthorization
--
-- SFAuthorization APIs are used for implementing access control in applications and daemons. It has NSCoder support for proxied objects SFAuthorization is a wrapper for using the Authorization API.
--
-- Generated bindings for @SFAuthorization@.
module ObjC.SecurityFoundation.SFAuthorization
  ( SFAuthorization
  , IsSFAuthorization(..)
  , authorization
  , authorizationRef
  , authorizationWithFlags_rights_environment
  , initWithFlags_rights_environment
  , init_
  , invalidateCredentials
  , obtainWithRight_flags_error
  , obtainWithRights_flags_environment_authorizedRights_error
  , permitWithRights_flags_environment_authorizedRights
  , permitWithRight_flags
  , authorizationRefSelector
  , authorizationSelector
  , authorizationWithFlags_rights_environmentSelector
  , initSelector
  , initWithFlags_rights_environmentSelector
  , invalidateCredentialsSelector
  , obtainWithRight_flags_errorSelector
  , obtainWithRights_flags_environment_authorizedRights_errorSelector
  , permitWithRight_flagsSelector
  , permitWithRights_flags_environment_authorizedRightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SecurityFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | authorization
--
-- Returns an authorization object initialized with a default environment, flags and rights.
--
-- ObjC selector: @+ authorization@
authorization :: IO RawId
authorization  =
  do
    cls' <- getRequiredClass "SFAuthorization"
    sendClassMessage cls' authorizationSelector

-- | authorizationRef
--
-- Returns the AuthorizationRef for this SFAuthorization.
--
-- ObjC selector: @- authorizationRef@
authorizationRef :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO RawId
authorizationRef sfAuthorization =
  sendMessage sfAuthorization authorizationRefSelector

-- | authorizationWithFlags:rights:environment:
--
-- Returns an authorization object initialized with the specified flags, rights and environment.
--
-- @flags@ — Authorization flags.
--
-- @rights@ — (input/optional) An AuthorizationItemSet containing rights for which authorization is being requested.  If none are specified the resulting AuthorizationRef will authorize nothing at all.
--
-- @environment@ — (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
--
-- ObjC selector: @+ authorizationWithFlags:rights:environment:@
authorizationWithFlags_rights_environment :: CInt -> Const RawId -> Const RawId -> IO RawId
authorizationWithFlags_rights_environment flags rights environment =
  do
    cls' <- getRequiredClass "SFAuthorization"
    sendClassMessage cls' authorizationWithFlags_rights_environmentSelector flags rights environment

-- | initWithFlags:rights:environment:
--
-- Initializes an authorization object specified flags, rights and environment.
--
-- @flags@ — Authorization flags.
--
-- @rights@ — (input/optional) An AuthorizationItemSet containing rights for which authorization is being requested.  If none are specified the resulting AuthorizationRef will authorize nothing at all.
--
-- @environment@ — (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
--
-- ObjC selector: @- initWithFlags:rights:environment:@
initWithFlags_rights_environment :: IsSFAuthorization sfAuthorization => sfAuthorization -> CInt -> Const RawId -> Const RawId -> IO RawId
initWithFlags_rights_environment sfAuthorization flags rights environment =
  sendOwnedMessage sfAuthorization initWithFlags_rights_environmentSelector flags rights environment

-- | init
--
-- Initializes an authorization object initialized with a default environment, flags and rights.
--
-- ObjC selector: @- init@
init_ :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO RawId
init_ sfAuthorization =
  sendOwnedMessage sfAuthorization initSelector

-- | invalidateCredentials
--
-- Calling this will prevent any rights that were obtained by this object to be preserved. It effectively locks down all potentially shared authorizations.
--
-- ObjC selector: @- invalidateCredentials@
invalidateCredentials :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO ()
invalidateCredentials sfAuthorization =
  sendMessage sfAuthorization invalidateCredentialsSelector

-- | obtainWithRight:flags:
--
-- Call obtainWithRight to gain a right to have access to a privilege operation. On success, YES is returned.
--
-- @rightName@ — The name of an authorization right.
--
-- @flags@ — Authorization flags.
--
-- @error@ — Resulting error.
--
-- ObjC selector: @- obtainWithRight:flags:error:@
obtainWithRight_flags_error :: (IsSFAuthorization sfAuthorization, IsNSError error_) => sfAuthorization -> RawId -> CInt -> error_ -> IO Bool
obtainWithRight_flags_error sfAuthorization rightName flags error_ =
  sendMessage sfAuthorization obtainWithRight_flags_errorSelector rightName flags (toNSError error_)

-- | obtainWithRights:flags:environment:authorizedRights:error:
--
-- Call obtainWithRights to gain the rights to have access to privileged operations. On success, YES is returned.
--
-- @flags@ — Authorization flags.
--
-- @rights@ — (input) A rights set (see AuthorizationCreate).
--
-- @environment@ — (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
--
-- @authorizedRights@ — (output/optional) A pointer to a newly allocated AuthorizationInfoSet in which the authorized subset of rights are returned (authorizedRights should be deallocated by calling AuthorizationFreeInfoSet() when it is no longer needed).  If NULL the only information returned is the status.  Note that if the kAuthorizationFlagPreAuthorize flag was specified rights that could not be preauthorized are returned in authorizedRights, but their flags contains the kAuthorizationFlagCanNotPreAuthorize bit.
--
-- @error@ — Resulting error.
--
-- ObjC selector: @- obtainWithRights:flags:environment:authorizedRights:error:@
obtainWithRights_flags_environment_authorizedRights_error :: (IsSFAuthorization sfAuthorization, IsNSError error_) => sfAuthorization -> Const RawId -> CInt -> Const RawId -> RawId -> error_ -> IO Bool
obtainWithRights_flags_environment_authorizedRights_error sfAuthorization rights flags environment authorizedRights error_ =
  sendMessage sfAuthorization obtainWithRights_flags_environment_authorizedRights_errorSelector rights flags environment authorizedRights (toNSError error_)

-- | DEPRECATED: Use obtainWithRights:flags:environment:authorizedRights:error:
--
-- permitWithRights:flags:environment:authorizedRights:
--
-- Call permitWithRights to gain the rights to have access to privileged operations and to obtain the result.
--
-- @flags@ — Authorization flags.
--
-- @rights@ — (input) A rights set (see AuthorizationCreate).
--
-- @environment@ — (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
--
-- @authorizedRights@ — (output/optional) A pointer to a newly allocated AuthorizationInfoSet in which the authorized subset of rights are returned (authorizedRights should be deallocated by calling AuthorizationFreeInfoSet() when it is no longer needed).  If NULL the only information returned is the status.  Note that if the kAuthorizationFlagPreAuthorize flag was specified rights that could not be preauthorized are returned in authorizedRights, but their flags contains the kAuthorizationFlagCanNotPreAuthorize bit.
--
-- ObjC selector: @- permitWithRights:flags:environment:authorizedRights:@
permitWithRights_flags_environment_authorizedRights :: IsSFAuthorization sfAuthorization => sfAuthorization -> Const RawId -> CInt -> Const RawId -> RawId -> IO CInt
permitWithRights_flags_environment_authorizedRights sfAuthorization rights flags environment authorizedRights =
  sendMessage sfAuthorization permitWithRights_flags_environment_authorizedRightsSelector rights flags environment authorizedRights

-- | DEPRECATED: Use obtainWithRight:flags:error:
--
-- permitWithRight:flags:
--
-- Call permitWithRight to gain a right to have access to a privilege operation.
--
-- @rightName@ — The name of an authorization right.
--
-- @flags@ — Authorization flags.
--
-- ObjC selector: @- permitWithRight:flags:@
permitWithRight_flags :: IsSFAuthorization sfAuthorization => sfAuthorization -> RawId -> CInt -> IO CInt
permitWithRight_flags sfAuthorization rightName flags =
  sendMessage sfAuthorization permitWithRight_flagsSelector rightName flags

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorization@
authorizationSelector :: Selector '[] RawId
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @authorizationRef@
authorizationRefSelector :: Selector '[] RawId
authorizationRefSelector = mkSelector "authorizationRef"

-- | @Selector@ for @authorizationWithFlags:rights:environment:@
authorizationWithFlags_rights_environmentSelector :: Selector '[CInt, Const RawId, Const RawId] RawId
authorizationWithFlags_rights_environmentSelector = mkSelector "authorizationWithFlags:rights:environment:"

-- | @Selector@ for @initWithFlags:rights:environment:@
initWithFlags_rights_environmentSelector :: Selector '[CInt, Const RawId, Const RawId] RawId
initWithFlags_rights_environmentSelector = mkSelector "initWithFlags:rights:environment:"

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @invalidateCredentials@
invalidateCredentialsSelector :: Selector '[] ()
invalidateCredentialsSelector = mkSelector "invalidateCredentials"

-- | @Selector@ for @obtainWithRight:flags:error:@
obtainWithRight_flags_errorSelector :: Selector '[RawId, CInt, Id NSError] Bool
obtainWithRight_flags_errorSelector = mkSelector "obtainWithRight:flags:error:"

-- | @Selector@ for @obtainWithRights:flags:environment:authorizedRights:error:@
obtainWithRights_flags_environment_authorizedRights_errorSelector :: Selector '[Const RawId, CInt, Const RawId, RawId, Id NSError] Bool
obtainWithRights_flags_environment_authorizedRights_errorSelector = mkSelector "obtainWithRights:flags:environment:authorizedRights:error:"

-- | @Selector@ for @permitWithRights:flags:environment:authorizedRights:@
permitWithRights_flags_environment_authorizedRightsSelector :: Selector '[Const RawId, CInt, Const RawId, RawId] CInt
permitWithRights_flags_environment_authorizedRightsSelector = mkSelector "permitWithRights:flags:environment:authorizedRights:"

-- | @Selector@ for @permitWithRight:flags:@
permitWithRight_flagsSelector :: Selector '[RawId, CInt] CInt
permitWithRight_flagsSelector = mkSelector "permitWithRight:flags:"

