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
  , authorizationSelector
  , authorizationRefSelector
  , authorizationWithFlags_rights_environmentSelector
  , initWithFlags_rights_environmentSelector
  , initSelector
  , invalidateCredentialsSelector
  , obtainWithRight_flags_errorSelector
  , obtainWithRights_flags_environment_authorizedRights_errorSelector
  , permitWithRights_flags_environment_authorizedRightsSelector
  , permitWithRight_flagsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "authorization") (retPtr retVoid) []

-- | authorizationRef
--
-- Returns the AuthorizationRef for this SFAuthorization.
--
-- ObjC selector: @- authorizationRef@
authorizationRef :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO RawId
authorizationRef sfAuthorization  =
    fmap (RawId . castPtr) $ sendMsg sfAuthorization (mkSelector "authorizationRef") (retPtr retVoid) []

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
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "authorizationWithFlags:rights:environment:") (retPtr retVoid) [argCInt (fromIntegral flags), argPtr (castPtr (unRawId (unConst rights)) :: Ptr ()), argPtr (castPtr (unRawId (unConst environment)) :: Ptr ())]

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
initWithFlags_rights_environment sfAuthorization  flags rights environment =
    fmap (RawId . castPtr) $ sendMsg sfAuthorization (mkSelector "initWithFlags:rights:environment:") (retPtr retVoid) [argCInt (fromIntegral flags), argPtr (castPtr (unRawId (unConst rights)) :: Ptr ()), argPtr (castPtr (unRawId (unConst environment)) :: Ptr ())]

-- | init
--
-- Initializes an authorization object initialized with a default environment, flags and rights.
--
-- ObjC selector: @- init@
init_ :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO RawId
init_ sfAuthorization  =
    fmap (RawId . castPtr) $ sendMsg sfAuthorization (mkSelector "init") (retPtr retVoid) []

-- | invalidateCredentials
--
-- Calling this will prevent any rights that were obtained by this object to be preserved. It effectively locks down all potentially shared authorizations.
--
-- ObjC selector: @- invalidateCredentials@
invalidateCredentials :: IsSFAuthorization sfAuthorization => sfAuthorization -> IO ()
invalidateCredentials sfAuthorization  =
    sendMsg sfAuthorization (mkSelector "invalidateCredentials") retVoid []

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
obtainWithRight_flags_error sfAuthorization  rightName flags error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorization (mkSelector "obtainWithRight:flags:error:") retCULong [argPtr (castPtr (unRawId rightName) :: Ptr ()), argCInt (fromIntegral flags), argPtr (castPtr raw_error_ :: Ptr ())]

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
obtainWithRights_flags_environment_authorizedRights_error sfAuthorization  rights flags environment authorizedRights error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorization (mkSelector "obtainWithRights:flags:environment:authorizedRights:error:") retCULong [argPtr (castPtr (unRawId (unConst rights)) :: Ptr ()), argCInt (fromIntegral flags), argPtr (castPtr (unRawId (unConst environment)) :: Ptr ()), argPtr (castPtr (unRawId authorizedRights) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

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
permitWithRights_flags_environment_authorizedRights sfAuthorization  rights flags environment authorizedRights =
    sendMsg sfAuthorization (mkSelector "permitWithRights:flags:environment:authorizedRights:") retCInt [argPtr (castPtr (unRawId (unConst rights)) :: Ptr ()), argCInt (fromIntegral flags), argPtr (castPtr (unRawId (unConst environment)) :: Ptr ()), argPtr (castPtr (unRawId authorizedRights) :: Ptr ())]

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
permitWithRight_flags sfAuthorization  rightName flags =
    sendMsg sfAuthorization (mkSelector "permitWithRight:flags:") retCInt [argPtr (castPtr (unRawId rightName) :: Ptr ()), argCInt (fromIntegral flags)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @authorizationRef@
authorizationRefSelector :: Selector
authorizationRefSelector = mkSelector "authorizationRef"

-- | @Selector@ for @authorizationWithFlags:rights:environment:@
authorizationWithFlags_rights_environmentSelector :: Selector
authorizationWithFlags_rights_environmentSelector = mkSelector "authorizationWithFlags:rights:environment:"

-- | @Selector@ for @initWithFlags:rights:environment:@
initWithFlags_rights_environmentSelector :: Selector
initWithFlags_rights_environmentSelector = mkSelector "initWithFlags:rights:environment:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @invalidateCredentials@
invalidateCredentialsSelector :: Selector
invalidateCredentialsSelector = mkSelector "invalidateCredentials"

-- | @Selector@ for @obtainWithRight:flags:error:@
obtainWithRight_flags_errorSelector :: Selector
obtainWithRight_flags_errorSelector = mkSelector "obtainWithRight:flags:error:"

-- | @Selector@ for @obtainWithRights:flags:environment:authorizedRights:error:@
obtainWithRights_flags_environment_authorizedRights_errorSelector :: Selector
obtainWithRights_flags_environment_authorizedRights_errorSelector = mkSelector "obtainWithRights:flags:environment:authorizedRights:error:"

-- | @Selector@ for @permitWithRights:flags:environment:authorizedRights:@
permitWithRights_flags_environment_authorizedRightsSelector :: Selector
permitWithRights_flags_environment_authorizedRightsSelector = mkSelector "permitWithRights:flags:environment:authorizedRights:"

-- | @Selector@ for @permitWithRight:flags:@
permitWithRight_flagsSelector :: Selector
permitWithRight_flagsSelector = mkSelector "permitWithRight:flags:"

