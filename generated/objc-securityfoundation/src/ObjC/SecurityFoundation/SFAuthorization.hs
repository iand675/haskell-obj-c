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
  , init_
  , invalidateCredentials
  , obtainWithRight_flags_error
  , permitWithRight_flags
  , authorizationSelector
  , authorizationRefSelector
  , initSelector
  , invalidateCredentialsSelector
  , obtainWithRight_flags_errorSelector
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
obtainWithRight_flags_error :: (IsSFAuthorization sfAuthorization, IsNSError error_) => sfAuthorization -> RawId -> AuthorizationFlags -> error_ -> IO Bool
obtainWithRight_flags_error sfAuthorization  rightName flags error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorization (mkSelector "obtainWithRight:flags:error:") retCULong [argPtr (castPtr (unRawId rightName) :: Ptr ()), argCUInt (coerce flags), argPtr (castPtr raw_error_ :: Ptr ())]

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
permitWithRight_flags :: IsSFAuthorization sfAuthorization => sfAuthorization -> RawId -> AuthorizationFlags -> IO CInt
permitWithRight_flags sfAuthorization  rightName flags =
  sendMsg sfAuthorization (mkSelector "permitWithRight:flags:") retCInt [argPtr (castPtr (unRawId rightName) :: Ptr ()), argCUInt (coerce flags)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @authorizationRef@
authorizationRefSelector :: Selector
authorizationRefSelector = mkSelector "authorizationRef"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @invalidateCredentials@
invalidateCredentialsSelector :: Selector
invalidateCredentialsSelector = mkSelector "invalidateCredentials"

-- | @Selector@ for @obtainWithRight:flags:error:@
obtainWithRight_flags_errorSelector :: Selector
obtainWithRight_flags_errorSelector = mkSelector "obtainWithRight:flags:error:"

-- | @Selector@ for @permitWithRight:flags:@
permitWithRight_flagsSelector :: Selector
permitWithRight_flagsSelector = mkSelector "permitWithRight:flags:"

