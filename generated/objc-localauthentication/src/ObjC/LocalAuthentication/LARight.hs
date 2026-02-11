{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups a set of requirements that need to be satisfied in order to grant access to certain resource or operation
--
-- Generated bindings for @LARight@.
module ObjC.LocalAuthentication.LARight
  ( LARight
  , IsLARight(..)
  , init_
  , initWithRequirement
  , authorizeWithLocalizedReason_completion
  , checkCanAuthorizeWithCompletion
  , deauthorizeWithCompletion
  , state
  , tag
  , setTag
  , initSelector
  , initWithRequirementSelector
  , authorizeWithLocalizedReason_completionSelector
  , checkCanAuthorizeWithCompletionSelector
  , deauthorizeWithCompletionSelector
  , stateSelector
  , tagSelector
  , setTagSelector

  -- * Enum types
  , LARightState(LARightState)
  , pattern LARightStateUnknown
  , pattern LARightStateAuthorizing
  , pattern LARightStateAuthorized
  , pattern LARightStateNotAuthorized

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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Constructs a right using default authorization requirements
--
-- For authorizing a right with default requirements a user will be asked to authenticate using biometry or the device passcode.
--
-- Returns: @LARight@ instance
--
-- ObjC selector: @- init@
init_ :: IsLARight laRight => laRight -> IO (Id LARight)
init_ laRight  =
  sendMsg laRight (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Constructs a right that will be granted only when the given @LAAuthenticationRequirement@ is statisfied.
--
-- @requirement@ — Requirement that needs to be satisfied to authorize the right
--
-- Returns: @LARight@ instance
--
-- ObjC selector: @- initWithRequirement:@
initWithRequirement :: (IsLARight laRight, IsLAAuthenticationRequirement requirement) => laRight -> requirement -> IO (Id LARight)
initWithRequirement laRight  requirement =
withObjCPtr requirement $ \raw_requirement ->
    sendMsg laRight (mkSelector "initWithRequirement:") (retPtr retVoid) [argPtr (castPtr raw_requirement :: Ptr ())] >>= ownedObject . castPtr

-- | Tries to authorize the right.
--
-- @localizedReason@ — Localized explanation for the authorization. Appears in the UI presented to the user.
--
-- @handler@ — Completion handler called after the authorization finishes. Returns an error when the authorization fails.
--
-- ObjC selector: @- authorizeWithLocalizedReason:completion:@
authorizeWithLocalizedReason_completion :: (IsLARight laRight, IsNSString localizedReason) => laRight -> localizedReason -> Ptr () -> IO ()
authorizeWithLocalizedReason_completion laRight  localizedReason handler =
withObjCPtr localizedReason $ \raw_localizedReason ->
    sendMsg laRight (mkSelector "authorizeWithLocalizedReason:completion:") retVoid [argPtr (castPtr raw_localizedReason :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Checks whether the client can eventually be granted the right.
--
-- @handler@ — Completion handler. Returns @nil@ if the right can be authorized or an error otherwise.
--
-- ObjC selector: @- checkCanAuthorizeWithCompletion:@
checkCanAuthorizeWithCompletion :: IsLARight laRight => laRight -> Ptr () -> IO ()
checkCanAuthorizeWithCompletion laRight  handler =
  sendMsg laRight (mkSelector "checkCanAuthorizeWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Invalidates a previously authorized right.
--
-- @handler@ — Completion handler called after the right is deauthorized.
--
-- ObjC selector: @- deauthorizeWithCompletion:@
deauthorizeWithCompletion :: IsLARight laRight => laRight -> Ptr () -> IO ()
deauthorizeWithCompletion laRight  handler =
  sendMsg laRight (mkSelector "deauthorizeWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Provides the current authorization state of the @LARight@ instance
--
-- ObjC selector: @- state@
state :: IsLARight laRight => laRight -> IO LARightState
state laRight  =
  fmap (coerce :: CLong -> LARightState) $ sendMsg laRight (mkSelector "state") retCLong []

-- | An application-supplied integer that can be used to identify right intances. The default value is @0.@
--
-- ObjC selector: @- tag@
tag :: IsLARight laRight => laRight -> IO CLong
tag laRight  =
  sendMsg laRight (mkSelector "tag") retCLong []

-- | An application-supplied integer that can be used to identify right intances. The default value is @0.@
--
-- ObjC selector: @- setTag:@
setTag :: IsLARight laRight => laRight -> CLong -> IO ()
setTag laRight  value =
  sendMsg laRight (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRequirement:@
initWithRequirementSelector :: Selector
initWithRequirementSelector = mkSelector "initWithRequirement:"

-- | @Selector@ for @authorizeWithLocalizedReason:completion:@
authorizeWithLocalizedReason_completionSelector :: Selector
authorizeWithLocalizedReason_completionSelector = mkSelector "authorizeWithLocalizedReason:completion:"

-- | @Selector@ for @checkCanAuthorizeWithCompletion:@
checkCanAuthorizeWithCompletionSelector :: Selector
checkCanAuthorizeWithCompletionSelector = mkSelector "checkCanAuthorizeWithCompletion:"

-- | @Selector@ for @deauthorizeWithCompletion:@
deauthorizeWithCompletionSelector :: Selector
deauthorizeWithCompletionSelector = mkSelector "deauthorizeWithCompletion:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

