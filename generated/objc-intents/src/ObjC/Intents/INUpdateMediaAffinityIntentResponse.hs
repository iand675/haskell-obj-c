{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpdateMediaAffinityIntentResponse@.
module ObjC.Intents.INUpdateMediaAffinityIntentResponse
  ( INUpdateMediaAffinityIntentResponse
  , IsINUpdateMediaAffinityIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INUpdateMediaAffinityIntentResponseCode(INUpdateMediaAffinityIntentResponseCode)
  , pattern INUpdateMediaAffinityIntentResponseCodeUnspecified
  , pattern INUpdateMediaAffinityIntentResponseCodeReady
  , pattern INUpdateMediaAffinityIntentResponseCodeInProgress
  , pattern INUpdateMediaAffinityIntentResponseCodeSuccess
  , pattern INUpdateMediaAffinityIntentResponseCodeFailure
  , pattern INUpdateMediaAffinityIntentResponseCodeFailureRequiringAppLaunch

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse => inUpdateMediaAffinityIntentResponse -> IO RawId
init_ inUpdateMediaAffinityIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inUpdateMediaAffinityIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse, IsNSUserActivity userActivity) => inUpdateMediaAffinityIntentResponse -> INUpdateMediaAffinityIntentResponseCode -> userActivity -> IO (Id INUpdateMediaAffinityIntentResponse)
initWithCode_userActivity inUpdateMediaAffinityIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inUpdateMediaAffinityIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse => inUpdateMediaAffinityIntentResponse -> IO INUpdateMediaAffinityIntentResponseCode
code inUpdateMediaAffinityIntentResponse  =
  fmap (coerce :: CLong -> INUpdateMediaAffinityIntentResponseCode) $ sendMsg inUpdateMediaAffinityIntentResponse (mkSelector "code") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

