{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUnsendMessagesIntentResponse@.
module ObjC.Intents.INUnsendMessagesIntentResponse
  ( INUnsendMessagesIntentResponse
  , IsINUnsendMessagesIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INUnsendMessagesIntentResponseCode(INUnsendMessagesIntentResponseCode)
  , pattern INUnsendMessagesIntentResponseCodeUnspecified
  , pattern INUnsendMessagesIntentResponseCodeReady
  , pattern INUnsendMessagesIntentResponseCodeInProgress
  , pattern INUnsendMessagesIntentResponseCodeSuccess
  , pattern INUnsendMessagesIntentResponseCodeFailure
  , pattern INUnsendMessagesIntentResponseCodeFailureRequiringAppLaunch
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageNotFound
  , pattern INUnsendMessagesIntentResponseCodeFailurePastUnsendTimeLimit
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageTypeUnsupported
  , pattern INUnsendMessagesIntentResponseCodeFailureUnsupportedOnService
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageServiceNotAvailable
  , pattern INUnsendMessagesIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse => inUnsendMessagesIntentResponse -> IO RawId
init_ inUnsendMessagesIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inUnsendMessagesIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse, IsNSUserActivity userActivity) => inUnsendMessagesIntentResponse -> INUnsendMessagesIntentResponseCode -> userActivity -> IO (Id INUnsendMessagesIntentResponse)
initWithCode_userActivity inUnsendMessagesIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inUnsendMessagesIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse => inUnsendMessagesIntentResponse -> IO INUnsendMessagesIntentResponseCode
code inUnsendMessagesIntentResponse  =
  fmap (coerce :: CLong -> INUnsendMessagesIntentResponseCode) $ sendMsg inUnsendMessagesIntentResponse (mkSelector "code") retCLong []

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

