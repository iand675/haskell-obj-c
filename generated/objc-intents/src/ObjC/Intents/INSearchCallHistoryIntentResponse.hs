{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchCallHistoryIntentResponse@.
module ObjC.Intents.INSearchCallHistoryIntentResponse
  ( INSearchCallHistoryIntentResponse
  , IsINSearchCallHistoryIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSearchCallHistoryIntentResponseCode(INSearchCallHistoryIntentResponseCode)
  , pattern INSearchCallHistoryIntentResponseCodeUnspecified
  , pattern INSearchCallHistoryIntentResponseCodeReady
  , pattern INSearchCallHistoryIntentResponseCodeContinueInApp
  , pattern INSearchCallHistoryIntentResponseCodeFailure
  , pattern INSearchCallHistoryIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchCallHistoryIntentResponseCodeFailureAppConfigurationRequired
  , pattern INSearchCallHistoryIntentResponseCodeInProgress
  , pattern INSearchCallHistoryIntentResponseCodeSuccess

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
init_ :: IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse => inSearchCallHistoryIntentResponse -> IO RawId
init_ inSearchCallHistoryIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchCallHistoryIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse, IsNSUserActivity userActivity) => inSearchCallHistoryIntentResponse -> INSearchCallHistoryIntentResponseCode -> userActivity -> IO (Id INSearchCallHistoryIntentResponse)
initWithCode_userActivity inSearchCallHistoryIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchCallHistoryIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse => inSearchCallHistoryIntentResponse -> IO INSearchCallHistoryIntentResponseCode
code inSearchCallHistoryIntentResponse  =
  fmap (coerce :: CLong -> INSearchCallHistoryIntentResponseCode) $ sendMsg inSearchCallHistoryIntentResponse (mkSelector "code") retCLong []

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

