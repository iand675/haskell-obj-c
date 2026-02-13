{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlayMediaIntentResponse@.
module ObjC.Intents.INPlayMediaIntentResponse
  ( INPlayMediaIntentResponse
  , IsINPlayMediaIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , nowPlayingInfo
  , setNowPlayingInfo
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , nowPlayingInfoSelector
  , setNowPlayingInfoSelector

  -- * Enum types
  , INPlayMediaIntentResponseCode(INPlayMediaIntentResponseCode)
  , pattern INPlayMediaIntentResponseCodeUnspecified
  , pattern INPlayMediaIntentResponseCodeReady
  , pattern INPlayMediaIntentResponseCodeContinueInApp
  , pattern INPlayMediaIntentResponseCodeInProgress
  , pattern INPlayMediaIntentResponseCodeSuccess
  , pattern INPlayMediaIntentResponseCodeHandleInApp
  , pattern INPlayMediaIntentResponseCodeFailure
  , pattern INPlayMediaIntentResponseCodeFailureRequiringAppLaunch
  , pattern INPlayMediaIntentResponseCodeFailureUnknownMediaType
  , pattern INPlayMediaIntentResponseCodeFailureNoUnplayedContent
  , pattern INPlayMediaIntentResponseCodeFailureRestrictedContent
  , pattern INPlayMediaIntentResponseCodeFailureMaxStreamLimitReached

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO RawId
init_ inPlayMediaIntentResponse =
  sendOwnedMessage inPlayMediaIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPlayMediaIntentResponse inPlayMediaIntentResponse, IsNSUserActivity userActivity) => inPlayMediaIntentResponse -> INPlayMediaIntentResponseCode -> userActivity -> IO (Id INPlayMediaIntentResponse)
initWithCode_userActivity inPlayMediaIntentResponse code userActivity =
  sendOwnedMessage inPlayMediaIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO INPlayMediaIntentResponseCode
code inPlayMediaIntentResponse =
  sendMessage inPlayMediaIntentResponse codeSelector

-- | @- nowPlayingInfo@
nowPlayingInfo :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO (Id NSDictionary)
nowPlayingInfo inPlayMediaIntentResponse =
  sendMessage inPlayMediaIntentResponse nowPlayingInfoSelector

-- | @- setNowPlayingInfo:@
setNowPlayingInfo :: (IsINPlayMediaIntentResponse inPlayMediaIntentResponse, IsNSDictionary value) => inPlayMediaIntentResponse -> value -> IO ()
setNowPlayingInfo inPlayMediaIntentResponse value =
  sendMessage inPlayMediaIntentResponse setNowPlayingInfoSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INPlayMediaIntentResponseCode, Id NSUserActivity] (Id INPlayMediaIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INPlayMediaIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @nowPlayingInfo@
nowPlayingInfoSelector :: Selector '[] (Id NSDictionary)
nowPlayingInfoSelector = mkSelector "nowPlayingInfo"

-- | @Selector@ for @setNowPlayingInfo:@
setNowPlayingInfoSelector :: Selector '[Id NSDictionary] ()
setNowPlayingInfoSelector = mkSelector "setNowPlayingInfo:"

