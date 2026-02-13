{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMediaIntentResponse@.
module ObjC.Intents.INSearchForMediaIntentResponse
  ( INSearchForMediaIntentResponse
  , IsINSearchForMediaIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , mediaItems
  , setMediaItems
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , mediaItemsSelector
  , setMediaItemsSelector

  -- * Enum types
  , INSearchForMediaIntentResponseCode(INSearchForMediaIntentResponseCode)
  , pattern INSearchForMediaIntentResponseCodeUnspecified
  , pattern INSearchForMediaIntentResponseCodeReady
  , pattern INSearchForMediaIntentResponseCodeContinueInApp
  , pattern INSearchForMediaIntentResponseCodeInProgress
  , pattern INSearchForMediaIntentResponseCodeSuccess
  , pattern INSearchForMediaIntentResponseCodeFailure
  , pattern INSearchForMediaIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO RawId
init_ inSearchForMediaIntentResponse =
  sendOwnedMessage inSearchForMediaIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse, IsNSUserActivity userActivity) => inSearchForMediaIntentResponse -> INSearchForMediaIntentResponseCode -> userActivity -> IO (Id INSearchForMediaIntentResponse)
initWithCode_userActivity inSearchForMediaIntentResponse code userActivity =
  sendOwnedMessage inSearchForMediaIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO INSearchForMediaIntentResponseCode
code inSearchForMediaIntentResponse =
  sendMessage inSearchForMediaIntentResponse codeSelector

-- | @- mediaItems@
mediaItems :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO (Id NSArray)
mediaItems inSearchForMediaIntentResponse =
  sendMessage inSearchForMediaIntentResponse mediaItemsSelector

-- | @- setMediaItems:@
setMediaItems :: (IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse, IsNSArray value) => inSearchForMediaIntentResponse -> value -> IO ()
setMediaItems inSearchForMediaIntentResponse value =
  sendMessage inSearchForMediaIntentResponse setMediaItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForMediaIntentResponseCode, Id NSUserActivity] (Id INSearchForMediaIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForMediaIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @setMediaItems:@
setMediaItemsSelector :: Selector '[Id NSArray] ()
setMediaItemsSelector = mkSelector "setMediaItems:"

