{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMessagesIntentResponse@.
module ObjC.Intents.INSearchForMessagesIntentResponse
  ( INSearchForMessagesIntentResponse
  , IsINSearchForMessagesIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , messages
  , setMessages
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , messagesSelector
  , setMessagesSelector

  -- * Enum types
  , INSearchForMessagesIntentResponseCode(INSearchForMessagesIntentResponseCode)
  , pattern INSearchForMessagesIntentResponseCodeUnspecified
  , pattern INSearchForMessagesIntentResponseCodeReady
  , pattern INSearchForMessagesIntentResponseCodeInProgress
  , pattern INSearchForMessagesIntentResponseCodeSuccess
  , pattern INSearchForMessagesIntentResponseCodeFailure
  , pattern INSearchForMessagesIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForMessagesIntentResponseCodeFailureMessageServiceNotAvailable
  , pattern INSearchForMessagesIntentResponseCodeFailureMessageTooManyResults
  , pattern INSearchForMessagesIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO RawId
init_ inSearchForMessagesIntentResponse =
  sendOwnedMessage inSearchForMessagesIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse, IsNSUserActivity userActivity) => inSearchForMessagesIntentResponse -> INSearchForMessagesIntentResponseCode -> userActivity -> IO (Id INSearchForMessagesIntentResponse)
initWithCode_userActivity inSearchForMessagesIntentResponse code userActivity =
  sendOwnedMessage inSearchForMessagesIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO INSearchForMessagesIntentResponseCode
code inSearchForMessagesIntentResponse =
  sendMessage inSearchForMessagesIntentResponse codeSelector

-- | @- messages@
messages :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO (Id NSArray)
messages inSearchForMessagesIntentResponse =
  sendMessage inSearchForMessagesIntentResponse messagesSelector

-- | @- setMessages:@
setMessages :: (IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse, IsNSArray value) => inSearchForMessagesIntentResponse -> value -> IO ()
setMessages inSearchForMessagesIntentResponse value =
  sendMessage inSearchForMessagesIntentResponse setMessagesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForMessagesIntentResponseCode, Id NSUserActivity] (Id INSearchForMessagesIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForMessagesIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @messages@
messagesSelector :: Selector '[] (Id NSArray)
messagesSelector = mkSelector "messages"

-- | @Selector@ for @setMessages:@
setMessagesSelector :: Selector '[Id NSArray] ()
setMessagesSelector = mkSelector "setMessages:"

