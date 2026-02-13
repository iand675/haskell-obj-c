{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageIntentResponse@.
module ObjC.Intents.INSendMessageIntentResponse
  ( INSendMessageIntentResponse
  , IsINSendMessageIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , sentMessages
  , setSentMessages
  , sentMessage
  , setSentMessage
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , sentMessageSelector
  , sentMessagesSelector
  , setSentMessageSelector
  , setSentMessagesSelector

  -- * Enum types
  , INSendMessageIntentResponseCode(INSendMessageIntentResponseCode)
  , pattern INSendMessageIntentResponseCodeUnspecified
  , pattern INSendMessageIntentResponseCodeReady
  , pattern INSendMessageIntentResponseCodeInProgress
  , pattern INSendMessageIntentResponseCodeSuccess
  , pattern INSendMessageIntentResponseCodeFailure
  , pattern INSendMessageIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSendMessageIntentResponseCodeFailureMessageServiceNotAvailable
  , pattern INSendMessageIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO RawId
init_ inSendMessageIntentResponse =
  sendOwnedMessage inSendMessageIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsNSUserActivity userActivity) => inSendMessageIntentResponse -> INSendMessageIntentResponseCode -> userActivity -> IO (Id INSendMessageIntentResponse)
initWithCode_userActivity inSendMessageIntentResponse code userActivity =
  sendOwnedMessage inSendMessageIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO INSendMessageIntentResponseCode
code inSendMessageIntentResponse =
  sendMessage inSendMessageIntentResponse codeSelector

-- | @- sentMessages@
sentMessages :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO (Id NSArray)
sentMessages inSendMessageIntentResponse =
  sendMessage inSendMessageIntentResponse sentMessagesSelector

-- | @- setSentMessages:@
setSentMessages :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsNSArray value) => inSendMessageIntentResponse -> value -> IO ()
setSentMessages inSendMessageIntentResponse value =
  sendMessage inSendMessageIntentResponse setSentMessagesSelector (toNSArray value)

-- | @- sentMessage@
sentMessage :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO (Id INMessage)
sentMessage inSendMessageIntentResponse =
  sendMessage inSendMessageIntentResponse sentMessageSelector

-- | @- setSentMessage:@
setSentMessage :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsINMessage value) => inSendMessageIntentResponse -> value -> IO ()
setSentMessage inSendMessageIntentResponse value =
  sendMessage inSendMessageIntentResponse setSentMessageSelector (toINMessage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSendMessageIntentResponseCode, Id NSUserActivity] (Id INSendMessageIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSendMessageIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @sentMessages@
sentMessagesSelector :: Selector '[] (Id NSArray)
sentMessagesSelector = mkSelector "sentMessages"

-- | @Selector@ for @setSentMessages:@
setSentMessagesSelector :: Selector '[Id NSArray] ()
setSentMessagesSelector = mkSelector "setSentMessages:"

-- | @Selector@ for @sentMessage@
sentMessageSelector :: Selector '[] (Id INMessage)
sentMessageSelector = mkSelector "sentMessage"

-- | @Selector@ for @setSentMessage:@
setSentMessageSelector :: Selector '[Id INMessage] ()
setSentMessageSelector = mkSelector "setSentMessage:"

