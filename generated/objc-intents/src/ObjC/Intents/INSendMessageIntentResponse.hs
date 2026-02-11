{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , sentMessagesSelector
  , setSentMessagesSelector
  , sentMessageSelector
  , setSentMessageSelector

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
init_ :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO RawId
init_ inSendMessageIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSendMessageIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsNSUserActivity userActivity) => inSendMessageIntentResponse -> INSendMessageIntentResponseCode -> userActivity -> IO (Id INSendMessageIntentResponse)
initWithCode_userActivity inSendMessageIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSendMessageIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO INSendMessageIntentResponseCode
code inSendMessageIntentResponse  =
  fmap (coerce :: CLong -> INSendMessageIntentResponseCode) $ sendMsg inSendMessageIntentResponse (mkSelector "code") retCLong []

-- | @- sentMessages@
sentMessages :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO (Id NSArray)
sentMessages inSendMessageIntentResponse  =
  sendMsg inSendMessageIntentResponse (mkSelector "sentMessages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSentMessages:@
setSentMessages :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsNSArray value) => inSendMessageIntentResponse -> value -> IO ()
setSentMessages inSendMessageIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSendMessageIntentResponse (mkSelector "setSentMessages:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sentMessage@
sentMessage :: IsINSendMessageIntentResponse inSendMessageIntentResponse => inSendMessageIntentResponse -> IO (Id INMessage)
sentMessage inSendMessageIntentResponse  =
  sendMsg inSendMessageIntentResponse (mkSelector "sentMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSentMessage:@
setSentMessage :: (IsINSendMessageIntentResponse inSendMessageIntentResponse, IsINMessage value) => inSendMessageIntentResponse -> value -> IO ()
setSentMessage inSendMessageIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSendMessageIntentResponse (mkSelector "setSentMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @sentMessages@
sentMessagesSelector :: Selector
sentMessagesSelector = mkSelector "sentMessages"

-- | @Selector@ for @setSentMessages:@
setSentMessagesSelector :: Selector
setSentMessagesSelector = mkSelector "setSentMessages:"

-- | @Selector@ for @sentMessage@
sentMessageSelector :: Selector
sentMessageSelector = mkSelector "sentMessage"

-- | @Selector@ for @setSentMessage:@
setSentMessageSelector :: Selector
setSentMessageSelector = mkSelector "setSentMessage:"

