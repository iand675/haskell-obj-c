{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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
init_ :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO RawId
init_ inSearchForMessagesIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForMessagesIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse, IsNSUserActivity userActivity) => inSearchForMessagesIntentResponse -> INSearchForMessagesIntentResponseCode -> userActivity -> IO (Id INSearchForMessagesIntentResponse)
initWithCode_userActivity inSearchForMessagesIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForMessagesIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO INSearchForMessagesIntentResponseCode
code inSearchForMessagesIntentResponse  =
  fmap (coerce :: CLong -> INSearchForMessagesIntentResponseCode) $ sendMsg inSearchForMessagesIntentResponse (mkSelector "code") retCLong []

-- | @- messages@
messages :: IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse => inSearchForMessagesIntentResponse -> IO (Id NSArray)
messages inSearchForMessagesIntentResponse  =
  sendMsg inSearchForMessagesIntentResponse (mkSelector "messages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessages:@
setMessages :: (IsINSearchForMessagesIntentResponse inSearchForMessagesIntentResponse, IsNSArray value) => inSearchForMessagesIntentResponse -> value -> IO ()
setMessages inSearchForMessagesIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForMessagesIntentResponse (mkSelector "setMessages:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @messages@
messagesSelector :: Selector
messagesSelector = mkSelector "messages"

-- | @Selector@ for @setMessages:@
setMessagesSelector :: Selector
setMessagesSelector = mkSelector "setMessages:"

