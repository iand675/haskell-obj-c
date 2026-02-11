{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INHangUpCallIntentResponse@.
module ObjC.Intents.INHangUpCallIntentResponse
  ( INHangUpCallIntentResponse
  , IsINHangUpCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INHangUpCallIntentResponseCode(INHangUpCallIntentResponseCode)
  , pattern INHangUpCallIntentResponseCodeUnspecified
  , pattern INHangUpCallIntentResponseCodeReady
  , pattern INHangUpCallIntentResponseCodeInProgress
  , pattern INHangUpCallIntentResponseCodeSuccess
  , pattern INHangUpCallIntentResponseCodeFailure
  , pattern INHangUpCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INHangUpCallIntentResponseCodeFailureNoCallToHangUp

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
init_ :: IsINHangUpCallIntentResponse inHangUpCallIntentResponse => inHangUpCallIntentResponse -> IO RawId
init_ inHangUpCallIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inHangUpCallIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINHangUpCallIntentResponse inHangUpCallIntentResponse, IsNSUserActivity userActivity) => inHangUpCallIntentResponse -> INHangUpCallIntentResponseCode -> userActivity -> IO (Id INHangUpCallIntentResponse)
initWithCode_userActivity inHangUpCallIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inHangUpCallIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINHangUpCallIntentResponse inHangUpCallIntentResponse => inHangUpCallIntentResponse -> IO INHangUpCallIntentResponseCode
code inHangUpCallIntentResponse  =
  fmap (coerce :: CLong -> INHangUpCallIntentResponseCode) $ sendMsg inHangUpCallIntentResponse (mkSelector "code") retCLong []

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

