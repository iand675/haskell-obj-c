{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaIntentResponse@.
module ObjC.Intents.INAddMediaIntentResponse
  ( INAddMediaIntentResponse
  , IsINAddMediaIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INAddMediaIntentResponseCode(INAddMediaIntentResponseCode)
  , pattern INAddMediaIntentResponseCodeUnspecified
  , pattern INAddMediaIntentResponseCodeReady
  , pattern INAddMediaIntentResponseCodeInProgress
  , pattern INAddMediaIntentResponseCodeSuccess
  , pattern INAddMediaIntentResponseCodeHandleInApp
  , pattern INAddMediaIntentResponseCodeFailure
  , pattern INAddMediaIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAddMediaIntentResponse inAddMediaIntentResponse => inAddMediaIntentResponse -> IO RawId
init_ inAddMediaIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inAddMediaIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAddMediaIntentResponse inAddMediaIntentResponse, IsNSUserActivity userActivity) => inAddMediaIntentResponse -> INAddMediaIntentResponseCode -> userActivity -> IO (Id INAddMediaIntentResponse)
initWithCode_userActivity inAddMediaIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inAddMediaIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINAddMediaIntentResponse inAddMediaIntentResponse => inAddMediaIntentResponse -> IO INAddMediaIntentResponseCode
code inAddMediaIntentResponse  =
  fmap (coerce :: CLong -> INAddMediaIntentResponseCode) $ sendMsg inAddMediaIntentResponse (mkSelector "code") retCLong []

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

