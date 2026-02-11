{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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
init_ :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO RawId
init_ inPlayMediaIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inPlayMediaIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPlayMediaIntentResponse inPlayMediaIntentResponse, IsNSUserActivity userActivity) => inPlayMediaIntentResponse -> INPlayMediaIntentResponseCode -> userActivity -> IO (Id INPlayMediaIntentResponse)
initWithCode_userActivity inPlayMediaIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inPlayMediaIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO INPlayMediaIntentResponseCode
code inPlayMediaIntentResponse  =
  fmap (coerce :: CLong -> INPlayMediaIntentResponseCode) $ sendMsg inPlayMediaIntentResponse (mkSelector "code") retCLong []

-- | @- nowPlayingInfo@
nowPlayingInfo :: IsINPlayMediaIntentResponse inPlayMediaIntentResponse => inPlayMediaIntentResponse -> IO (Id NSDictionary)
nowPlayingInfo inPlayMediaIntentResponse  =
  sendMsg inPlayMediaIntentResponse (mkSelector "nowPlayingInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNowPlayingInfo:@
setNowPlayingInfo :: (IsINPlayMediaIntentResponse inPlayMediaIntentResponse, IsNSDictionary value) => inPlayMediaIntentResponse -> value -> IO ()
setNowPlayingInfo inPlayMediaIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPlayMediaIntentResponse (mkSelector "setNowPlayingInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @nowPlayingInfo@
nowPlayingInfoSelector :: Selector
nowPlayingInfoSelector = mkSelector "nowPlayingInfo"

-- | @Selector@ for @setNowPlayingInfo:@
setNowPlayingInfoSelector :: Selector
setNowPlayingInfoSelector = mkSelector "setNowPlayingInfo:"

