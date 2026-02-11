{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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
init_ :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO RawId
init_ inSearchForMediaIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForMediaIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse, IsNSUserActivity userActivity) => inSearchForMediaIntentResponse -> INSearchForMediaIntentResponseCode -> userActivity -> IO (Id INSearchForMediaIntentResponse)
initWithCode_userActivity inSearchForMediaIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForMediaIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO INSearchForMediaIntentResponseCode
code inSearchForMediaIntentResponse  =
  fmap (coerce :: CLong -> INSearchForMediaIntentResponseCode) $ sendMsg inSearchForMediaIntentResponse (mkSelector "code") retCLong []

-- | @- mediaItems@
mediaItems :: IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse => inSearchForMediaIntentResponse -> IO (Id NSArray)
mediaItems inSearchForMediaIntentResponse  =
  sendMsg inSearchForMediaIntentResponse (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMediaItems:@
setMediaItems :: (IsINSearchForMediaIntentResponse inSearchForMediaIntentResponse, IsNSArray value) => inSearchForMediaIntentResponse -> value -> IO ()
setMediaItems inSearchForMediaIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForMediaIntentResponse (mkSelector "setMediaItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @setMediaItems:@
setMediaItemsSelector :: Selector
setMediaItemsSelector = mkSelector "setMediaItems:"

