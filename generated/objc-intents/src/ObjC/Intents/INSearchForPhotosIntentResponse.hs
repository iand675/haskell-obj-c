{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForPhotosIntentResponse@.
module ObjC.Intents.INSearchForPhotosIntentResponse
  ( INSearchForPhotosIntentResponse
  , IsINSearchForPhotosIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSearchForPhotosIntentResponseCode(INSearchForPhotosIntentResponseCode)
  , pattern INSearchForPhotosIntentResponseCodeUnspecified
  , pattern INSearchForPhotosIntentResponseCodeReady
  , pattern INSearchForPhotosIntentResponseCodeContinueInApp
  , pattern INSearchForPhotosIntentResponseCodeFailure
  , pattern INSearchForPhotosIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForPhotosIntentResponseCodeFailureAppConfigurationRequired

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
init_ :: IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse => inSearchForPhotosIntentResponse -> IO RawId
init_ inSearchForPhotosIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForPhotosIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse, IsNSUserActivity userActivity) => inSearchForPhotosIntentResponse -> INSearchForPhotosIntentResponseCode -> userActivity -> IO (Id INSearchForPhotosIntentResponse)
initWithCode_userActivity inSearchForPhotosIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForPhotosIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse => inSearchForPhotosIntentResponse -> IO INSearchForPhotosIntentResponseCode
code inSearchForPhotosIntentResponse  =
  fmap (coerce :: CLong -> INSearchForPhotosIntentResponseCode) $ sendMsg inSearchForPhotosIntentResponse (mkSelector "code") retCLong []

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

