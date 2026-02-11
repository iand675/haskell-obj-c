{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSaveProfileInCarIntentResponse@.
module ObjC.Intents.INSaveProfileInCarIntentResponse
  ( INSaveProfileInCarIntentResponse
  , IsINSaveProfileInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSaveProfileInCarIntentResponseCode(INSaveProfileInCarIntentResponseCode)
  , pattern INSaveProfileInCarIntentResponseCodeUnspecified
  , pattern INSaveProfileInCarIntentResponseCodeReady
  , pattern INSaveProfileInCarIntentResponseCodeInProgress
  , pattern INSaveProfileInCarIntentResponseCodeSuccess
  , pattern INSaveProfileInCarIntentResponseCodeFailure
  , pattern INSaveProfileInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse => inSaveProfileInCarIntentResponse -> IO RawId
init_ inSaveProfileInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSaveProfileInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse, IsNSUserActivity userActivity) => inSaveProfileInCarIntentResponse -> INSaveProfileInCarIntentResponseCode -> userActivity -> IO (Id INSaveProfileInCarIntentResponse)
initWithCode_userActivity inSaveProfileInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSaveProfileInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse => inSaveProfileInCarIntentResponse -> IO INSaveProfileInCarIntentResponseCode
code inSaveProfileInCarIntentResponse  =
  fmap (coerce :: CLong -> INSaveProfileInCarIntentResponseCode) $ sendMsg inSaveProfileInCarIntentResponse (mkSelector "code") retCLong []

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

