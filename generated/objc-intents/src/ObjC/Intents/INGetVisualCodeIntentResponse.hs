{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetVisualCodeIntentResponse@.
module ObjC.Intents.INGetVisualCodeIntentResponse
  ( INGetVisualCodeIntentResponse
  , IsINGetVisualCodeIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , visualCodeImage
  , setVisualCodeImage
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , visualCodeImageSelector
  , setVisualCodeImageSelector

  -- * Enum types
  , INGetVisualCodeIntentResponseCode(INGetVisualCodeIntentResponseCode)
  , pattern INGetVisualCodeIntentResponseCodeUnspecified
  , pattern INGetVisualCodeIntentResponseCodeReady
  , pattern INGetVisualCodeIntentResponseCodeContinueInApp
  , pattern INGetVisualCodeIntentResponseCodeInProgress
  , pattern INGetVisualCodeIntentResponseCodeSuccess
  , pattern INGetVisualCodeIntentResponseCodeFailure
  , pattern INGetVisualCodeIntentResponseCodeFailureRequiringAppLaunch
  , pattern INGetVisualCodeIntentResponseCodeFailureAppConfigurationRequired

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
init_ :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO RawId
init_ inGetVisualCodeIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inGetVisualCodeIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse, IsNSUserActivity userActivity) => inGetVisualCodeIntentResponse -> INGetVisualCodeIntentResponseCode -> userActivity -> IO (Id INGetVisualCodeIntentResponse)
initWithCode_userActivity inGetVisualCodeIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inGetVisualCodeIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO INGetVisualCodeIntentResponseCode
code inGetVisualCodeIntentResponse  =
  fmap (coerce :: CLong -> INGetVisualCodeIntentResponseCode) $ sendMsg inGetVisualCodeIntentResponse (mkSelector "code") retCLong []

-- | @- visualCodeImage@
visualCodeImage :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO (Id INImage)
visualCodeImage inGetVisualCodeIntentResponse  =
  sendMsg inGetVisualCodeIntentResponse (mkSelector "visualCodeImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVisualCodeImage:@
setVisualCodeImage :: (IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse, IsINImage value) => inGetVisualCodeIntentResponse -> value -> IO ()
setVisualCodeImage inGetVisualCodeIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetVisualCodeIntentResponse (mkSelector "setVisualCodeImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @visualCodeImage@
visualCodeImageSelector :: Selector
visualCodeImageSelector = mkSelector "visualCodeImage"

-- | @Selector@ for @setVisualCodeImage:@
setVisualCodeImageSelector :: Selector
setVisualCodeImageSelector = mkSelector "setVisualCodeImage:"

