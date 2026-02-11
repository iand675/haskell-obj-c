{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INListCarsIntentResponse@.
module ObjC.Intents.INListCarsIntentResponse
  ( INListCarsIntentResponse
  , IsINListCarsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , cars
  , setCars
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , carsSelector
  , setCarsSelector

  -- * Enum types
  , INListCarsIntentResponseCode(INListCarsIntentResponseCode)
  , pattern INListCarsIntentResponseCodeUnspecified
  , pattern INListCarsIntentResponseCodeReady
  , pattern INListCarsIntentResponseCodeInProgress
  , pattern INListCarsIntentResponseCodeSuccess
  , pattern INListCarsIntentResponseCodeFailure
  , pattern INListCarsIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO RawId
init_ inListCarsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inListCarsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINListCarsIntentResponse inListCarsIntentResponse, IsNSUserActivity userActivity) => inListCarsIntentResponse -> INListCarsIntentResponseCode -> userActivity -> IO (Id INListCarsIntentResponse)
initWithCode_userActivity inListCarsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inListCarsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO INListCarsIntentResponseCode
code inListCarsIntentResponse  =
  fmap (coerce :: CLong -> INListCarsIntentResponseCode) $ sendMsg inListCarsIntentResponse (mkSelector "code") retCLong []

-- | @- cars@
cars :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO (Id NSArray)
cars inListCarsIntentResponse  =
  sendMsg inListCarsIntentResponse (mkSelector "cars") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCars:@
setCars :: (IsINListCarsIntentResponse inListCarsIntentResponse, IsNSArray value) => inListCarsIntentResponse -> value -> IO ()
setCars inListCarsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inListCarsIntentResponse (mkSelector "setCars:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @cars@
carsSelector :: Selector
carsSelector = mkSelector "cars"

-- | @Selector@ for @setCars:@
setCarsSelector :: Selector
setCarsSelector = mkSelector "setCars:"

