{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetRestaurantGuestIntentResponse@.
module ObjC.Intents.INGetRestaurantGuestIntentResponse
  ( INGetRestaurantGuestIntentResponse
  , IsINGetRestaurantGuestIntentResponse(..)
  , initWithCode_userActivity
  , guest
  , setGuest
  , guestDisplayPreferences
  , setGuestDisplayPreferences
  , code
  , initWithCode_userActivitySelector
  , guestSelector
  , setGuestSelector
  , guestDisplayPreferencesSelector
  , setGuestDisplayPreferencesSelector
  , codeSelector

  -- * Enum types
  , INGetRestaurantGuestIntentResponseCode(INGetRestaurantGuestIntentResponseCode)
  , pattern INGetRestaurantGuestIntentResponseCodeSuccess
  , pattern INGetRestaurantGuestIntentResponseCodeFailure

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

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsNSUserActivity userActivity) => inGetRestaurantGuestIntentResponse -> INGetRestaurantGuestIntentResponseCode -> userActivity -> IO (Id INGetRestaurantGuestIntentResponse)
initWithCode_userActivity inGetRestaurantGuestIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inGetRestaurantGuestIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- guest@
guest :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO (Id INRestaurantGuest)
guest inGetRestaurantGuestIntentResponse  =
  sendMsg inGetRestaurantGuestIntentResponse (mkSelector "guest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuest:@
setGuest :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsINRestaurantGuest value) => inGetRestaurantGuestIntentResponse -> value -> IO ()
setGuest inGetRestaurantGuestIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetRestaurantGuestIntentResponse (mkSelector "setGuest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- guestDisplayPreferences@
guestDisplayPreferences :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO (Id INRestaurantGuestDisplayPreferences)
guestDisplayPreferences inGetRestaurantGuestIntentResponse  =
  sendMsg inGetRestaurantGuestIntentResponse (mkSelector "guestDisplayPreferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuestDisplayPreferences:@
setGuestDisplayPreferences :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsINRestaurantGuestDisplayPreferences value) => inGetRestaurantGuestIntentResponse -> value -> IO ()
setGuestDisplayPreferences inGetRestaurantGuestIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetRestaurantGuestIntentResponse (mkSelector "setGuestDisplayPreferences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- code@
code :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO INGetRestaurantGuestIntentResponseCode
code inGetRestaurantGuestIntentResponse  =
  fmap (coerce :: CLong -> INGetRestaurantGuestIntentResponseCode) $ sendMsg inGetRestaurantGuestIntentResponse (mkSelector "code") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @guest@
guestSelector :: Selector
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector
setGuestSelector = mkSelector "setGuest:"

-- | @Selector@ for @guestDisplayPreferences@
guestDisplayPreferencesSelector :: Selector
guestDisplayPreferencesSelector = mkSelector "guestDisplayPreferences"

-- | @Selector@ for @setGuestDisplayPreferences:@
setGuestDisplayPreferencesSelector :: Selector
setGuestDisplayPreferencesSelector = mkSelector "setGuestDisplayPreferences:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

