{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , guestDisplayPreferencesSelector
  , guestSelector
  , initWithCode_userActivitySelector
  , setGuestDisplayPreferencesSelector
  , setGuestSelector

  -- * Enum types
  , INGetRestaurantGuestIntentResponseCode(INGetRestaurantGuestIntentResponseCode)
  , pattern INGetRestaurantGuestIntentResponseCodeSuccess
  , pattern INGetRestaurantGuestIntentResponseCodeFailure

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsNSUserActivity userActivity) => inGetRestaurantGuestIntentResponse -> INGetRestaurantGuestIntentResponseCode -> userActivity -> IO (Id INGetRestaurantGuestIntentResponse)
initWithCode_userActivity inGetRestaurantGuestIntentResponse code userActivity =
  sendOwnedMessage inGetRestaurantGuestIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- guest@
guest :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO (Id INRestaurantGuest)
guest inGetRestaurantGuestIntentResponse =
  sendMessage inGetRestaurantGuestIntentResponse guestSelector

-- | @- setGuest:@
setGuest :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsINRestaurantGuest value) => inGetRestaurantGuestIntentResponse -> value -> IO ()
setGuest inGetRestaurantGuestIntentResponse value =
  sendMessage inGetRestaurantGuestIntentResponse setGuestSelector (toINRestaurantGuest value)

-- | @- guestDisplayPreferences@
guestDisplayPreferences :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO (Id INRestaurantGuestDisplayPreferences)
guestDisplayPreferences inGetRestaurantGuestIntentResponse =
  sendMessage inGetRestaurantGuestIntentResponse guestDisplayPreferencesSelector

-- | @- setGuestDisplayPreferences:@
setGuestDisplayPreferences :: (IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse, IsINRestaurantGuestDisplayPreferences value) => inGetRestaurantGuestIntentResponse -> value -> IO ()
setGuestDisplayPreferences inGetRestaurantGuestIntentResponse value =
  sendMessage inGetRestaurantGuestIntentResponse setGuestDisplayPreferencesSelector (toINRestaurantGuestDisplayPreferences value)

-- | @- code@
code :: IsINGetRestaurantGuestIntentResponse inGetRestaurantGuestIntentResponse => inGetRestaurantGuestIntentResponse -> IO INGetRestaurantGuestIntentResponseCode
code inGetRestaurantGuestIntentResponse =
  sendMessage inGetRestaurantGuestIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetRestaurantGuestIntentResponseCode, Id NSUserActivity] (Id INGetRestaurantGuestIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @guest@
guestSelector :: Selector '[] (Id INRestaurantGuest)
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector '[Id INRestaurantGuest] ()
setGuestSelector = mkSelector "setGuest:"

-- | @Selector@ for @guestDisplayPreferences@
guestDisplayPreferencesSelector :: Selector '[] (Id INRestaurantGuestDisplayPreferences)
guestDisplayPreferencesSelector = mkSelector "guestDisplayPreferences"

-- | @Selector@ for @setGuestDisplayPreferences:@
setGuestDisplayPreferencesSelector :: Selector '[Id INRestaurantGuestDisplayPreferences] ()
setGuestDisplayPreferencesSelector = mkSelector "setGuestDisplayPreferences:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetRestaurantGuestIntentResponseCode
codeSelector = mkSelector "code"

