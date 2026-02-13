{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetUserCurrentRestaurantReservationBookingsIntentResponse@.
module ObjC.Intents.INGetUserCurrentRestaurantReservationBookingsIntentResponse
  ( INGetUserCurrentRestaurantReservationBookingsIntentResponse
  , IsINGetUserCurrentRestaurantReservationBookingsIntentResponse(..)
  , initWithUserCurrentBookings_code_userActivity
  , code
  , userCurrentBookings
  , setUserCurrentBookings
  , codeSelector
  , initWithUserCurrentBookings_code_userActivitySelector
  , setUserCurrentBookingsSelector
  , userCurrentBookingsSelector

  -- * Enum types
  , INGetUserCurrentRestaurantReservationBookingsIntentResponseCode(INGetUserCurrentRestaurantReservationBookingsIntentResponseCode)
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeSuccess
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailure
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailureRequestUnsatisfiable
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeUnspecified

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

-- | @- initWithUserCurrentBookings:code:userActivity:@
initWithUserCurrentBookings_code_userActivity :: (IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse, IsNSArray userCurrentBookings, IsNSUserActivity userActivity) => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> userCurrentBookings -> INGetUserCurrentRestaurantReservationBookingsIntentResponseCode -> userActivity -> IO (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse)
initWithUserCurrentBookings_code_userActivity inGetUserCurrentRestaurantReservationBookingsIntentResponse userCurrentBookings code userActivity =
  sendOwnedMessage inGetUserCurrentRestaurantReservationBookingsIntentResponse initWithUserCurrentBookings_code_userActivitySelector (toNSArray userCurrentBookings) code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> IO INGetUserCurrentRestaurantReservationBookingsIntentResponseCode
code inGetUserCurrentRestaurantReservationBookingsIntentResponse =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntentResponse codeSelector

-- | @- userCurrentBookings@
userCurrentBookings :: IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> IO (Id NSArray)
userCurrentBookings inGetUserCurrentRestaurantReservationBookingsIntentResponse =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntentResponse userCurrentBookingsSelector

-- | @- setUserCurrentBookings:@
setUserCurrentBookings :: (IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse, IsNSArray value) => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> value -> IO ()
setUserCurrentBookings inGetUserCurrentRestaurantReservationBookingsIntentResponse value =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntentResponse setUserCurrentBookingsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUserCurrentBookings:code:userActivity:@
initWithUserCurrentBookings_code_userActivitySelector :: Selector '[Id NSArray, INGetUserCurrentRestaurantReservationBookingsIntentResponseCode, Id NSUserActivity] (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse)
initWithUserCurrentBookings_code_userActivitySelector = mkSelector "initWithUserCurrentBookings:code:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetUserCurrentRestaurantReservationBookingsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @userCurrentBookings@
userCurrentBookingsSelector :: Selector '[] (Id NSArray)
userCurrentBookingsSelector = mkSelector "userCurrentBookings"

-- | @Selector@ for @setUserCurrentBookings:@
setUserCurrentBookingsSelector :: Selector '[Id NSArray] ()
setUserCurrentBookingsSelector = mkSelector "setUserCurrentBookings:"

