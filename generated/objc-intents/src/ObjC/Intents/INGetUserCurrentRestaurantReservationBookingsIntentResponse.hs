{-# LANGUAGE PatternSynonyms #-}
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
  , initWithUserCurrentBookings_code_userActivitySelector
  , codeSelector
  , userCurrentBookingsSelector
  , setUserCurrentBookingsSelector

  -- * Enum types
  , INGetUserCurrentRestaurantReservationBookingsIntentResponseCode(INGetUserCurrentRestaurantReservationBookingsIntentResponseCode)
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeSuccess
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailure
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailureRequestUnsatisfiable
  , pattern INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeUnspecified

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

-- | @- initWithUserCurrentBookings:code:userActivity:@
initWithUserCurrentBookings_code_userActivity :: (IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse, IsNSArray userCurrentBookings, IsNSUserActivity userActivity) => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> userCurrentBookings -> INGetUserCurrentRestaurantReservationBookingsIntentResponseCode -> userActivity -> IO (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse)
initWithUserCurrentBookings_code_userActivity inGetUserCurrentRestaurantReservationBookingsIntentResponse  userCurrentBookings code userActivity =
withObjCPtr userCurrentBookings $ \raw_userCurrentBookings ->
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inGetUserCurrentRestaurantReservationBookingsIntentResponse (mkSelector "initWithUserCurrentBookings:code:userActivity:") (retPtr retVoid) [argPtr (castPtr raw_userCurrentBookings :: Ptr ()), argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> IO INGetUserCurrentRestaurantReservationBookingsIntentResponseCode
code inGetUserCurrentRestaurantReservationBookingsIntentResponse  =
  fmap (coerce :: CLong -> INGetUserCurrentRestaurantReservationBookingsIntentResponseCode) $ sendMsg inGetUserCurrentRestaurantReservationBookingsIntentResponse (mkSelector "code") retCLong []

-- | @- userCurrentBookings@
userCurrentBookings :: IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> IO (Id NSArray)
userCurrentBookings inGetUserCurrentRestaurantReservationBookingsIntentResponse  =
  sendMsg inGetUserCurrentRestaurantReservationBookingsIntentResponse (mkSelector "userCurrentBookings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserCurrentBookings:@
setUserCurrentBookings :: (IsINGetUserCurrentRestaurantReservationBookingsIntentResponse inGetUserCurrentRestaurantReservationBookingsIntentResponse, IsNSArray value) => inGetUserCurrentRestaurantReservationBookingsIntentResponse -> value -> IO ()
setUserCurrentBookings inGetUserCurrentRestaurantReservationBookingsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetUserCurrentRestaurantReservationBookingsIntentResponse (mkSelector "setUserCurrentBookings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUserCurrentBookings:code:userActivity:@
initWithUserCurrentBookings_code_userActivitySelector :: Selector
initWithUserCurrentBookings_code_userActivitySelector = mkSelector "initWithUserCurrentBookings:code:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @userCurrentBookings@
userCurrentBookingsSelector :: Selector
userCurrentBookingsSelector = mkSelector "userCurrentBookings"

-- | @Selector@ for @setUserCurrentBookings:@
setUserCurrentBookingsSelector :: Selector
setUserCurrentBookingsSelector = mkSelector "setUserCurrentBookings:"

