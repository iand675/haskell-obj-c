{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingDefaultsIntentResponse@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingDefaultsIntentResponse
  ( INGetAvailableRestaurantReservationBookingDefaultsIntentResponse
  , IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse(..)
  , initWithDefaultPartySize_defaultBookingDate_code_userActivity
  , defaultPartySize
  , defaultBookingDate
  , maximumPartySize
  , setMaximumPartySize
  , minimumPartySize
  , setMinimumPartySize
  , providerImage
  , setProviderImage
  , code
  , codeSelector
  , defaultBookingDateSelector
  , defaultPartySizeSelector
  , initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector
  , maximumPartySizeSelector
  , minimumPartySizeSelector
  , providerImageSelector
  , setMaximumPartySizeSelector
  , setMinimumPartySizeSelector
  , setProviderImageSelector

  -- * Enum types
  , INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode(INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode)
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeSuccess
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeFailure
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeUnspecified

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

-- | @- initWithDefaultPartySize:defaultBookingDate:code:userActivity:@
initWithDefaultPartySize_defaultBookingDate_code_userActivity :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSDate defaultBookingDate, IsNSUserActivity userActivity) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> CULong -> defaultBookingDate -> INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode -> userActivity -> IO (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse)
initWithDefaultPartySize_defaultBookingDate_code_userActivity inGetAvailableRestaurantReservationBookingDefaultsIntentResponse defaultPartySize defaultBookingDate code userActivity =
  sendOwnedMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector defaultPartySize (toNSDate defaultBookingDate) code (toNSUserActivity userActivity)

-- | @- defaultPartySize@
defaultPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO CULong
defaultPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse defaultPartySizeSelector

-- | @- defaultBookingDate@
defaultBookingDate :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSDate)
defaultBookingDate inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse defaultBookingDateSelector

-- | @- maximumPartySize@
maximumPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSNumber)
maximumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse maximumPartySizeSelector

-- | @- setMaximumPartySize:@
setMaximumPartySize :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSNumber value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setMaximumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse setMaximumPartySizeSelector (toNSNumber value)

-- | @- minimumPartySize@
minimumPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSNumber)
minimumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse minimumPartySizeSelector

-- | @- setMinimumPartySize:@
setMinimumPartySize :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSNumber value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setMinimumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse setMinimumPartySizeSelector (toNSNumber value)

-- | @- providerImage@
providerImage :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id INImage)
providerImage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse providerImageSelector

-- | @- setProviderImage:@
setProviderImage :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsINImage value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setProviderImage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse setProviderImageSelector (toINImage value)

-- | @- code@
code :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode
code inGetAvailableRestaurantReservationBookingDefaultsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefaultPartySize:defaultBookingDate:code:userActivity:@
initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector :: Selector '[CULong, Id NSDate, INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode, Id NSUserActivity] (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse)
initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector = mkSelector "initWithDefaultPartySize:defaultBookingDate:code:userActivity:"

-- | @Selector@ for @defaultPartySize@
defaultPartySizeSelector :: Selector '[] CULong
defaultPartySizeSelector = mkSelector "defaultPartySize"

-- | @Selector@ for @defaultBookingDate@
defaultBookingDateSelector :: Selector '[] (Id NSDate)
defaultBookingDateSelector = mkSelector "defaultBookingDate"

-- | @Selector@ for @maximumPartySize@
maximumPartySizeSelector :: Selector '[] (Id NSNumber)
maximumPartySizeSelector = mkSelector "maximumPartySize"

-- | @Selector@ for @setMaximumPartySize:@
setMaximumPartySizeSelector :: Selector '[Id NSNumber] ()
setMaximumPartySizeSelector = mkSelector "setMaximumPartySize:"

-- | @Selector@ for @minimumPartySize@
minimumPartySizeSelector :: Selector '[] (Id NSNumber)
minimumPartySizeSelector = mkSelector "minimumPartySize"

-- | @Selector@ for @setMinimumPartySize:@
setMinimumPartySizeSelector :: Selector '[Id NSNumber] ()
setMinimumPartySizeSelector = mkSelector "setMinimumPartySize:"

-- | @Selector@ for @providerImage@
providerImageSelector :: Selector '[] (Id INImage)
providerImageSelector = mkSelector "providerImage"

-- | @Selector@ for @setProviderImage:@
setProviderImageSelector :: Selector '[Id INImage] ()
setProviderImageSelector = mkSelector "setProviderImage:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode
codeSelector = mkSelector "code"

