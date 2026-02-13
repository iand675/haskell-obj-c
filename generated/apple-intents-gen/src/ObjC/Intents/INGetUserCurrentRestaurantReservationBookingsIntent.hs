{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetUserCurrentRestaurantReservationBookingsIntent@.
module ObjC.Intents.INGetUserCurrentRestaurantReservationBookingsIntent
  ( INGetUserCurrentRestaurantReservationBookingsIntent
  , IsINGetUserCurrentRestaurantReservationBookingsIntent(..)
  , initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResults
  , restaurant
  , setRestaurant
  , reservationIdentifier
  , setReservationIdentifier
  , maximumNumberOfResults
  , setMaximumNumberOfResults
  , earliestBookingDateForResults
  , setEarliestBookingDateForResults
  , earliestBookingDateForResultsSelector
  , initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector
  , maximumNumberOfResultsSelector
  , reservationIdentifierSelector
  , restaurantSelector
  , setEarliestBookingDateForResultsSelector
  , setMaximumNumberOfResultsSelector
  , setReservationIdentifierSelector
  , setRestaurantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:@
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsINRestaurant restaurant, IsNSString reservationIdentifier, IsNSNumber maximumNumberOfResults, IsNSDate earliestBookingDateForResults) => inGetUserCurrentRestaurantReservationBookingsIntent -> restaurant -> reservationIdentifier -> maximumNumberOfResults -> earliestBookingDateForResults -> IO (Id INGetUserCurrentRestaurantReservationBookingsIntent)
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent restaurant reservationIdentifier maximumNumberOfResults earliestBookingDateForResults =
  sendOwnedMessage inGetUserCurrentRestaurantReservationBookingsIntent initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector (toINRestaurant restaurant) (toNSString reservationIdentifier) (toNSNumber maximumNumberOfResults) (toNSDate earliestBookingDateForResults)

-- | @- restaurant@
restaurant :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id INRestaurant)
restaurant inGetUserCurrentRestaurantReservationBookingsIntent =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent restaurantSelector

-- | @- setRestaurant:@
setRestaurant :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsINRestaurant value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setRestaurant inGetUserCurrentRestaurantReservationBookingsIntent value =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent setRestaurantSelector (toINRestaurant value)

-- | @- reservationIdentifier@
reservationIdentifier :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSString)
reservationIdentifier inGetUserCurrentRestaurantReservationBookingsIntent =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent reservationIdentifierSelector

-- | @- setReservationIdentifier:@
setReservationIdentifier :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSString value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setReservationIdentifier inGetUserCurrentRestaurantReservationBookingsIntent value =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent setReservationIdentifierSelector (toNSString value)

-- | @- maximumNumberOfResults@
maximumNumberOfResults :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSNumber)
maximumNumberOfResults inGetUserCurrentRestaurantReservationBookingsIntent =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent maximumNumberOfResultsSelector

-- | @- setMaximumNumberOfResults:@
setMaximumNumberOfResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSNumber value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setMaximumNumberOfResults inGetUserCurrentRestaurantReservationBookingsIntent value =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent setMaximumNumberOfResultsSelector (toNSNumber value)

-- | @- earliestBookingDateForResults@
earliestBookingDateForResults :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSDate)
earliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent earliestBookingDateForResultsSelector

-- | @- setEarliestBookingDateForResults:@
setEarliestBookingDateForResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSDate value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setEarliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent value =
  sendMessage inGetUserCurrentRestaurantReservationBookingsIntent setEarliestBookingDateForResultsSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:@
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector :: Selector '[Id INRestaurant, Id NSString, Id NSNumber, Id NSDate] (Id INGetUserCurrentRestaurantReservationBookingsIntent)
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector = mkSelector "initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector '[] (Id INRestaurant)
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector '[Id INRestaurant] ()
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @reservationIdentifier@
reservationIdentifierSelector :: Selector '[] (Id NSString)
reservationIdentifierSelector = mkSelector "reservationIdentifier"

-- | @Selector@ for @setReservationIdentifier:@
setReservationIdentifierSelector :: Selector '[Id NSString] ()
setReservationIdentifierSelector = mkSelector "setReservationIdentifier:"

-- | @Selector@ for @maximumNumberOfResults@
maximumNumberOfResultsSelector :: Selector '[] (Id NSNumber)
maximumNumberOfResultsSelector = mkSelector "maximumNumberOfResults"

-- | @Selector@ for @setMaximumNumberOfResults:@
setMaximumNumberOfResultsSelector :: Selector '[Id NSNumber] ()
setMaximumNumberOfResultsSelector = mkSelector "setMaximumNumberOfResults:"

-- | @Selector@ for @earliestBookingDateForResults@
earliestBookingDateForResultsSelector :: Selector '[] (Id NSDate)
earliestBookingDateForResultsSelector = mkSelector "earliestBookingDateForResults"

-- | @Selector@ for @setEarliestBookingDateForResults:@
setEarliestBookingDateForResultsSelector :: Selector '[Id NSDate] ()
setEarliestBookingDateForResultsSelector = mkSelector "setEarliestBookingDateForResults:"

