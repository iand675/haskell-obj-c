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
  , initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector
  , restaurantSelector
  , setRestaurantSelector
  , reservationIdentifierSelector
  , setReservationIdentifierSelector
  , maximumNumberOfResultsSelector
  , setMaximumNumberOfResultsSelector
  , earliestBookingDateForResultsSelector
  , setEarliestBookingDateForResultsSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:@
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsINRestaurant restaurant, IsNSString reservationIdentifier, IsNSNumber maximumNumberOfResults, IsNSDate earliestBookingDateForResults) => inGetUserCurrentRestaurantReservationBookingsIntent -> restaurant -> reservationIdentifier -> maximumNumberOfResults -> earliestBookingDateForResults -> IO (Id INGetUserCurrentRestaurantReservationBookingsIntent)
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent  restaurant reservationIdentifier maximumNumberOfResults earliestBookingDateForResults =
withObjCPtr restaurant $ \raw_restaurant ->
  withObjCPtr reservationIdentifier $ \raw_reservationIdentifier ->
    withObjCPtr maximumNumberOfResults $ \raw_maximumNumberOfResults ->
      withObjCPtr earliestBookingDateForResults $ \raw_earliestBookingDateForResults ->
          sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ()), argPtr (castPtr raw_reservationIdentifier :: Ptr ()), argPtr (castPtr raw_maximumNumberOfResults :: Ptr ()), argPtr (castPtr raw_earliestBookingDateForResults :: Ptr ())] >>= ownedObject . castPtr

-- | @- restaurant@
restaurant :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id INRestaurant)
restaurant inGetUserCurrentRestaurantReservationBookingsIntent  =
  sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "restaurant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurant:@
setRestaurant :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsINRestaurant value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setRestaurant inGetUserCurrentRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "setRestaurant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reservationIdentifier@
reservationIdentifier :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSString)
reservationIdentifier inGetUserCurrentRestaurantReservationBookingsIntent  =
  sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "reservationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReservationIdentifier:@
setReservationIdentifier :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSString value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setReservationIdentifier inGetUserCurrentRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "setReservationIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumNumberOfResults@
maximumNumberOfResults :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSNumber)
maximumNumberOfResults inGetUserCurrentRestaurantReservationBookingsIntent  =
  sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "maximumNumberOfResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumNumberOfResults:@
setMaximumNumberOfResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSNumber value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setMaximumNumberOfResults inGetUserCurrentRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "setMaximumNumberOfResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- earliestBookingDateForResults@
earliestBookingDateForResults :: IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent => inGetUserCurrentRestaurantReservationBookingsIntent -> IO (Id NSDate)
earliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent  =
  sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "earliestBookingDateForResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEarliestBookingDateForResults:@
setEarliestBookingDateForResults :: (IsINGetUserCurrentRestaurantReservationBookingsIntent inGetUserCurrentRestaurantReservationBookingsIntent, IsNSDate value) => inGetUserCurrentRestaurantReservationBookingsIntent -> value -> IO ()
setEarliestBookingDateForResults inGetUserCurrentRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetUserCurrentRestaurantReservationBookingsIntent (mkSelector "setEarliestBookingDateForResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:@
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector :: Selector
initWithRestaurant_reservationIdentifier_maximumNumberOfResults_earliestBookingDateForResultsSelector = mkSelector "initWithRestaurant:reservationIdentifier:maximumNumberOfResults:earliestBookingDateForResults:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @reservationIdentifier@
reservationIdentifierSelector :: Selector
reservationIdentifierSelector = mkSelector "reservationIdentifier"

-- | @Selector@ for @setReservationIdentifier:@
setReservationIdentifierSelector :: Selector
setReservationIdentifierSelector = mkSelector "setReservationIdentifier:"

-- | @Selector@ for @maximumNumberOfResults@
maximumNumberOfResultsSelector :: Selector
maximumNumberOfResultsSelector = mkSelector "maximumNumberOfResults"

-- | @Selector@ for @setMaximumNumberOfResults:@
setMaximumNumberOfResultsSelector :: Selector
setMaximumNumberOfResultsSelector = mkSelector "setMaximumNumberOfResults:"

-- | @Selector@ for @earliestBookingDateForResults@
earliestBookingDateForResultsSelector :: Selector
earliestBookingDateForResultsSelector = mkSelector "earliestBookingDateForResults"

-- | @Selector@ for @setEarliestBookingDateForResults:@
setEarliestBookingDateForResultsSelector :: Selector
setEarliestBookingDateForResultsSelector = mkSelector "setEarliestBookingDateForResults:"

