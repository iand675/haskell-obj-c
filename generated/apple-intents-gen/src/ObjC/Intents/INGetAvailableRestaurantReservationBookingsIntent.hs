{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingsIntent@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingsIntent
  ( INGetAvailableRestaurantReservationBookingsIntent
  , IsINGetAvailableRestaurantReservationBookingsIntent(..)
  , initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResults
  , restaurant
  , setRestaurant
  , partySize
  , setPartySize
  , preferredBookingDateComponents
  , setPreferredBookingDateComponents
  , maximumNumberOfResults
  , setMaximumNumberOfResults
  , earliestBookingDateForResults
  , setEarliestBookingDateForResults
  , latestBookingDateForResults
  , setLatestBookingDateForResults
  , earliestBookingDateForResultsSelector
  , initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector
  , latestBookingDateForResultsSelector
  , maximumNumberOfResultsSelector
  , partySizeSelector
  , preferredBookingDateComponentsSelector
  , restaurantSelector
  , setEarliestBookingDateForResultsSelector
  , setLatestBookingDateForResultsSelector
  , setMaximumNumberOfResultsSelector
  , setPartySizeSelector
  , setPreferredBookingDateComponentsSelector
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

-- | @- initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:@
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsINRestaurant restaurant, IsNSDateComponents preferredBookingDateComponents, IsNSNumber maximumNumberOfResults, IsNSDate earliestBookingDateForResults, IsNSDate latestBookingDateForResults) => inGetAvailableRestaurantReservationBookingsIntent -> restaurant -> CULong -> preferredBookingDateComponents -> maximumNumberOfResults -> earliestBookingDateForResults -> latestBookingDateForResults -> IO (Id INGetAvailableRestaurantReservationBookingsIntent)
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent restaurant partySize preferredBookingDateComponents maximumNumberOfResults earliestBookingDateForResults latestBookingDateForResults =
  sendOwnedMessage inGetAvailableRestaurantReservationBookingsIntent initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector (toINRestaurant restaurant) partySize (toNSDateComponents preferredBookingDateComponents) (toNSNumber maximumNumberOfResults) (toNSDate earliestBookingDateForResults) (toNSDate latestBookingDateForResults)

-- | @- restaurant@
restaurant :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id INRestaurant)
restaurant inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent restaurantSelector

-- | @- setRestaurant:@
setRestaurant :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsINRestaurant value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setRestaurant inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setRestaurantSelector (toINRestaurant value)

-- | @- partySize@
partySize :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO CULong
partySize inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent partySizeSelector

-- | @- setPartySize:@
setPartySize :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> CULong -> IO ()
setPartySize inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setPartySizeSelector value

-- | @- preferredBookingDateComponents@
preferredBookingDateComponents :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDateComponents)
preferredBookingDateComponents inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent preferredBookingDateComponentsSelector

-- | @- setPreferredBookingDateComponents:@
setPreferredBookingDateComponents :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDateComponents value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setPreferredBookingDateComponents inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setPreferredBookingDateComponentsSelector (toNSDateComponents value)

-- | @- maximumNumberOfResults@
maximumNumberOfResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSNumber)
maximumNumberOfResults inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent maximumNumberOfResultsSelector

-- | @- setMaximumNumberOfResults:@
setMaximumNumberOfResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSNumber value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setMaximumNumberOfResults inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setMaximumNumberOfResultsSelector (toNSNumber value)

-- | @- earliestBookingDateForResults@
earliestBookingDateForResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDate)
earliestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent earliestBookingDateForResultsSelector

-- | @- setEarliestBookingDateForResults:@
setEarliestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDate value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setEarliestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setEarliestBookingDateForResultsSelector (toNSDate value)

-- | @- latestBookingDateForResults@
latestBookingDateForResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDate)
latestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent latestBookingDateForResultsSelector

-- | @- setLatestBookingDateForResults:@
setLatestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDate value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setLatestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntent setLatestBookingDateForResultsSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:@
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector :: Selector '[Id INRestaurant, CULong, Id NSDateComponents, Id NSNumber, Id NSDate, Id NSDate] (Id INGetAvailableRestaurantReservationBookingsIntent)
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector = mkSelector "initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector '[] (Id INRestaurant)
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector '[Id INRestaurant] ()
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector '[] CULong
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @setPartySize:@
setPartySizeSelector :: Selector '[CULong] ()
setPartySizeSelector = mkSelector "setPartySize:"

-- | @Selector@ for @preferredBookingDateComponents@
preferredBookingDateComponentsSelector :: Selector '[] (Id NSDateComponents)
preferredBookingDateComponentsSelector = mkSelector "preferredBookingDateComponents"

-- | @Selector@ for @setPreferredBookingDateComponents:@
setPreferredBookingDateComponentsSelector :: Selector '[Id NSDateComponents] ()
setPreferredBookingDateComponentsSelector = mkSelector "setPreferredBookingDateComponents:"

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

-- | @Selector@ for @latestBookingDateForResults@
latestBookingDateForResultsSelector :: Selector '[] (Id NSDate)
latestBookingDateForResultsSelector = mkSelector "latestBookingDateForResults"

-- | @Selector@ for @setLatestBookingDateForResults:@
setLatestBookingDateForResultsSelector :: Selector '[Id NSDate] ()
setLatestBookingDateForResultsSelector = mkSelector "setLatestBookingDateForResults:"

