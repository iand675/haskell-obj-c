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
  , initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector
  , restaurantSelector
  , setRestaurantSelector
  , partySizeSelector
  , setPartySizeSelector
  , preferredBookingDateComponentsSelector
  , setPreferredBookingDateComponentsSelector
  , maximumNumberOfResultsSelector
  , setMaximumNumberOfResultsSelector
  , earliestBookingDateForResultsSelector
  , setEarliestBookingDateForResultsSelector
  , latestBookingDateForResultsSelector
  , setLatestBookingDateForResultsSelector


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

-- | @- initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:@
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsINRestaurant restaurant, IsNSDateComponents preferredBookingDateComponents, IsNSNumber maximumNumberOfResults, IsNSDate earliestBookingDateForResults, IsNSDate latestBookingDateForResults) => inGetAvailableRestaurantReservationBookingsIntent -> restaurant -> CULong -> preferredBookingDateComponents -> maximumNumberOfResults -> earliestBookingDateForResults -> latestBookingDateForResults -> IO (Id INGetAvailableRestaurantReservationBookingsIntent)
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent  restaurant partySize preferredBookingDateComponents maximumNumberOfResults earliestBookingDateForResults latestBookingDateForResults =
withObjCPtr restaurant $ \raw_restaurant ->
  withObjCPtr preferredBookingDateComponents $ \raw_preferredBookingDateComponents ->
    withObjCPtr maximumNumberOfResults $ \raw_maximumNumberOfResults ->
      withObjCPtr earliestBookingDateForResults $ \raw_earliestBookingDateForResults ->
        withObjCPtr latestBookingDateForResults $ \raw_latestBookingDateForResults ->
            sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ()), argCULong (fromIntegral partySize), argPtr (castPtr raw_preferredBookingDateComponents :: Ptr ()), argPtr (castPtr raw_maximumNumberOfResults :: Ptr ()), argPtr (castPtr raw_earliestBookingDateForResults :: Ptr ()), argPtr (castPtr raw_latestBookingDateForResults :: Ptr ())] >>= ownedObject . castPtr

-- | @- restaurant@
restaurant :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id INRestaurant)
restaurant inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "restaurant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurant:@
setRestaurant :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsINRestaurant value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setRestaurant inGetAvailableRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setRestaurant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- partySize@
partySize :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO CULong
partySize inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "partySize") retCULong []

-- | @- setPartySize:@
setPartySize :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> CULong -> IO ()
setPartySize inGetAvailableRestaurantReservationBookingsIntent  value =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setPartySize:") retVoid [argCULong (fromIntegral value)]

-- | @- preferredBookingDateComponents@
preferredBookingDateComponents :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDateComponents)
preferredBookingDateComponents inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "preferredBookingDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredBookingDateComponents:@
setPreferredBookingDateComponents :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDateComponents value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setPreferredBookingDateComponents inGetAvailableRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setPreferredBookingDateComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumNumberOfResults@
maximumNumberOfResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSNumber)
maximumNumberOfResults inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "maximumNumberOfResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumNumberOfResults:@
setMaximumNumberOfResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSNumber value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setMaximumNumberOfResults inGetAvailableRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setMaximumNumberOfResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- earliestBookingDateForResults@
earliestBookingDateForResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDate)
earliestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "earliestBookingDateForResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEarliestBookingDateForResults:@
setEarliestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDate value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setEarliestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setEarliestBookingDateForResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latestBookingDateForResults@
latestBookingDateForResults :: IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent => inGetAvailableRestaurantReservationBookingsIntent -> IO (Id NSDate)
latestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "latestBookingDateForResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatestBookingDateForResults:@
setLatestBookingDateForResults :: (IsINGetAvailableRestaurantReservationBookingsIntent inGetAvailableRestaurantReservationBookingsIntent, IsNSDate value) => inGetAvailableRestaurantReservationBookingsIntent -> value -> IO ()
setLatestBookingDateForResults inGetAvailableRestaurantReservationBookingsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntent (mkSelector "setLatestBookingDateForResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:@
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector :: Selector
initWithRestaurant_partySize_preferredBookingDateComponents_maximumNumberOfResults_earliestBookingDateForResults_latestBookingDateForResultsSelector = mkSelector "initWithRestaurant:partySize:preferredBookingDateComponents:maximumNumberOfResults:earliestBookingDateForResults:latestBookingDateForResults:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @setPartySize:@
setPartySizeSelector :: Selector
setPartySizeSelector = mkSelector "setPartySize:"

-- | @Selector@ for @preferredBookingDateComponents@
preferredBookingDateComponentsSelector :: Selector
preferredBookingDateComponentsSelector = mkSelector "preferredBookingDateComponents"

-- | @Selector@ for @setPreferredBookingDateComponents:@
setPreferredBookingDateComponentsSelector :: Selector
setPreferredBookingDateComponentsSelector = mkSelector "setPreferredBookingDateComponents:"

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

-- | @Selector@ for @latestBookingDateForResults@
latestBookingDateForResultsSelector :: Selector
latestBookingDateForResultsSelector = mkSelector "latestBookingDateForResults"

-- | @Selector@ for @setLatestBookingDateForResults:@
setLatestBookingDateForResultsSelector :: Selector
setLatestBookingDateForResultsSelector = mkSelector "setLatestBookingDateForResults:"

