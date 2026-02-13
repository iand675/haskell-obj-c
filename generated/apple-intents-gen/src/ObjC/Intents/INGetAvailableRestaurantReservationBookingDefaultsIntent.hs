{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingDefaultsIntent@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingDefaultsIntent
  ( INGetAvailableRestaurantReservationBookingDefaultsIntent
  , IsINGetAvailableRestaurantReservationBookingDefaultsIntent(..)
  , initWithRestaurant
  , restaurant
  , setRestaurant
  , initWithRestaurantSelector
  , restaurantSelector
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

-- | @- initWithRestaurant:@
initWithRestaurant :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent, IsINRestaurant restaurant) => inGetAvailableRestaurantReservationBookingDefaultsIntent -> restaurant -> IO (Id INGetAvailableRestaurantReservationBookingDefaultsIntent)
initWithRestaurant inGetAvailableRestaurantReservationBookingDefaultsIntent restaurant =
  sendOwnedMessage inGetAvailableRestaurantReservationBookingDefaultsIntent initWithRestaurantSelector (toINRestaurant restaurant)

-- | @- restaurant@
restaurant :: IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent => inGetAvailableRestaurantReservationBookingDefaultsIntent -> IO (Id INRestaurant)
restaurant inGetAvailableRestaurantReservationBookingDefaultsIntent =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntent restaurantSelector

-- | @- setRestaurant:@
setRestaurant :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent, IsINRestaurant value) => inGetAvailableRestaurantReservationBookingDefaultsIntent -> value -> IO ()
setRestaurant inGetAvailableRestaurantReservationBookingDefaultsIntent value =
  sendMessage inGetAvailableRestaurantReservationBookingDefaultsIntent setRestaurantSelector (toINRestaurant value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:@
initWithRestaurantSelector :: Selector '[Id INRestaurant] (Id INGetAvailableRestaurantReservationBookingDefaultsIntent)
initWithRestaurantSelector = mkSelector "initWithRestaurant:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector '[] (Id INRestaurant)
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector '[Id INRestaurant] ()
setRestaurantSelector = mkSelector "setRestaurant:"

