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

-- | @- initWithRestaurant:@
initWithRestaurant :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent, IsINRestaurant restaurant) => inGetAvailableRestaurantReservationBookingDefaultsIntent -> restaurant -> IO (Id INGetAvailableRestaurantReservationBookingDefaultsIntent)
initWithRestaurant inGetAvailableRestaurantReservationBookingDefaultsIntent  restaurant =
withObjCPtr restaurant $ \raw_restaurant ->
    sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntent (mkSelector "initWithRestaurant:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ())] >>= ownedObject . castPtr

-- | @- restaurant@
restaurant :: IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent => inGetAvailableRestaurantReservationBookingDefaultsIntent -> IO (Id INRestaurant)
restaurant inGetAvailableRestaurantReservationBookingDefaultsIntent  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntent (mkSelector "restaurant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurant:@
setRestaurant :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntent inGetAvailableRestaurantReservationBookingDefaultsIntent, IsINRestaurant value) => inGetAvailableRestaurantReservationBookingDefaultsIntent -> value -> IO ()
setRestaurant inGetAvailableRestaurantReservationBookingDefaultsIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntent (mkSelector "setRestaurant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:@
initWithRestaurantSelector :: Selector
initWithRestaurantSelector = mkSelector "initWithRestaurant:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector
setRestaurantSelector = mkSelector "setRestaurant:"

