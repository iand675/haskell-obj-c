{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantReservation@.
module ObjC.Intents.INRestaurantReservation
  ( INRestaurantReservation
  , IsINRestaurantReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation
  , reservationDuration
  , restaurantLocation
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector
  , reservationDurationSelector
  , restaurantLocationSelector

  -- * Enum types
  , INReservationStatus(INReservationStatus)
  , pattern INReservationStatusUnknown
  , pattern INReservationStatusCanceled
  , pattern INReservationStatusPending
  , pattern INReservationStatusHold
  , pattern INReservationStatusConfirmed

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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation :: (IsINRestaurantReservation inRestaurantReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINDateComponentsRange reservationDuration, IsNSNumber partySize, IsCLPlacemark restaurantLocation) => inRestaurantReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservationDuration -> partySize -> restaurantLocation -> IO (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation inRestaurantReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservationDuration partySize restaurantLocation =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservationDuration $ \raw_reservationDuration ->
              withObjCPtr partySize $ \raw_partySize ->
                withObjCPtr restaurantLocation $ \raw_restaurantLocation ->
                    sendMsg inRestaurantReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservationDuration :: Ptr ()), argPtr (castPtr raw_partySize :: Ptr ()), argPtr (castPtr raw_restaurantLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation :: (IsINRestaurantReservation inRestaurantReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINDateComponentsRange reservationDuration, IsNSNumber partySize, IsCLPlacemark restaurantLocation) => inRestaurantReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservationDuration -> partySize -> restaurantLocation -> IO (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation inRestaurantReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservationDuration partySize restaurantLocation =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr reservationDuration $ \raw_reservationDuration ->
            withObjCPtr partySize $ \raw_partySize ->
              withObjCPtr restaurantLocation $ \raw_restaurantLocation ->
                  sendMsg inRestaurantReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_reservationDuration :: Ptr ()), argPtr (castPtr raw_partySize :: Ptr ()), argPtr (castPtr raw_restaurantLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservationDuration@
reservationDuration :: IsINRestaurantReservation inRestaurantReservation => inRestaurantReservation -> IO (Id INDateComponentsRange)
reservationDuration inRestaurantReservation  =
  sendMsg inRestaurantReservation (mkSelector "reservationDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- restaurantLocation@
restaurantLocation :: IsINRestaurantReservation inRestaurantReservation => inRestaurantReservation -> IO (Id CLPlacemark)
restaurantLocation inRestaurantReservation  =
  sendMsg inRestaurantReservation (mkSelector "restaurantLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:"

-- | @Selector@ for @reservationDuration@
reservationDurationSelector :: Selector
reservationDurationSelector = mkSelector "reservationDuration"

-- | @Selector@ for @restaurantLocation@
restaurantLocationSelector :: Selector
restaurantLocationSelector = mkSelector "restaurantLocation"

