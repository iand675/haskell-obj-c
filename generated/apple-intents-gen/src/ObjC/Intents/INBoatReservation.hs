{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBoatReservation@.
module ObjC.Intents.INBoatReservation
  ( INBoatReservation
  , IsINBoatReservation(..)
  , init_
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip
  , reservedSeat
  , boatTrip
  , boatTripSelector
  , initSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector
  , reservedSeatSelector

  -- * Enum types
  , INReservationStatus(INReservationStatus)
  , pattern INReservationStatusUnknown
  , pattern INReservationStatusCanceled
  , pattern INReservationStatusPending
  , pattern INReservationStatusHold
  , pattern INReservationStatusConfirmed

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

-- | @- init@
init_ :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INBoatReservation)
init_ inBoatReservation =
  sendOwnedMessage inBoatReservation initSelector

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip :: (IsINBoatReservation inBoatReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINBoatTrip boatTrip) => inBoatReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> boatTrip -> IO (Id INBoatReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip inBoatReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat boatTrip =
  sendOwnedMessage inBoatReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toINSeat reservedSeat) (toINBoatTrip boatTrip)

-- | @- reservedSeat@
reservedSeat :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INSeat)
reservedSeat inBoatReservation =
  sendMessage inBoatReservation reservedSeatSelector

-- | @- boatTrip@
boatTrip :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INBoatTrip)
boatTrip inBoatReservation =
  sendMessage inBoatReservation boatTripSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBoatReservation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id INSeat, Id INBoatTrip] (Id INBoatReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector '[] (Id INSeat)
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @boatTrip@
boatTripSelector :: Selector '[] (Id INBoatTrip)
boatTripSelector = mkSelector "boatTrip"

