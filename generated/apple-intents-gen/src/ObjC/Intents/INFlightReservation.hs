{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFlightReservation@.
module ObjC.Intents.INFlightReservation
  ( INFlightReservation
  , IsINFlightReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight
  , reservedSeat
  , flight
  , flightSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector
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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight :: (IsINFlightReservation inFlightReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINFlight flight) => inFlightReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> flight -> IO (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight inFlightReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat flight =
  sendOwnedMessage inFlightReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toINSeat reservedSeat) (toINFlight flight)

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight :: (IsINFlightReservation inFlightReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINSeat reservedSeat, IsINFlight flight) => inFlightReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservedSeat -> flight -> IO (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight inFlightReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservedSeat flight =
  sendOwnedMessage inFlightReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toINSeat reservedSeat) (toINFlight flight)

-- | @- reservedSeat@
reservedSeat :: IsINFlightReservation inFlightReservation => inFlightReservation -> IO (Id INSeat)
reservedSeat inFlightReservation =
  sendMessage inFlightReservation reservedSeatSelector

-- | @- flight@
flight :: IsINFlightReservation inFlightReservation => inFlightReservation -> IO (Id INFlight)
flight inFlightReservation =
  sendMessage inFlightReservation flightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id INSeat, Id INFlight] (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id INSeat, Id INFlight] (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector '[] (Id INSeat)
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @flight@
flightSelector :: Selector '[] (Id INFlight)
flightSelector = mkSelector "flight"

