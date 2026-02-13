{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBusReservation@.
module ObjC.Intents.INBusReservation
  ( INBusReservation
  , IsINBusReservation(..)
  , init_
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTrip
  , reservedSeat
  , busTrip
  , busTripSelector
  , initSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector
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
init_ :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INBusReservation)
init_ inBusReservation =
  sendOwnedMessage inBusReservation initSelector

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTrip :: (IsINBusReservation inBusReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINBusTrip busTrip) => inBusReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> busTrip -> IO (Id INBusReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTrip inBusReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat busTrip =
  sendOwnedMessage inBusReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toINSeat reservedSeat) (toINBusTrip busTrip)

-- | @- reservedSeat@
reservedSeat :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INSeat)
reservedSeat inBusReservation =
  sendMessage inBusReservation reservedSeatSelector

-- | @- busTrip@
busTrip :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INBusTrip)
busTrip inBusReservation =
  sendMessage inBusReservation busTripSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBusReservation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id INSeat, Id INBusTrip] (Id INBusReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector '[] (Id INSeat)
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @busTrip@
busTripSelector :: Selector '[] (Id INBusTrip)
busTripSelector = mkSelector "busTrip"

