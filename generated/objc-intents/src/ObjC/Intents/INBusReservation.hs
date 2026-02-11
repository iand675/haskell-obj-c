{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector
  , reservedSeatSelector
  , busTripSelector

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INBusReservation)
init_ inBusReservation  =
  sendMsg inBusReservation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTrip :: (IsINBusReservation inBusReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINBusTrip busTrip) => inBusReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> busTrip -> IO (Id INBusReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTrip inBusReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat busTrip =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservedSeat $ \raw_reservedSeat ->
              withObjCPtr busTrip $ \raw_busTrip ->
                  sendMsg inBusReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_busTrip :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservedSeat@
reservedSeat :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INSeat)
reservedSeat inBusReservation  =
  sendMsg inBusReservation (mkSelector "reservedSeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- busTrip@
busTrip :: IsINBusReservation inBusReservation => inBusReservation -> IO (Id INBusTrip)
busTrip inBusReservation  =
  sendMsg inBusReservation (mkSelector "busTrip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_busTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:busTrip:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @busTrip@
busTripSelector :: Selector
busTripSelector = mkSelector "busTrip"

