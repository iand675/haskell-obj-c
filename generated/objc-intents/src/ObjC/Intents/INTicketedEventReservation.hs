{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTicketedEventReservation@.
module ObjC.Intents.INTicketedEventReservation
  ( INTicketedEventReservation
  , IsINTicketedEventReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_event
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_event
  , event
  , reservedSeat
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_eventSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_eventSelector
  , eventSelector
  , reservedSeatSelector

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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:event:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_event :: (IsINTicketedEventReservation inTicketedEventReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINTicketedEvent event) => inTicketedEventReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> event -> IO (Id INTicketedEventReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_event inTicketedEventReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat event =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservedSeat $ \raw_reservedSeat ->
              withObjCPtr event $ \raw_event ->
                  sendMsg inTicketedEventReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:event:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:event:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_event :: (IsINTicketedEventReservation inTicketedEventReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINSeat reservedSeat, IsINTicketedEvent event) => inTicketedEventReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservedSeat -> event -> IO (Id INTicketedEventReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_event inTicketedEventReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservedSeat event =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr reservedSeat $ \raw_reservedSeat ->
            withObjCPtr event $ \raw_event ->
                sendMsg inTicketedEventReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:event:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())] >>= ownedObject . castPtr

-- | @- event@
event :: IsINTicketedEventReservation inTicketedEventReservation => inTicketedEventReservation -> IO (Id INTicketedEvent)
event inTicketedEventReservation  =
  sendMsg inTicketedEventReservation (mkSelector "event") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reservedSeat@
reservedSeat :: IsINTicketedEventReservation inTicketedEventReservation => inTicketedEventReservation -> IO (Id INSeat)
reservedSeat inTicketedEventReservation  =
  sendMsg inTicketedEventReservation (mkSelector "reservedSeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:event:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_eventSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_eventSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:event:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:event:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_eventSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_eventSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:event:"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector
reservedSeatSelector = mkSelector "reservedSeat"

