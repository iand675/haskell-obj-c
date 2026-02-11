{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLodgingReservation@.
module ObjC.Intents.INLodgingReservation
  ( INLodgingReservation
  , IsINLodgingReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren
  , lodgingBusinessLocation
  , reservationDuration
  , numberOfAdults
  , numberOfChildren
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector
  , lodgingBusinessLocationSelector
  , reservationDurationSelector
  , numberOfAdultsSelector
  , numberOfChildrenSelector

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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren :: (IsINLodgingReservation inLodgingReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsCLPlacemark lodgingBusinessLocation, IsINDateComponentsRange reservationDuration, IsNSNumber numberOfAdults, IsNSNumber numberOfChildren) => inLodgingReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> lodgingBusinessLocation -> reservationDuration -> numberOfAdults -> numberOfChildren -> IO (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren inLodgingReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url lodgingBusinessLocation reservationDuration numberOfAdults numberOfChildren =
  withObjCPtr itemReference $ \raw_itemReference ->
    withObjCPtr reservationNumber $ \raw_reservationNumber ->
      withObjCPtr bookingTime $ \raw_bookingTime ->
        withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
          withObjCPtr actions $ \raw_actions ->
            withObjCPtr url $ \raw_url ->
              withObjCPtr lodgingBusinessLocation $ \raw_lodgingBusinessLocation ->
                withObjCPtr reservationDuration $ \raw_reservationDuration ->
                  withObjCPtr numberOfAdults $ \raw_numberOfAdults ->
                    withObjCPtr numberOfChildren $ \raw_numberOfChildren ->
                        sendMsg inLodgingReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_lodgingBusinessLocation :: Ptr ()), argPtr (castPtr raw_reservationDuration :: Ptr ()), argPtr (castPtr raw_numberOfAdults :: Ptr ()), argPtr (castPtr raw_numberOfChildren :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren :: (IsINLodgingReservation inLodgingReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsCLPlacemark lodgingBusinessLocation, IsINDateComponentsRange reservationDuration, IsNSNumber numberOfAdults, IsNSNumber numberOfChildren) => inLodgingReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> lodgingBusinessLocation -> reservationDuration -> numberOfAdults -> numberOfChildren -> IO (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren inLodgingReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions lodgingBusinessLocation reservationDuration numberOfAdults numberOfChildren =
  withObjCPtr itemReference $ \raw_itemReference ->
    withObjCPtr reservationNumber $ \raw_reservationNumber ->
      withObjCPtr bookingTime $ \raw_bookingTime ->
        withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
          withObjCPtr actions $ \raw_actions ->
            withObjCPtr lodgingBusinessLocation $ \raw_lodgingBusinessLocation ->
              withObjCPtr reservationDuration $ \raw_reservationDuration ->
                withObjCPtr numberOfAdults $ \raw_numberOfAdults ->
                  withObjCPtr numberOfChildren $ \raw_numberOfChildren ->
                      sendMsg inLodgingReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_lodgingBusinessLocation :: Ptr ()), argPtr (castPtr raw_reservationDuration :: Ptr ()), argPtr (castPtr raw_numberOfAdults :: Ptr ()), argPtr (castPtr raw_numberOfChildren :: Ptr ())] >>= ownedObject . castPtr

-- | @- lodgingBusinessLocation@
lodgingBusinessLocation :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id CLPlacemark)
lodgingBusinessLocation inLodgingReservation  =
    sendMsg inLodgingReservation (mkSelector "lodgingBusinessLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reservationDuration@
reservationDuration :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id INDateComponentsRange)
reservationDuration inLodgingReservation  =
    sendMsg inLodgingReservation (mkSelector "reservationDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfAdults@
numberOfAdults :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id NSNumber)
numberOfAdults inLodgingReservation  =
    sendMsg inLodgingReservation (mkSelector "numberOfAdults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfChildren@
numberOfChildren :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id NSNumber)
numberOfChildren inLodgingReservation  =
    sendMsg inLodgingReservation (mkSelector "numberOfChildren") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:"

-- | @Selector@ for @lodgingBusinessLocation@
lodgingBusinessLocationSelector :: Selector
lodgingBusinessLocationSelector = mkSelector "lodgingBusinessLocation"

-- | @Selector@ for @reservationDuration@
reservationDurationSelector :: Selector
reservationDurationSelector = mkSelector "reservationDuration"

-- | @Selector@ for @numberOfAdults@
numberOfAdultsSelector :: Selector
numberOfAdultsSelector = mkSelector "numberOfAdults"

-- | @Selector@ for @numberOfChildren@
numberOfChildrenSelector :: Selector
numberOfChildrenSelector = mkSelector "numberOfChildren"

