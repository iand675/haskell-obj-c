{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INReservation@.
module ObjC.Intents.INReservation
  ( INReservation
  , IsINReservation(..)
  , init_
  , itemReference
  , reservationNumber
  , bookingTime
  , reservationStatus
  , reservationHolderName
  , actions
  , url
  , actionsSelector
  , bookingTimeSelector
  , initSelector
  , itemReferenceSelector
  , reservationHolderNameSelector
  , reservationNumberSelector
  , reservationStatusSelector
  , urlSelector

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
init_ :: IsINReservation inReservation => inReservation -> IO (Id INReservation)
init_ inReservation =
  sendOwnedMessage inReservation initSelector

-- | @- itemReference@
itemReference :: IsINReservation inReservation => inReservation -> IO (Id INSpeakableString)
itemReference inReservation =
  sendMessage inReservation itemReferenceSelector

-- | @- reservationNumber@
reservationNumber :: IsINReservation inReservation => inReservation -> IO (Id NSString)
reservationNumber inReservation =
  sendMessage inReservation reservationNumberSelector

-- | @- bookingTime@
bookingTime :: IsINReservation inReservation => inReservation -> IO (Id NSDate)
bookingTime inReservation =
  sendMessage inReservation bookingTimeSelector

-- | @- reservationStatus@
reservationStatus :: IsINReservation inReservation => inReservation -> IO INReservationStatus
reservationStatus inReservation =
  sendMessage inReservation reservationStatusSelector

-- | @- reservationHolderName@
reservationHolderName :: IsINReservation inReservation => inReservation -> IO (Id NSString)
reservationHolderName inReservation =
  sendMessage inReservation reservationHolderNameSelector

-- | @- actions@
actions :: IsINReservation inReservation => inReservation -> IO (Id NSArray)
actions inReservation =
  sendMessage inReservation actionsSelector

-- | @- URL@
url :: IsINReservation inReservation => inReservation -> IO (Id NSURL)
url inReservation =
  sendMessage inReservation urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INReservation)
initSelector = mkSelector "init"

-- | @Selector@ for @itemReference@
itemReferenceSelector :: Selector '[] (Id INSpeakableString)
itemReferenceSelector = mkSelector "itemReference"

-- | @Selector@ for @reservationNumber@
reservationNumberSelector :: Selector '[] (Id NSString)
reservationNumberSelector = mkSelector "reservationNumber"

-- | @Selector@ for @bookingTime@
bookingTimeSelector :: Selector '[] (Id NSDate)
bookingTimeSelector = mkSelector "bookingTime"

-- | @Selector@ for @reservationStatus@
reservationStatusSelector :: Selector '[] INReservationStatus
reservationStatusSelector = mkSelector "reservationStatus"

-- | @Selector@ for @reservationHolderName@
reservationHolderNameSelector :: Selector '[] (Id NSString)
reservationHolderNameSelector = mkSelector "reservationHolderName"

-- | @Selector@ for @actions@
actionsSelector :: Selector '[] (Id NSArray)
actionsSelector = mkSelector "actions"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

