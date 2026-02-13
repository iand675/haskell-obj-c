{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetReservationDetailsIntent@.
module ObjC.Intents.INGetReservationDetailsIntent
  ( INGetReservationDetailsIntent
  , IsINGetReservationDetailsIntent(..)
  , initWithReservationContainerReference_reservationItemReferences
  , reservationContainerReference
  , reservationItemReferences
  , initWithReservationContainerReference_reservationItemReferencesSelector
  , reservationContainerReferenceSelector
  , reservationItemReferencesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithReservationContainerReference:reservationItemReferences:@
initWithReservationContainerReference_reservationItemReferences :: (IsINGetReservationDetailsIntent inGetReservationDetailsIntent, IsINSpeakableString reservationContainerReference, IsNSArray reservationItemReferences) => inGetReservationDetailsIntent -> reservationContainerReference -> reservationItemReferences -> IO (Id INGetReservationDetailsIntent)
initWithReservationContainerReference_reservationItemReferences inGetReservationDetailsIntent reservationContainerReference reservationItemReferences =
  sendOwnedMessage inGetReservationDetailsIntent initWithReservationContainerReference_reservationItemReferencesSelector (toINSpeakableString reservationContainerReference) (toNSArray reservationItemReferences)

-- | @- reservationContainerReference@
reservationContainerReference :: IsINGetReservationDetailsIntent inGetReservationDetailsIntent => inGetReservationDetailsIntent -> IO (Id INSpeakableString)
reservationContainerReference inGetReservationDetailsIntent =
  sendMessage inGetReservationDetailsIntent reservationContainerReferenceSelector

-- | @- reservationItemReferences@
reservationItemReferences :: IsINGetReservationDetailsIntent inGetReservationDetailsIntent => inGetReservationDetailsIntent -> IO (Id NSArray)
reservationItemReferences inGetReservationDetailsIntent =
  sendMessage inGetReservationDetailsIntent reservationItemReferencesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReservationContainerReference:reservationItemReferences:@
initWithReservationContainerReference_reservationItemReferencesSelector :: Selector '[Id INSpeakableString, Id NSArray] (Id INGetReservationDetailsIntent)
initWithReservationContainerReference_reservationItemReferencesSelector = mkSelector "initWithReservationContainerReference:reservationItemReferences:"

-- | @Selector@ for @reservationContainerReference@
reservationContainerReferenceSelector :: Selector '[] (Id INSpeakableString)
reservationContainerReferenceSelector = mkSelector "reservationContainerReference"

-- | @Selector@ for @reservationItemReferences@
reservationItemReferencesSelector :: Selector '[] (Id NSArray)
reservationItemReferencesSelector = mkSelector "reservationItemReferences"

