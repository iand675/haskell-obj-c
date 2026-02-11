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

-- | @- initWithReservationContainerReference:reservationItemReferences:@
initWithReservationContainerReference_reservationItemReferences :: (IsINGetReservationDetailsIntent inGetReservationDetailsIntent, IsINSpeakableString reservationContainerReference, IsNSArray reservationItemReferences) => inGetReservationDetailsIntent -> reservationContainerReference -> reservationItemReferences -> IO (Id INGetReservationDetailsIntent)
initWithReservationContainerReference_reservationItemReferences inGetReservationDetailsIntent  reservationContainerReference reservationItemReferences =
withObjCPtr reservationContainerReference $ \raw_reservationContainerReference ->
  withObjCPtr reservationItemReferences $ \raw_reservationItemReferences ->
      sendMsg inGetReservationDetailsIntent (mkSelector "initWithReservationContainerReference:reservationItemReferences:") (retPtr retVoid) [argPtr (castPtr raw_reservationContainerReference :: Ptr ()), argPtr (castPtr raw_reservationItemReferences :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservationContainerReference@
reservationContainerReference :: IsINGetReservationDetailsIntent inGetReservationDetailsIntent => inGetReservationDetailsIntent -> IO (Id INSpeakableString)
reservationContainerReference inGetReservationDetailsIntent  =
  sendMsg inGetReservationDetailsIntent (mkSelector "reservationContainerReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reservationItemReferences@
reservationItemReferences :: IsINGetReservationDetailsIntent inGetReservationDetailsIntent => inGetReservationDetailsIntent -> IO (Id NSArray)
reservationItemReferences inGetReservationDetailsIntent  =
  sendMsg inGetReservationDetailsIntent (mkSelector "reservationItemReferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReservationContainerReference:reservationItemReferences:@
initWithReservationContainerReference_reservationItemReferencesSelector :: Selector
initWithReservationContainerReference_reservationItemReferencesSelector = mkSelector "initWithReservationContainerReference:reservationItemReferences:"

-- | @Selector@ for @reservationContainerReference@
reservationContainerReferenceSelector :: Selector
reservationContainerReferenceSelector = mkSelector "reservationContainerReference"

-- | @Selector@ for @reservationItemReferences@
reservationItemReferencesSelector :: Selector
reservationItemReferencesSelector = mkSelector "reservationItemReferences"

