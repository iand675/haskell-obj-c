{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestRideIntent@.
module ObjC.Intents.INRequestRideIntent
  ( INRequestRideIntent
  , IsINRequestRideIntent(..)
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTime
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod
  , pickupLocation
  , dropOffLocation
  , rideOptionName
  , partySize
  , paymentMethod
  , scheduledPickupTime
  , dropOffLocationSelector
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector
  , partySizeSelector
  , paymentMethodSelector
  , pickupLocationSelector
  , rideOptionNameSelector
  , scheduledPickupTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTime :: (IsINRequestRideIntent inRequestRideIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation, IsINSpeakableString rideOptionName, IsNSNumber partySize, IsINPaymentMethod paymentMethod, IsINDateComponentsRange scheduledPickupTime) => inRequestRideIntent -> pickupLocation -> dropOffLocation -> rideOptionName -> partySize -> paymentMethod -> scheduledPickupTime -> IO (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTime inRequestRideIntent pickupLocation dropOffLocation rideOptionName partySize paymentMethod scheduledPickupTime =
  sendOwnedMessage inRequestRideIntent initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector (toCLPlacemark pickupLocation) (toCLPlacemark dropOffLocation) (toINSpeakableString rideOptionName) (toNSNumber partySize) (toINPaymentMethod paymentMethod) (toINDateComponentsRange scheduledPickupTime)

-- | @- initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod :: (IsINRequestRideIntent inRequestRideIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation, IsINSpeakableString rideOptionName, IsNSNumber partySize, IsINPaymentMethod paymentMethod) => inRequestRideIntent -> pickupLocation -> dropOffLocation -> rideOptionName -> partySize -> paymentMethod -> IO (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod inRequestRideIntent pickupLocation dropOffLocation rideOptionName partySize paymentMethod =
  sendOwnedMessage inRequestRideIntent initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector (toCLPlacemark pickupLocation) (toCLPlacemark dropOffLocation) (toINSpeakableString rideOptionName) (toNSNumber partySize) (toINPaymentMethod paymentMethod)

-- | @- pickupLocation@
pickupLocation :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id CLPlacemark)
pickupLocation inRequestRideIntent =
  sendMessage inRequestRideIntent pickupLocationSelector

-- | @- dropOffLocation@
dropOffLocation :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id CLPlacemark)
dropOffLocation inRequestRideIntent =
  sendMessage inRequestRideIntent dropOffLocationSelector

-- | @- rideOptionName@
rideOptionName :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INSpeakableString)
rideOptionName inRequestRideIntent =
  sendMessage inRequestRideIntent rideOptionNameSelector

-- | @- partySize@
partySize :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id NSNumber)
partySize inRequestRideIntent =
  sendMessage inRequestRideIntent partySizeSelector

-- | @- paymentMethod@
paymentMethod :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INPaymentMethod)
paymentMethod inRequestRideIntent =
  sendMessage inRequestRideIntent paymentMethodSelector

-- | @- scheduledPickupTime@
scheduledPickupTime :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INDateComponentsRange)
scheduledPickupTime inRequestRideIntent =
  sendMessage inRequestRideIntent scheduledPickupTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector :: Selector '[Id CLPlacemark, Id CLPlacemark, Id INSpeakableString, Id NSNumber, Id INPaymentMethod, Id INDateComponentsRange] (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector = mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:"

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector :: Selector '[Id CLPlacemark, Id CLPlacemark, Id INSpeakableString, Id NSNumber, Id INPaymentMethod] (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector = mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector '[] (Id CLPlacemark)
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector '[] (Id CLPlacemark)
dropOffLocationSelector = mkSelector "dropOffLocation"

-- | @Selector@ for @rideOptionName@
rideOptionNameSelector :: Selector '[] (Id INSpeakableString)
rideOptionNameSelector = mkSelector "rideOptionName"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector '[] (Id NSNumber)
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @paymentMethod@
paymentMethodSelector :: Selector '[] (Id INPaymentMethod)
paymentMethodSelector = mkSelector "paymentMethod"

-- | @Selector@ for @scheduledPickupTime@
scheduledPickupTimeSelector :: Selector '[] (Id INDateComponentsRange)
scheduledPickupTimeSelector = mkSelector "scheduledPickupTime"

