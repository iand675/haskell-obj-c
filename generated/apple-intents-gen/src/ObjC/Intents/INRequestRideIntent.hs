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
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector
  , initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector
  , pickupLocationSelector
  , dropOffLocationSelector
  , rideOptionNameSelector
  , partySizeSelector
  , paymentMethodSelector
  , scheduledPickupTimeSelector


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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTime :: (IsINRequestRideIntent inRequestRideIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation, IsINSpeakableString rideOptionName, IsNSNumber partySize, IsINPaymentMethod paymentMethod, IsINDateComponentsRange scheduledPickupTime) => inRequestRideIntent -> pickupLocation -> dropOffLocation -> rideOptionName -> partySize -> paymentMethod -> scheduledPickupTime -> IO (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTime inRequestRideIntent  pickupLocation dropOffLocation rideOptionName partySize paymentMethod scheduledPickupTime =
  withObjCPtr pickupLocation $ \raw_pickupLocation ->
    withObjCPtr dropOffLocation $ \raw_dropOffLocation ->
      withObjCPtr rideOptionName $ \raw_rideOptionName ->
        withObjCPtr partySize $ \raw_partySize ->
          withObjCPtr paymentMethod $ \raw_paymentMethod ->
            withObjCPtr scheduledPickupTime $ \raw_scheduledPickupTime ->
                sendMsg inRequestRideIntent (mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:") (retPtr retVoid) [argPtr (castPtr raw_pickupLocation :: Ptr ()), argPtr (castPtr raw_dropOffLocation :: Ptr ()), argPtr (castPtr raw_rideOptionName :: Ptr ()), argPtr (castPtr raw_partySize :: Ptr ()), argPtr (castPtr raw_paymentMethod :: Ptr ()), argPtr (castPtr raw_scheduledPickupTime :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod :: (IsINRequestRideIntent inRequestRideIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation, IsINSpeakableString rideOptionName, IsNSNumber partySize, IsINPaymentMethod paymentMethod) => inRequestRideIntent -> pickupLocation -> dropOffLocation -> rideOptionName -> partySize -> paymentMethod -> IO (Id INRequestRideIntent)
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod inRequestRideIntent  pickupLocation dropOffLocation rideOptionName partySize paymentMethod =
  withObjCPtr pickupLocation $ \raw_pickupLocation ->
    withObjCPtr dropOffLocation $ \raw_dropOffLocation ->
      withObjCPtr rideOptionName $ \raw_rideOptionName ->
        withObjCPtr partySize $ \raw_partySize ->
          withObjCPtr paymentMethod $ \raw_paymentMethod ->
              sendMsg inRequestRideIntent (mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:") (retPtr retVoid) [argPtr (castPtr raw_pickupLocation :: Ptr ()), argPtr (castPtr raw_dropOffLocation :: Ptr ()), argPtr (castPtr raw_rideOptionName :: Ptr ()), argPtr (castPtr raw_partySize :: Ptr ()), argPtr (castPtr raw_paymentMethod :: Ptr ())] >>= ownedObject . castPtr

-- | @- pickupLocation@
pickupLocation :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id CLPlacemark)
pickupLocation inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "pickupLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dropOffLocation@
dropOffLocation :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id CLPlacemark)
dropOffLocation inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "dropOffLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rideOptionName@
rideOptionName :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INSpeakableString)
rideOptionName inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "rideOptionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- partySize@
partySize :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id NSNumber)
partySize inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "partySize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paymentMethod@
paymentMethod :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INPaymentMethod)
paymentMethod inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "paymentMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scheduledPickupTime@
scheduledPickupTime :: IsINRequestRideIntent inRequestRideIntent => inRequestRideIntent -> IO (Id INDateComponentsRange)
scheduledPickupTime inRequestRideIntent  =
    sendMsg inRequestRideIntent (mkSelector "scheduledPickupTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector :: Selector
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethod_scheduledPickupTimeSelector = mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:scheduledPickupTime:"

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:@
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector :: Selector
initWithPickupLocation_dropOffLocation_rideOptionName_partySize_paymentMethodSelector = mkSelector "initWithPickupLocation:dropOffLocation:rideOptionName:partySize:paymentMethod:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector
dropOffLocationSelector = mkSelector "dropOffLocation"

-- | @Selector@ for @rideOptionName@
rideOptionNameSelector :: Selector
rideOptionNameSelector = mkSelector "rideOptionName"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @paymentMethod@
paymentMethodSelector :: Selector
paymentMethodSelector = mkSelector "paymentMethod"

-- | @Selector@ for @scheduledPickupTime@
scheduledPickupTimeSelector :: Selector
scheduledPickupTimeSelector = mkSelector "scheduledPickupTime"

