{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INListRideOptionsIntent@.
module ObjC.Intents.INListRideOptionsIntent
  ( INListRideOptionsIntent
  , IsINListRideOptionsIntent(..)
  , initWithPickupLocation_dropOffLocation
  , pickupLocation
  , dropOffLocation
  , dropOffLocationSelector
  , initWithPickupLocation_dropOffLocationSelector
  , pickupLocationSelector


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

-- | @- initWithPickupLocation:dropOffLocation:@
initWithPickupLocation_dropOffLocation :: (IsINListRideOptionsIntent inListRideOptionsIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inListRideOptionsIntent -> pickupLocation -> dropOffLocation -> IO (Id INListRideOptionsIntent)
initWithPickupLocation_dropOffLocation inListRideOptionsIntent pickupLocation dropOffLocation =
  sendOwnedMessage inListRideOptionsIntent initWithPickupLocation_dropOffLocationSelector (toCLPlacemark pickupLocation) (toCLPlacemark dropOffLocation)

-- | @- pickupLocation@
pickupLocation :: IsINListRideOptionsIntent inListRideOptionsIntent => inListRideOptionsIntent -> IO (Id CLPlacemark)
pickupLocation inListRideOptionsIntent =
  sendMessage inListRideOptionsIntent pickupLocationSelector

-- | @- dropOffLocation@
dropOffLocation :: IsINListRideOptionsIntent inListRideOptionsIntent => inListRideOptionsIntent -> IO (Id CLPlacemark)
dropOffLocation inListRideOptionsIntent =
  sendMessage inListRideOptionsIntent dropOffLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:@
initWithPickupLocation_dropOffLocationSelector :: Selector '[Id CLPlacemark, Id CLPlacemark] (Id INListRideOptionsIntent)
initWithPickupLocation_dropOffLocationSelector = mkSelector "initWithPickupLocation:dropOffLocation:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector '[] (Id CLPlacemark)
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector '[] (Id CLPlacemark)
dropOffLocationSelector = mkSelector "dropOffLocation"

