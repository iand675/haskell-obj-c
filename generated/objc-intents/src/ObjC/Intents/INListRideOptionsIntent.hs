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
  , initWithPickupLocation_dropOffLocationSelector
  , pickupLocationSelector
  , dropOffLocationSelector


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

-- | @- initWithPickupLocation:dropOffLocation:@
initWithPickupLocation_dropOffLocation :: (IsINListRideOptionsIntent inListRideOptionsIntent, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inListRideOptionsIntent -> pickupLocation -> dropOffLocation -> IO (Id INListRideOptionsIntent)
initWithPickupLocation_dropOffLocation inListRideOptionsIntent  pickupLocation dropOffLocation =
withObjCPtr pickupLocation $ \raw_pickupLocation ->
  withObjCPtr dropOffLocation $ \raw_dropOffLocation ->
      sendMsg inListRideOptionsIntent (mkSelector "initWithPickupLocation:dropOffLocation:") (retPtr retVoid) [argPtr (castPtr raw_pickupLocation :: Ptr ()), argPtr (castPtr raw_dropOffLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- pickupLocation@
pickupLocation :: IsINListRideOptionsIntent inListRideOptionsIntent => inListRideOptionsIntent -> IO (Id CLPlacemark)
pickupLocation inListRideOptionsIntent  =
  sendMsg inListRideOptionsIntent (mkSelector "pickupLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dropOffLocation@
dropOffLocation :: IsINListRideOptionsIntent inListRideOptionsIntent => inListRideOptionsIntent -> IO (Id CLPlacemark)
dropOffLocation inListRideOptionsIntent  =
  sendMsg inListRideOptionsIntent (mkSelector "dropOffLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickupLocation:dropOffLocation:@
initWithPickupLocation_dropOffLocationSelector :: Selector
initWithPickupLocation_dropOffLocationSelector = mkSelector "initWithPickupLocation:dropOffLocation:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector
dropOffLocationSelector = mkSelector "dropOffLocation"

