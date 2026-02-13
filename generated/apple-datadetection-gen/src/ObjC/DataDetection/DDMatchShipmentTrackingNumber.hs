{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains parcel tracking information that the data detection system matches.
--
-- The DataDetection framework returns a shipment tracking number match in a @DDMatchShipmentTrackingNumber@ object, which contains a carrier name and tracking identifier.
--
-- Generated bindings for @DDMatchShipmentTrackingNumber@.
module ObjC.DataDetection.DDMatchShipmentTrackingNumber
  ( DDMatchShipmentTrackingNumber
  , IsDDMatchShipmentTrackingNumber(..)
  , carrier
  , trackingNumber
  , carrierSelector
  , trackingNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of a parcel carrier.
--
-- ObjC selector: @- carrier@
carrier :: IsDDMatchShipmentTrackingNumber ddMatchShipmentTrackingNumber => ddMatchShipmentTrackingNumber -> IO (Id NSString)
carrier ddMatchShipmentTrackingNumber =
  sendMessage ddMatchShipmentTrackingNumber carrierSelector

-- | A string that represents a carrier’s tracking identifier for a parcel.
--
-- ObjC selector: @- trackingNumber@
trackingNumber :: IsDDMatchShipmentTrackingNumber ddMatchShipmentTrackingNumber => ddMatchShipmentTrackingNumber -> IO (Id NSString)
trackingNumber ddMatchShipmentTrackingNumber =
  sendMessage ddMatchShipmentTrackingNumber trackingNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @carrier@
carrierSelector :: Selector '[] (Id NSString)
carrierSelector = mkSelector "carrier"

-- | @Selector@ for @trackingNumber@
trackingNumberSelector :: Selector '[] (Id NSString)
trackingNumberSelector = mkSelector "trackingNumber"

