{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information read from the commissionee device during commissioning.
--
-- Generated bindings for @MTRCommissioneeInfo@.
module ObjC.Matter.MTRCommissioneeInfo
  ( MTRCommissioneeInfo
  , IsMTRCommissioneeInfo(..)
  , productIdentity
  , endpointsById
  , rootEndpoint
  , attributes
  , networkInterfaces
  , attributesSelector
  , endpointsByIdSelector
  , networkInterfacesSelector
  , productIdentitySelector
  , rootEndpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The product identity (VID / PID) of the commissionee.
--
-- ObjC selector: @- productIdentity@
productIdentity :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id MTRProductIdentity)
productIdentity mtrCommissioneeInfo =
  sendMessage mtrCommissioneeInfo productIdentitySelector

-- | Endpoint information for all endpoints of the commissionee. Will be present only if readEndpointInformation is set to YES on MTRCommissioningParameters.
--
-- Use @rootEndpoint@ and @-[MTREndpointInfo children]@ to traverse endpoints in composition order.
--
-- ObjC selector: @- endpointsById@
endpointsById :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSDictionary)
endpointsById mtrCommissioneeInfo =
  sendMessage mtrCommissioneeInfo endpointsByIdSelector

-- | Endpoint information for the root endpoint of the commissionee. Will be present only if readEndpointInformation is set to YES on MTRCommissioningParameters.
--
-- ObjC selector: @- rootEndpoint@
rootEndpoint :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id MTREndpointInfo)
rootEndpoint mtrCommissioneeInfo =
  sendMessage mtrCommissioneeInfo rootEndpointSelector

-- | Attributes that were read from the commissionee.  This will contain the following, if they are available:
--
-- 1) The attributes in extraAttributesToRead on MTRCommissioningParameters. 2) The FeatureMap attributes of all Network Commissioning clusters on the commissionee.
--
-- ObjC selector: @- attributes@
attributes :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSDictionary)
attributes mtrCommissioneeInfo =
  sendMessage mtrCommissioneeInfo attributesSelector

-- | Network interfaces the commissionee has.  The array will be empty if there are no network interfaces exposed on the commissionee.
--
-- ObjC selector: @- networkInterfaces@
networkInterfaces :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSArray)
networkInterfaces mtrCommissioneeInfo =
  sendMessage mtrCommissioneeInfo networkInterfacesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @productIdentity@
productIdentitySelector :: Selector '[] (Id MTRProductIdentity)
productIdentitySelector = mkSelector "productIdentity"

-- | @Selector@ for @endpointsById@
endpointsByIdSelector :: Selector '[] (Id NSDictionary)
endpointsByIdSelector = mkSelector "endpointsById"

-- | @Selector@ for @rootEndpoint@
rootEndpointSelector :: Selector '[] (Id MTREndpointInfo)
rootEndpointSelector = mkSelector "rootEndpoint"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @networkInterfaces@
networkInterfacesSelector :: Selector '[] (Id NSArray)
networkInterfacesSelector = mkSelector "networkInterfaces"

