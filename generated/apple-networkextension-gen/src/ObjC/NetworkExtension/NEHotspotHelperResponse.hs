{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotHelperResponse
--
-- The HotspotHelper creates an NEHotspotHelperResponse object to provide   the results of running the corresponding NEHotspotHelperCommand.
--
-- Generated bindings for @NEHotspotHelperResponse@.
module ObjC.NetworkExtension.NEHotspotHelperResponse
  ( NEHotspotHelperResponse
  , IsNEHotspotHelperResponse(..)
  , setNetwork
  , setNetworkList
  , deliver
  , deliverSelector
  , setNetworkListSelector
  , setNetworkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setNetwork
--
-- Set the network that conveys the confidence level.
--
-- Provide the annotated NEHotspotNetwork object in the response to the   kNEHotspotHelperCommandTypeEvaluate command. The helper sets the   confidence in the network object to indicate its ability to handle   the current network.
--
-- ObjC selector: @- setNetwork:@
setNetwork :: (IsNEHotspotHelperResponse neHotspotHelperResponse, IsNEHotspotNetwork network) => neHotspotHelperResponse -> network -> IO ()
setNetwork neHotspotHelperResponse network =
  sendMessage neHotspotHelperResponse setNetworkSelector (toNEHotspotNetwork network)

-- | setNetworkList
--
-- Set the list of handled networks.
--
-- Provide an NSArray of annotated NEHotspotNetwork objects in response   to the kNEHotspotHelperCommandTypeFilterScanList command.   The helper provides the list of network objects that it is capable of   handling with at least low confidence. Networks that it has no   confidence in handling should not be specified.
--
-- ObjC selector: @- setNetworkList:@
setNetworkList :: (IsNEHotspotHelperResponse neHotspotHelperResponse, IsNSArray networkList) => neHotspotHelperResponse -> networkList -> IO ()
setNetworkList neHotspotHelperResponse networkList =
  sendMessage neHotspotHelperResponse setNetworkListSelector (toNSArray networkList)

-- | deliver
--
-- Delivers the response to the command.
--
-- Deliver the NEHotspotHelperResponse to the HotspotHelper infrastructure.
--
-- ObjC selector: @- deliver@
deliver :: IsNEHotspotHelperResponse neHotspotHelperResponse => neHotspotHelperResponse -> IO ()
deliver neHotspotHelperResponse =
  sendMessage neHotspotHelperResponse deliverSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNetwork:@
setNetworkSelector :: Selector '[Id NEHotspotNetwork] ()
setNetworkSelector = mkSelector "setNetwork:"

-- | @Selector@ for @setNetworkList:@
setNetworkListSelector :: Selector '[Id NSArray] ()
setNetworkListSelector = mkSelector "setNetworkList:"

-- | @Selector@ for @deliver@
deliverSelector :: Selector '[] ()
deliverSelector = mkSelector "deliver"

