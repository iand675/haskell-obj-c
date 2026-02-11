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
  , setNetworkSelector
  , setNetworkListSelector
  , deliverSelector


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
setNetwork neHotspotHelperResponse  network =
withObjCPtr network $ \raw_network ->
    sendMsg neHotspotHelperResponse (mkSelector "setNetwork:") retVoid [argPtr (castPtr raw_network :: Ptr ())]

-- | setNetworkList
--
-- Set the list of handled networks.
--
-- Provide an NSArray of annotated NEHotspotNetwork objects in response   to the kNEHotspotHelperCommandTypeFilterScanList command.   The helper provides the list of network objects that it is capable of   handling with at least low confidence. Networks that it has no   confidence in handling should not be specified.
--
-- ObjC selector: @- setNetworkList:@
setNetworkList :: (IsNEHotspotHelperResponse neHotspotHelperResponse, IsNSArray networkList) => neHotspotHelperResponse -> networkList -> IO ()
setNetworkList neHotspotHelperResponse  networkList =
withObjCPtr networkList $ \raw_networkList ->
    sendMsg neHotspotHelperResponse (mkSelector "setNetworkList:") retVoid [argPtr (castPtr raw_networkList :: Ptr ())]

-- | deliver
--
-- Delivers the response to the command.
--
-- Deliver the NEHotspotHelperResponse to the HotspotHelper infrastructure.
--
-- ObjC selector: @- deliver@
deliver :: IsNEHotspotHelperResponse neHotspotHelperResponse => neHotspotHelperResponse -> IO ()
deliver neHotspotHelperResponse  =
  sendMsg neHotspotHelperResponse (mkSelector "deliver") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNetwork:@
setNetworkSelector :: Selector
setNetworkSelector = mkSelector "setNetwork:"

-- | @Selector@ for @setNetworkList:@
setNetworkListSelector :: Selector
setNetworkListSelector = mkSelector "setNetworkList:"

-- | @Selector@ for @deliver@
deliverSelector :: Selector
deliverSelector = mkSelector "deliver"

