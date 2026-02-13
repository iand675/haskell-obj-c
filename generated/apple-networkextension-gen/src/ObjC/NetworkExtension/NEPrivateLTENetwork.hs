{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEPrivateLTENetwork
--
-- The NEPrivateLTENetwork class declares an object that contains the parameters of a private LTE network.
--
-- Generated bindings for @NEPrivateLTENetwork@.
module ObjC.NetworkExtension.NEPrivateLTENetwork
  ( NEPrivateLTENetwork
  , IsNEPrivateLTENetwork(..)
  , mobileCountryCode
  , setMobileCountryCode
  , mobileNetworkCode
  , setMobileNetworkCode
  , trackingAreaCode
  , setTrackingAreaCode
  , mobileCountryCodeSelector
  , mobileNetworkCodeSelector
  , setMobileCountryCodeSelector
  , setMobileNetworkCodeSelector
  , setTrackingAreaCodeSelector
  , trackingAreaCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | mobileCountryCode
--
-- Mobile Country Code of the private LTE network.
--
-- ObjC selector: @- mobileCountryCode@
mobileCountryCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
mobileCountryCode nePrivateLTENetwork =
  sendMessage nePrivateLTENetwork mobileCountryCodeSelector

-- | mobileCountryCode
--
-- Mobile Country Code of the private LTE network.
--
-- ObjC selector: @- setMobileCountryCode:@
setMobileCountryCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setMobileCountryCode nePrivateLTENetwork value =
  sendMessage nePrivateLTENetwork setMobileCountryCodeSelector (toNSString value)

-- | mobileNetworkCode
--
-- Mobile Network Code of the private LTE network.
--
-- ObjC selector: @- mobileNetworkCode@
mobileNetworkCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
mobileNetworkCode nePrivateLTENetwork =
  sendMessage nePrivateLTENetwork mobileNetworkCodeSelector

-- | mobileNetworkCode
--
-- Mobile Network Code of the private LTE network.
--
-- ObjC selector: @- setMobileNetworkCode:@
setMobileNetworkCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setMobileNetworkCode nePrivateLTENetwork value =
  sendMessage nePrivateLTENetwork setMobileNetworkCodeSelector (toNSString value)

-- | trackingAreaCode
--
-- Tracking Area Code of the private LTE network. This property is only applicable for band 48 private LTE networks.
--
-- ObjC selector: @- trackingAreaCode@
trackingAreaCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
trackingAreaCode nePrivateLTENetwork =
  sendMessage nePrivateLTENetwork trackingAreaCodeSelector

-- | trackingAreaCode
--
-- Tracking Area Code of the private LTE network. This property is only applicable for band 48 private LTE networks.
--
-- ObjC selector: @- setTrackingAreaCode:@
setTrackingAreaCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setTrackingAreaCode nePrivateLTENetwork value =
  sendMessage nePrivateLTENetwork setTrackingAreaCodeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mobileCountryCode@
mobileCountryCodeSelector :: Selector '[] (Id NSString)
mobileCountryCodeSelector = mkSelector "mobileCountryCode"

-- | @Selector@ for @setMobileCountryCode:@
setMobileCountryCodeSelector :: Selector '[Id NSString] ()
setMobileCountryCodeSelector = mkSelector "setMobileCountryCode:"

-- | @Selector@ for @mobileNetworkCode@
mobileNetworkCodeSelector :: Selector '[] (Id NSString)
mobileNetworkCodeSelector = mkSelector "mobileNetworkCode"

-- | @Selector@ for @setMobileNetworkCode:@
setMobileNetworkCodeSelector :: Selector '[Id NSString] ()
setMobileNetworkCodeSelector = mkSelector "setMobileNetworkCode:"

-- | @Selector@ for @trackingAreaCode@
trackingAreaCodeSelector :: Selector '[] (Id NSString)
trackingAreaCodeSelector = mkSelector "trackingAreaCode"

-- | @Selector@ for @setTrackingAreaCode:@
setTrackingAreaCodeSelector :: Selector '[Id NSString] ()
setTrackingAreaCodeSelector = mkSelector "setTrackingAreaCode:"

