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
  , setMobileCountryCodeSelector
  , mobileNetworkCodeSelector
  , setMobileNetworkCodeSelector
  , trackingAreaCodeSelector
  , setTrackingAreaCodeSelector


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

-- | mobileCountryCode
--
-- Mobile Country Code of the private LTE network.
--
-- ObjC selector: @- mobileCountryCode@
mobileCountryCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
mobileCountryCode nePrivateLTENetwork  =
    sendMsg nePrivateLTENetwork (mkSelector "mobileCountryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mobileCountryCode
--
-- Mobile Country Code of the private LTE network.
--
-- ObjC selector: @- setMobileCountryCode:@
setMobileCountryCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setMobileCountryCode nePrivateLTENetwork  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePrivateLTENetwork (mkSelector "setMobileCountryCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | mobileNetworkCode
--
-- Mobile Network Code of the private LTE network.
--
-- ObjC selector: @- mobileNetworkCode@
mobileNetworkCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
mobileNetworkCode nePrivateLTENetwork  =
    sendMsg nePrivateLTENetwork (mkSelector "mobileNetworkCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mobileNetworkCode
--
-- Mobile Network Code of the private LTE network.
--
-- ObjC selector: @- setMobileNetworkCode:@
setMobileNetworkCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setMobileNetworkCode nePrivateLTENetwork  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePrivateLTENetwork (mkSelector "setMobileNetworkCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | trackingAreaCode
--
-- Tracking Area Code of the private LTE network. This property is only applicable for band 48 private LTE networks.
--
-- ObjC selector: @- trackingAreaCode@
trackingAreaCode :: IsNEPrivateLTENetwork nePrivateLTENetwork => nePrivateLTENetwork -> IO (Id NSString)
trackingAreaCode nePrivateLTENetwork  =
    sendMsg nePrivateLTENetwork (mkSelector "trackingAreaCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | trackingAreaCode
--
-- Tracking Area Code of the private LTE network. This property is only applicable for band 48 private LTE networks.
--
-- ObjC selector: @- setTrackingAreaCode:@
setTrackingAreaCode :: (IsNEPrivateLTENetwork nePrivateLTENetwork, IsNSString value) => nePrivateLTENetwork -> value -> IO ()
setTrackingAreaCode nePrivateLTENetwork  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePrivateLTENetwork (mkSelector "setTrackingAreaCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mobileCountryCode@
mobileCountryCodeSelector :: Selector
mobileCountryCodeSelector = mkSelector "mobileCountryCode"

-- | @Selector@ for @setMobileCountryCode:@
setMobileCountryCodeSelector :: Selector
setMobileCountryCodeSelector = mkSelector "setMobileCountryCode:"

-- | @Selector@ for @mobileNetworkCode@
mobileNetworkCodeSelector :: Selector
mobileNetworkCodeSelector = mkSelector "mobileNetworkCode"

-- | @Selector@ for @setMobileNetworkCode:@
setMobileNetworkCodeSelector :: Selector
setMobileNetworkCodeSelector = mkSelector "setMobileNetworkCode:"

-- | @Selector@ for @trackingAreaCode@
trackingAreaCodeSelector :: Selector
trackingAreaCodeSelector = mkSelector "trackingAreaCode"

-- | @Selector@ for @setTrackingAreaCode:@
setTrackingAreaCodeSelector :: Selector
setTrackingAreaCodeSelector = mkSelector "setTrackingAreaCode:"

