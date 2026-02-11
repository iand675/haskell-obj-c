{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKAdImpression@.
module ObjC.StoreKit.SKAdImpression
  ( SKAdImpression
  , IsSKAdImpression(..)
  , sourceAppStoreItemIdentifier
  , setSourceAppStoreItemIdentifier
  , advertisedAppStoreItemIdentifier
  , setAdvertisedAppStoreItemIdentifier
  , adNetworkIdentifier
  , setAdNetworkIdentifier
  , adCampaignIdentifier
  , setAdCampaignIdentifier
  , adImpressionIdentifier
  , setAdImpressionIdentifier
  , adType
  , setAdType
  , adDescription
  , setAdDescription
  , adPurchaserName
  , setAdPurchaserName
  , timestamp
  , setTimestamp
  , signature
  , setSignature
  , version
  , setVersion
  , sourceAppStoreItemIdentifierSelector
  , setSourceAppStoreItemIdentifierSelector
  , advertisedAppStoreItemIdentifierSelector
  , setAdvertisedAppStoreItemIdentifierSelector
  , adNetworkIdentifierSelector
  , setAdNetworkIdentifierSelector
  , adCampaignIdentifierSelector
  , setAdCampaignIdentifierSelector
  , adImpressionIdentifierSelector
  , setAdImpressionIdentifierSelector
  , adTypeSelector
  , setAdTypeSelector
  , adDescriptionSelector
  , setAdDescriptionSelector
  , adPurchaserNameSelector
  , setAdPurchaserNameSelector
  , timestampSelector
  , setTimestampSelector
  , signatureSelector
  , setSignatureSelector
  , versionSelector
  , setVersionSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The App Store item identifier for the source app.
--
-- ObjC selector: @- sourceAppStoreItemIdentifier@
sourceAppStoreItemIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
sourceAppStoreItemIdentifier skAdImpression  =
  sendMsg skAdImpression (mkSelector "sourceAppStoreItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The App Store item identifier for the source app.
--
-- ObjC selector: @- setSourceAppStoreItemIdentifier:@
setSourceAppStoreItemIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setSourceAppStoreItemIdentifier skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setSourceAppStoreItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The App Store item identifier for the app being advertised.
--
-- ObjC selector: @- advertisedAppStoreItemIdentifier@
advertisedAppStoreItemIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
advertisedAppStoreItemIdentifier skAdImpression  =
  sendMsg skAdImpression (mkSelector "advertisedAppStoreItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The App Store item identifier for the app being advertised.
--
-- ObjC selector: @- setAdvertisedAppStoreItemIdentifier:@
setAdvertisedAppStoreItemIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setAdvertisedAppStoreItemIdentifier skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdvertisedAppStoreItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The identifier for the ad network.
--
-- ObjC selector: @- adNetworkIdentifier@
adNetworkIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adNetworkIdentifier skAdImpression  =
  sendMsg skAdImpression (mkSelector "adNetworkIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier for the ad network.
--
-- ObjC selector: @- setAdNetworkIdentifier:@
setAdNetworkIdentifier :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdNetworkIdentifier skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdNetworkIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The ad campaign identifier.
--
-- ObjC selector: @- adCampaignIdentifier@
adCampaignIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
adCampaignIdentifier skAdImpression  =
  sendMsg skAdImpression (mkSelector "adCampaignIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The ad campaign identifier.
--
-- ObjC selector: @- setAdCampaignIdentifier:@
setAdCampaignIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setAdCampaignIdentifier skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdCampaignIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The nonce used to generate the signature.
--
-- ObjC selector: @- adImpressionIdentifier@
adImpressionIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adImpressionIdentifier skAdImpression  =
  sendMsg skAdImpression (mkSelector "adImpressionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The nonce used to generate the signature.
--
-- ObjC selector: @- setAdImpressionIdentifier:@
setAdImpressionIdentifier :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdImpressionIdentifier skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdImpressionIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The type of ad being presented.
--
-- ObjC selector: @- adType@
adType :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adType skAdImpression  =
  sendMsg skAdImpression (mkSelector "adType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of ad being presented.
--
-- ObjC selector: @- setAdType:@
setAdType :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdType skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The description of the ad.
--
-- ObjC selector: @- adDescription@
adDescription :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adDescription skAdImpression  =
  sendMsg skAdImpression (mkSelector "adDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The description of the ad.
--
-- ObjC selector: @- setAdDescription:@
setAdDescription :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdDescription skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Name of entity that purchased the ad being presented.
--
-- ObjC selector: @- adPurchaserName@
adPurchaserName :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adPurchaserName skAdImpression  =
  sendMsg skAdImpression (mkSelector "adPurchaserName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of entity that purchased the ad being presented.
--
-- ObjC selector: @- setAdPurchaserName:@
setAdPurchaserName :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdPurchaserName skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setAdPurchaserName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The timestamp of the start and end call.
--
-- ObjC selector: @- timestamp@
timestamp :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
timestamp skAdImpression  =
  sendMsg skAdImpression (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The timestamp of the start and end call.
--
-- ObjC selector: @- setTimestamp:@
setTimestamp :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setTimestamp skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The signature of the impression.
--
-- ObjC selector: @- signature@
signature :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
signature skAdImpression  =
  sendMsg skAdImpression (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The signature of the impression.
--
-- ObjC selector: @- setSignature:@
setSignature :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setSignature skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The version of SKAdNetwork being used.
--
-- ObjC selector: @- version@
version :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
version skAdImpression  =
  sendMsg skAdImpression (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of SKAdNetwork being used.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setVersion skAdImpression  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAdImpression (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceAppStoreItemIdentifier@
sourceAppStoreItemIdentifierSelector :: Selector
sourceAppStoreItemIdentifierSelector = mkSelector "sourceAppStoreItemIdentifier"

-- | @Selector@ for @setSourceAppStoreItemIdentifier:@
setSourceAppStoreItemIdentifierSelector :: Selector
setSourceAppStoreItemIdentifierSelector = mkSelector "setSourceAppStoreItemIdentifier:"

-- | @Selector@ for @advertisedAppStoreItemIdentifier@
advertisedAppStoreItemIdentifierSelector :: Selector
advertisedAppStoreItemIdentifierSelector = mkSelector "advertisedAppStoreItemIdentifier"

-- | @Selector@ for @setAdvertisedAppStoreItemIdentifier:@
setAdvertisedAppStoreItemIdentifierSelector :: Selector
setAdvertisedAppStoreItemIdentifierSelector = mkSelector "setAdvertisedAppStoreItemIdentifier:"

-- | @Selector@ for @adNetworkIdentifier@
adNetworkIdentifierSelector :: Selector
adNetworkIdentifierSelector = mkSelector "adNetworkIdentifier"

-- | @Selector@ for @setAdNetworkIdentifier:@
setAdNetworkIdentifierSelector :: Selector
setAdNetworkIdentifierSelector = mkSelector "setAdNetworkIdentifier:"

-- | @Selector@ for @adCampaignIdentifier@
adCampaignIdentifierSelector :: Selector
adCampaignIdentifierSelector = mkSelector "adCampaignIdentifier"

-- | @Selector@ for @setAdCampaignIdentifier:@
setAdCampaignIdentifierSelector :: Selector
setAdCampaignIdentifierSelector = mkSelector "setAdCampaignIdentifier:"

-- | @Selector@ for @adImpressionIdentifier@
adImpressionIdentifierSelector :: Selector
adImpressionIdentifierSelector = mkSelector "adImpressionIdentifier"

-- | @Selector@ for @setAdImpressionIdentifier:@
setAdImpressionIdentifierSelector :: Selector
setAdImpressionIdentifierSelector = mkSelector "setAdImpressionIdentifier:"

-- | @Selector@ for @adType@
adTypeSelector :: Selector
adTypeSelector = mkSelector "adType"

-- | @Selector@ for @setAdType:@
setAdTypeSelector :: Selector
setAdTypeSelector = mkSelector "setAdType:"

-- | @Selector@ for @adDescription@
adDescriptionSelector :: Selector
adDescriptionSelector = mkSelector "adDescription"

-- | @Selector@ for @setAdDescription:@
setAdDescriptionSelector :: Selector
setAdDescriptionSelector = mkSelector "setAdDescription:"

-- | @Selector@ for @adPurchaserName@
adPurchaserNameSelector :: Selector
adPurchaserNameSelector = mkSelector "adPurchaserName"

-- | @Selector@ for @setAdPurchaserName:@
setAdPurchaserNameSelector :: Selector
setAdPurchaserNameSelector = mkSelector "setAdPurchaserName:"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @setTimestamp:@
setTimestampSelector :: Selector
setTimestampSelector = mkSelector "setTimestamp:"

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector
setSignatureSelector = mkSelector "setSignature:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

