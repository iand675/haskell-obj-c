{-# LANGUAGE DataKinds #-}
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
  , sourceIdentifier
  , setSourceIdentifier
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
  , adCampaignIdentifierSelector
  , adDescriptionSelector
  , adImpressionIdentifierSelector
  , adNetworkIdentifierSelector
  , adPurchaserNameSelector
  , adTypeSelector
  , advertisedAppStoreItemIdentifierSelector
  , setAdCampaignIdentifierSelector
  , setAdDescriptionSelector
  , setAdImpressionIdentifierSelector
  , setAdNetworkIdentifierSelector
  , setAdPurchaserNameSelector
  , setAdTypeSelector
  , setAdvertisedAppStoreItemIdentifierSelector
  , setSignatureSelector
  , setSourceAppStoreItemIdentifierSelector
  , setSourceIdentifierSelector
  , setTimestampSelector
  , setVersionSelector
  , signatureSelector
  , sourceAppStoreItemIdentifierSelector
  , sourceIdentifierSelector
  , timestampSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The App Store item identifier for the source app.
--
-- ObjC selector: @- sourceAppStoreItemIdentifier@
sourceAppStoreItemIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
sourceAppStoreItemIdentifier skAdImpression =
  sendMessage skAdImpression sourceAppStoreItemIdentifierSelector

-- | The App Store item identifier for the source app.
--
-- ObjC selector: @- setSourceAppStoreItemIdentifier:@
setSourceAppStoreItemIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setSourceAppStoreItemIdentifier skAdImpression value =
  sendMessage skAdImpression setSourceAppStoreItemIdentifierSelector (toNSNumber value)

-- | The App Store item identifier for the app being advertised.
--
-- ObjC selector: @- advertisedAppStoreItemIdentifier@
advertisedAppStoreItemIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
advertisedAppStoreItemIdentifier skAdImpression =
  sendMessage skAdImpression advertisedAppStoreItemIdentifierSelector

-- | The App Store item identifier for the app being advertised.
--
-- ObjC selector: @- setAdvertisedAppStoreItemIdentifier:@
setAdvertisedAppStoreItemIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setAdvertisedAppStoreItemIdentifier skAdImpression value =
  sendMessage skAdImpression setAdvertisedAppStoreItemIdentifierSelector (toNSNumber value)

-- | The identifier for the ad network.
--
-- ObjC selector: @- adNetworkIdentifier@
adNetworkIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adNetworkIdentifier skAdImpression =
  sendMessage skAdImpression adNetworkIdentifierSelector

-- | The identifier for the ad network.
--
-- ObjC selector: @- setAdNetworkIdentifier:@
setAdNetworkIdentifier :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdNetworkIdentifier skAdImpression value =
  sendMessage skAdImpression setAdNetworkIdentifierSelector (toNSString value)

-- | The ad campaign identifier.
--
-- ObjC selector: @- adCampaignIdentifier@
adCampaignIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
adCampaignIdentifier skAdImpression =
  sendMessage skAdImpression adCampaignIdentifierSelector

-- | The ad campaign identifier.
--
-- ObjC selector: @- setAdCampaignIdentifier:@
setAdCampaignIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setAdCampaignIdentifier skAdImpression value =
  sendMessage skAdImpression setAdCampaignIdentifierSelector (toNSNumber value)

-- | The source identifier
--
-- ObjC selector: @- sourceIdentifier@
sourceIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
sourceIdentifier skAdImpression =
  sendMessage skAdImpression sourceIdentifierSelector

-- | The source identifier
--
-- ObjC selector: @- setSourceIdentifier:@
setSourceIdentifier :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setSourceIdentifier skAdImpression value =
  sendMessage skAdImpression setSourceIdentifierSelector (toNSNumber value)

-- | The nonce used to generate the signature.
--
-- ObjC selector: @- adImpressionIdentifier@
adImpressionIdentifier :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adImpressionIdentifier skAdImpression =
  sendMessage skAdImpression adImpressionIdentifierSelector

-- | The nonce used to generate the signature.
--
-- ObjC selector: @- setAdImpressionIdentifier:@
setAdImpressionIdentifier :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdImpressionIdentifier skAdImpression value =
  sendMessage skAdImpression setAdImpressionIdentifierSelector (toNSString value)

-- | The type of ad being presented.
--
-- ObjC selector: @- adType@
adType :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adType skAdImpression =
  sendMessage skAdImpression adTypeSelector

-- | The type of ad being presented.
--
-- ObjC selector: @- setAdType:@
setAdType :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdType skAdImpression value =
  sendMessage skAdImpression setAdTypeSelector (toNSString value)

-- | The description of the ad.
--
-- ObjC selector: @- adDescription@
adDescription :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adDescription skAdImpression =
  sendMessage skAdImpression adDescriptionSelector

-- | The description of the ad.
--
-- ObjC selector: @- setAdDescription:@
setAdDescription :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdDescription skAdImpression value =
  sendMessage skAdImpression setAdDescriptionSelector (toNSString value)

-- | Name of entity that purchased the ad being presented.
--
-- ObjC selector: @- adPurchaserName@
adPurchaserName :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
adPurchaserName skAdImpression =
  sendMessage skAdImpression adPurchaserNameSelector

-- | Name of entity that purchased the ad being presented.
--
-- ObjC selector: @- setAdPurchaserName:@
setAdPurchaserName :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setAdPurchaserName skAdImpression value =
  sendMessage skAdImpression setAdPurchaserNameSelector (toNSString value)

-- | The timestamp of the start and end call.
--
-- ObjC selector: @- timestamp@
timestamp :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSNumber)
timestamp skAdImpression =
  sendMessage skAdImpression timestampSelector

-- | The timestamp of the start and end call.
--
-- ObjC selector: @- setTimestamp:@
setTimestamp :: (IsSKAdImpression skAdImpression, IsNSNumber value) => skAdImpression -> value -> IO ()
setTimestamp skAdImpression value =
  sendMessage skAdImpression setTimestampSelector (toNSNumber value)

-- | The signature of the impression.
--
-- ObjC selector: @- signature@
signature :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
signature skAdImpression =
  sendMessage skAdImpression signatureSelector

-- | The signature of the impression.
--
-- ObjC selector: @- setSignature:@
setSignature :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setSignature skAdImpression value =
  sendMessage skAdImpression setSignatureSelector (toNSString value)

-- | The version of SKAdNetwork being used.
--
-- ObjC selector: @- version@
version :: IsSKAdImpression skAdImpression => skAdImpression -> IO (Id NSString)
version skAdImpression =
  sendMessage skAdImpression versionSelector

-- | The version of SKAdNetwork being used.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsSKAdImpression skAdImpression, IsNSString value) => skAdImpression -> value -> IO ()
setVersion skAdImpression value =
  sendMessage skAdImpression setVersionSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceAppStoreItemIdentifier@
sourceAppStoreItemIdentifierSelector :: Selector '[] (Id NSNumber)
sourceAppStoreItemIdentifierSelector = mkSelector "sourceAppStoreItemIdentifier"

-- | @Selector@ for @setSourceAppStoreItemIdentifier:@
setSourceAppStoreItemIdentifierSelector :: Selector '[Id NSNumber] ()
setSourceAppStoreItemIdentifierSelector = mkSelector "setSourceAppStoreItemIdentifier:"

-- | @Selector@ for @advertisedAppStoreItemIdentifier@
advertisedAppStoreItemIdentifierSelector :: Selector '[] (Id NSNumber)
advertisedAppStoreItemIdentifierSelector = mkSelector "advertisedAppStoreItemIdentifier"

-- | @Selector@ for @setAdvertisedAppStoreItemIdentifier:@
setAdvertisedAppStoreItemIdentifierSelector :: Selector '[Id NSNumber] ()
setAdvertisedAppStoreItemIdentifierSelector = mkSelector "setAdvertisedAppStoreItemIdentifier:"

-- | @Selector@ for @adNetworkIdentifier@
adNetworkIdentifierSelector :: Selector '[] (Id NSString)
adNetworkIdentifierSelector = mkSelector "adNetworkIdentifier"

-- | @Selector@ for @setAdNetworkIdentifier:@
setAdNetworkIdentifierSelector :: Selector '[Id NSString] ()
setAdNetworkIdentifierSelector = mkSelector "setAdNetworkIdentifier:"

-- | @Selector@ for @adCampaignIdentifier@
adCampaignIdentifierSelector :: Selector '[] (Id NSNumber)
adCampaignIdentifierSelector = mkSelector "adCampaignIdentifier"

-- | @Selector@ for @setAdCampaignIdentifier:@
setAdCampaignIdentifierSelector :: Selector '[Id NSNumber] ()
setAdCampaignIdentifierSelector = mkSelector "setAdCampaignIdentifier:"

-- | @Selector@ for @sourceIdentifier@
sourceIdentifierSelector :: Selector '[] (Id NSNumber)
sourceIdentifierSelector = mkSelector "sourceIdentifier"

-- | @Selector@ for @setSourceIdentifier:@
setSourceIdentifierSelector :: Selector '[Id NSNumber] ()
setSourceIdentifierSelector = mkSelector "setSourceIdentifier:"

-- | @Selector@ for @adImpressionIdentifier@
adImpressionIdentifierSelector :: Selector '[] (Id NSString)
adImpressionIdentifierSelector = mkSelector "adImpressionIdentifier"

-- | @Selector@ for @setAdImpressionIdentifier:@
setAdImpressionIdentifierSelector :: Selector '[Id NSString] ()
setAdImpressionIdentifierSelector = mkSelector "setAdImpressionIdentifier:"

-- | @Selector@ for @adType@
adTypeSelector :: Selector '[] (Id NSString)
adTypeSelector = mkSelector "adType"

-- | @Selector@ for @setAdType:@
setAdTypeSelector :: Selector '[Id NSString] ()
setAdTypeSelector = mkSelector "setAdType:"

-- | @Selector@ for @adDescription@
adDescriptionSelector :: Selector '[] (Id NSString)
adDescriptionSelector = mkSelector "adDescription"

-- | @Selector@ for @setAdDescription:@
setAdDescriptionSelector :: Selector '[Id NSString] ()
setAdDescriptionSelector = mkSelector "setAdDescription:"

-- | @Selector@ for @adPurchaserName@
adPurchaserNameSelector :: Selector '[] (Id NSString)
adPurchaserNameSelector = mkSelector "adPurchaserName"

-- | @Selector@ for @setAdPurchaserName:@
setAdPurchaserNameSelector :: Selector '[Id NSString] ()
setAdPurchaserNameSelector = mkSelector "setAdPurchaserName:"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSNumber)
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @setTimestamp:@
setTimestampSelector :: Selector '[Id NSNumber] ()
setTimestampSelector = mkSelector "setTimestamp:"

-- | @Selector@ for @signature@
signatureSelector :: Selector '[] (Id NSString)
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector '[Id NSString] ()
setSignatureSelector = mkSelector "setSignature:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSString] ()
setVersionSelector = mkSelector "setVersion:"

