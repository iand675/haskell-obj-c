{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.StoreKit.Internal.Classes (
    module ObjC.StoreKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SKAdImpression ----------

-- | Phantom type for @SKAdImpression@.
data SKAdImpression

instance IsObjCObject (Id SKAdImpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAdImpression"

class IsNSObject a => IsSKAdImpression a where
  toSKAdImpression :: a -> Id SKAdImpression

instance IsSKAdImpression (Id SKAdImpression) where
  toSKAdImpression = unsafeCastId

instance IsNSObject (Id SKAdImpression) where
  toNSObject = unsafeCastId

-- ---------- SKAdNetwork ----------

-- | Phantom type for @SKAdNetwork@.
data SKAdNetwork

instance IsObjCObject (Id SKAdNetwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAdNetwork"

class IsNSObject a => IsSKAdNetwork a where
  toSKAdNetwork :: a -> Id SKAdNetwork

instance IsSKAdNetwork (Id SKAdNetwork) where
  toSKAdNetwork = unsafeCastId

instance IsNSObject (Id SKAdNetwork) where
  toNSObject = unsafeCastId

-- ---------- SKArcadeService ----------

-- | Phantom type for @SKArcadeService@.
data SKArcadeService

instance IsObjCObject (Id SKArcadeService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKArcadeService"

class IsNSObject a => IsSKArcadeService a where
  toSKArcadeService :: a -> Id SKArcadeService

instance IsSKArcadeService (Id SKArcadeService) where
  toSKArcadeService = unsafeCastId

instance IsNSObject (Id SKArcadeService) where
  toNSObject = unsafeCastId

-- ---------- SKCloudServiceController ----------

-- | Phantom type for @SKCloudServiceController@.
data SKCloudServiceController

instance IsObjCObject (Id SKCloudServiceController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKCloudServiceController"

class IsNSObject a => IsSKCloudServiceController a where
  toSKCloudServiceController :: a -> Id SKCloudServiceController

instance IsSKCloudServiceController (Id SKCloudServiceController) where
  toSKCloudServiceController = unsafeCastId

instance IsNSObject (Id SKCloudServiceController) where
  toNSObject = unsafeCastId

-- ---------- SKDownload ----------

-- | Phantom type for @SKDownload@.
data SKDownload

instance IsObjCObject (Id SKDownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKDownload"

class IsNSObject a => IsSKDownload a where
  toSKDownload :: a -> Id SKDownload

instance IsSKDownload (Id SKDownload) where
  toSKDownload = unsafeCastId

instance IsNSObject (Id SKDownload) where
  toNSObject = unsafeCastId

-- ---------- SKOverlay ----------

-- | Phantom type for @SKOverlay@.
data SKOverlay

instance IsObjCObject (Id SKOverlay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKOverlay"

class IsNSObject a => IsSKOverlay a where
  toSKOverlay :: a -> Id SKOverlay

instance IsSKOverlay (Id SKOverlay) where
  toSKOverlay = unsafeCastId

instance IsNSObject (Id SKOverlay) where
  toNSObject = unsafeCastId

-- ---------- SKOverlayConfiguration ----------

-- | Phantom type for @SKOverlayConfiguration@.
data SKOverlayConfiguration

instance IsObjCObject (Id SKOverlayConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKOverlayConfiguration"

class IsNSObject a => IsSKOverlayConfiguration a where
  toSKOverlayConfiguration :: a -> Id SKOverlayConfiguration

instance IsSKOverlayConfiguration (Id SKOverlayConfiguration) where
  toSKOverlayConfiguration = unsafeCastId

instance IsNSObject (Id SKOverlayConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SKOverlayTransitionContext ----------

-- | Phantom type for @SKOverlayTransitionContext@.
data SKOverlayTransitionContext

instance IsObjCObject (Id SKOverlayTransitionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKOverlayTransitionContext"

class IsNSObject a => IsSKOverlayTransitionContext a where
  toSKOverlayTransitionContext :: a -> Id SKOverlayTransitionContext

instance IsSKOverlayTransitionContext (Id SKOverlayTransitionContext) where
  toSKOverlayTransitionContext = unsafeCastId

instance IsNSObject (Id SKOverlayTransitionContext) where
  toNSObject = unsafeCastId

-- ---------- SKPayment ----------

-- | Phantom type for @SKPayment@.
data SKPayment

instance IsObjCObject (Id SKPayment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPayment"

class IsNSObject a => IsSKPayment a where
  toSKPayment :: a -> Id SKPayment

instance IsSKPayment (Id SKPayment) where
  toSKPayment = unsafeCastId

instance IsNSObject (Id SKPayment) where
  toNSObject = unsafeCastId

-- ---------- SKPaymentDiscount ----------

-- | Phantom type for @SKPaymentDiscount@.
data SKPaymentDiscount

instance IsObjCObject (Id SKPaymentDiscount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPaymentDiscount"

class IsNSObject a => IsSKPaymentDiscount a where
  toSKPaymentDiscount :: a -> Id SKPaymentDiscount

instance IsSKPaymentDiscount (Id SKPaymentDiscount) where
  toSKPaymentDiscount = unsafeCastId

instance IsNSObject (Id SKPaymentDiscount) where
  toNSObject = unsafeCastId

-- ---------- SKPaymentQueue ----------

-- | Phantom type for @SKPaymentQueue@.
data SKPaymentQueue

instance IsObjCObject (Id SKPaymentQueue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPaymentQueue"

class IsNSObject a => IsSKPaymentQueue a where
  toSKPaymentQueue :: a -> Id SKPaymentQueue

instance IsSKPaymentQueue (Id SKPaymentQueue) where
  toSKPaymentQueue = unsafeCastId

instance IsNSObject (Id SKPaymentQueue) where
  toNSObject = unsafeCastId

-- ---------- SKPaymentTransaction ----------

-- | Phantom type for @SKPaymentTransaction@.
data SKPaymentTransaction

instance IsObjCObject (Id SKPaymentTransaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPaymentTransaction"

class IsNSObject a => IsSKPaymentTransaction a where
  toSKPaymentTransaction :: a -> Id SKPaymentTransaction

instance IsSKPaymentTransaction (Id SKPaymentTransaction) where
  toSKPaymentTransaction = unsafeCastId

instance IsNSObject (Id SKPaymentTransaction) where
  toNSObject = unsafeCastId

-- ---------- SKProduct ----------

-- | Phantom type for @SKProduct@.
data SKProduct

instance IsObjCObject (Id SKProduct) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProduct"

class IsNSObject a => IsSKProduct a where
  toSKProduct :: a -> Id SKProduct

instance IsSKProduct (Id SKProduct) where
  toSKProduct = unsafeCastId

instance IsNSObject (Id SKProduct) where
  toNSObject = unsafeCastId

-- ---------- SKProductDiscount ----------

-- | Phantom type for @SKProductDiscount@.
data SKProductDiscount

instance IsObjCObject (Id SKProductDiscount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProductDiscount"

class IsNSObject a => IsSKProductDiscount a where
  toSKProductDiscount :: a -> Id SKProductDiscount

instance IsSKProductDiscount (Id SKProductDiscount) where
  toSKProductDiscount = unsafeCastId

instance IsNSObject (Id SKProductDiscount) where
  toNSObject = unsafeCastId

-- ---------- SKProductStorePromotionController ----------

-- | Phantom type for @SKProductStorePromotionController@.
data SKProductStorePromotionController

instance IsObjCObject (Id SKProductStorePromotionController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProductStorePromotionController"

class IsNSObject a => IsSKProductStorePromotionController a where
  toSKProductStorePromotionController :: a -> Id SKProductStorePromotionController

instance IsSKProductStorePromotionController (Id SKProductStorePromotionController) where
  toSKProductStorePromotionController = unsafeCastId

instance IsNSObject (Id SKProductStorePromotionController) where
  toNSObject = unsafeCastId

-- ---------- SKProductSubscriptionPeriod ----------

-- | Phantom type for @SKProductSubscriptionPeriod@.
data SKProductSubscriptionPeriod

instance IsObjCObject (Id SKProductSubscriptionPeriod) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProductSubscriptionPeriod"

class IsNSObject a => IsSKProductSubscriptionPeriod a where
  toSKProductSubscriptionPeriod :: a -> Id SKProductSubscriptionPeriod

instance IsSKProductSubscriptionPeriod (Id SKProductSubscriptionPeriod) where
  toSKProductSubscriptionPeriod = unsafeCastId

instance IsNSObject (Id SKProductSubscriptionPeriod) where
  toNSObject = unsafeCastId

-- ---------- SKProductsResponse ----------

-- | Phantom type for @SKProductsResponse@.
data SKProductsResponse

instance IsObjCObject (Id SKProductsResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProductsResponse"

class IsNSObject a => IsSKProductsResponse a where
  toSKProductsResponse :: a -> Id SKProductsResponse

instance IsSKProductsResponse (Id SKProductsResponse) where
  toSKProductsResponse = unsafeCastId

instance IsNSObject (Id SKProductsResponse) where
  toNSObject = unsafeCastId

-- ---------- SKRequest ----------

-- | Phantom type for @SKRequest@.
data SKRequest

instance IsObjCObject (Id SKRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKRequest"

class IsNSObject a => IsSKRequest a where
  toSKRequest :: a -> Id SKRequest

instance IsSKRequest (Id SKRequest) where
  toSKRequest = unsafeCastId

instance IsNSObject (Id SKRequest) where
  toNSObject = unsafeCastId

-- ---------- SKStoreReviewController ----------

-- | Controller class to request a review from the current user
-- 
-- Phantom type for @SKStoreReviewController@.
data SKStoreReviewController

instance IsObjCObject (Id SKStoreReviewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKStoreReviewController"

class IsNSObject a => IsSKStoreReviewController a where
  toSKStoreReviewController :: a -> Id SKStoreReviewController

instance IsSKStoreReviewController (Id SKStoreReviewController) where
  toSKStoreReviewController = unsafeCastId

instance IsNSObject (Id SKStoreReviewController) where
  toNSObject = unsafeCastId

-- ---------- SKStorefront ----------

-- | Phantom type for @SKStorefront@.
data SKStorefront

instance IsObjCObject (Id SKStorefront) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKStorefront"

class IsNSObject a => IsSKStorefront a where
  toSKStorefront :: a -> Id SKStorefront

instance IsSKStorefront (Id SKStorefront) where
  toSKStorefront = unsafeCastId

instance IsNSObject (Id SKStorefront) where
  toNSObject = unsafeCastId

-- ---------- SKOverlayAppClipConfiguration ----------

-- | An overlay configuration that can be used to show an app clip's full app.
-- 
-- Phantom type for @SKOverlayAppClipConfiguration@.
data SKOverlayAppClipConfiguration

instance IsObjCObject (Id SKOverlayAppClipConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKOverlayAppClipConfiguration"

class IsSKOverlayConfiguration a => IsSKOverlayAppClipConfiguration a where
  toSKOverlayAppClipConfiguration :: a -> Id SKOverlayAppClipConfiguration

instance IsSKOverlayAppClipConfiguration (Id SKOverlayAppClipConfiguration) where
  toSKOverlayAppClipConfiguration = unsafeCastId

instance IsNSObject (Id SKOverlayAppClipConfiguration) where
  toNSObject = unsafeCastId

instance IsSKOverlayConfiguration (Id SKOverlayAppClipConfiguration) where
  toSKOverlayConfiguration = unsafeCastId

-- ---------- SKOverlayAppConfiguration ----------

-- | An overlay configuration that can be used to show any app from the App Store.
-- 
-- Phantom type for @SKOverlayAppConfiguration@.
data SKOverlayAppConfiguration

instance IsObjCObject (Id SKOverlayAppConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKOverlayAppConfiguration"

class IsSKOverlayConfiguration a => IsSKOverlayAppConfiguration a where
  toSKOverlayAppConfiguration :: a -> Id SKOverlayAppConfiguration

instance IsSKOverlayAppConfiguration (Id SKOverlayAppConfiguration) where
  toSKOverlayAppConfiguration = unsafeCastId

instance IsNSObject (Id SKOverlayAppConfiguration) where
  toNSObject = unsafeCastId

instance IsSKOverlayConfiguration (Id SKOverlayAppConfiguration) where
  toSKOverlayConfiguration = unsafeCastId

-- ---------- SKMutablePayment ----------

-- | Phantom type for @SKMutablePayment@.
data SKMutablePayment

instance IsObjCObject (Id SKMutablePayment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKMutablePayment"

class IsSKPayment a => IsSKMutablePayment a where
  toSKMutablePayment :: a -> Id SKMutablePayment

instance IsSKMutablePayment (Id SKMutablePayment) where
  toSKMutablePayment = unsafeCastId

instance IsNSObject (Id SKMutablePayment) where
  toNSObject = unsafeCastId

instance IsSKPayment (Id SKMutablePayment) where
  toSKPayment = unsafeCastId

-- ---------- SKProductsRequest ----------

-- | Phantom type for @SKProductsRequest@.
data SKProductsRequest

instance IsObjCObject (Id SKProductsRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKProductsRequest"

class IsSKRequest a => IsSKProductsRequest a where
  toSKProductsRequest :: a -> Id SKProductsRequest

instance IsSKProductsRequest (Id SKProductsRequest) where
  toSKProductsRequest = unsafeCastId

instance IsNSObject (Id SKProductsRequest) where
  toNSObject = unsafeCastId

instance IsSKRequest (Id SKProductsRequest) where
  toSKRequest = unsafeCastId

-- ---------- SKReceiptRefreshRequest ----------

-- | Phantom type for @SKReceiptRefreshRequest@.
data SKReceiptRefreshRequest

instance IsObjCObject (Id SKReceiptRefreshRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKReceiptRefreshRequest"

class IsSKRequest a => IsSKReceiptRefreshRequest a where
  toSKReceiptRefreshRequest :: a -> Id SKReceiptRefreshRequest

instance IsSKReceiptRefreshRequest (Id SKReceiptRefreshRequest) where
  toSKReceiptRefreshRequest = unsafeCastId

instance IsNSObject (Id SKReceiptRefreshRequest) where
  toNSObject = unsafeCastId

instance IsSKRequest (Id SKReceiptRefreshRequest) where
  toSKRequest = unsafeCastId

-- ---------- SKCloudServiceSetupViewController ----------

-- | Phantom type for @SKCloudServiceSetupViewController@.
data SKCloudServiceSetupViewController

instance IsObjCObject (Id SKCloudServiceSetupViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKCloudServiceSetupViewController"

class IsNSViewController a => IsSKCloudServiceSetupViewController a where
  toSKCloudServiceSetupViewController :: a -> Id SKCloudServiceSetupViewController

instance IsSKCloudServiceSetupViewController (Id SKCloudServiceSetupViewController) where
  toSKCloudServiceSetupViewController = unsafeCastId

instance IsNSObject (Id SKCloudServiceSetupViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKCloudServiceSetupViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id SKCloudServiceSetupViewController) where
  toNSViewController = unsafeCastId

-- ---------- SKStoreProductViewController ----------

-- | Phantom type for @SKStoreProductViewController@.
data SKStoreProductViewController

instance IsObjCObject (Id SKStoreProductViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKStoreProductViewController"

class IsNSViewController a => IsSKStoreProductViewController a where
  toSKStoreProductViewController :: a -> Id SKStoreProductViewController

instance IsSKStoreProductViewController (Id SKStoreProductViewController) where
  toSKStoreProductViewController = unsafeCastId

instance IsNSObject (Id SKStoreProductViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKStoreProductViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id SKStoreProductViewController) where
  toNSViewController = unsafeCastId
