{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductStorePromotionController@.
module ObjC.StoreKit.SKProductStorePromotionController
  ( SKProductStorePromotionController
  , IsSKProductStorePromotionController(..)
  , defaultController
  , fetchStorePromotionVisibilityForProduct_completionHandler
  , updateStorePromotionVisibility_forProduct_completionHandler
  , updateStorePromotionOrder_completionHandler
  , defaultControllerSelector
  , fetchStorePromotionVisibilityForProduct_completionHandlerSelector
  , updateStorePromotionOrder_completionHandlerSelector
  , updateStorePromotionVisibility_forProduct_completionHandlerSelector

  -- * Enum types
  , SKProductStorePromotionVisibility(SKProductStorePromotionVisibility)
  , pattern SKProductStorePromotionVisibilityDefault
  , pattern SKProductStorePromotionVisibilityShow
  , pattern SKProductStorePromotionVisibilityHide

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultController@
defaultController :: IO (Id SKProductStorePromotionController)
defaultController  =
  do
    cls' <- getRequiredClass "SKProductStorePromotionController"
    sendClassMessage cls' defaultControllerSelector

-- | @- fetchStorePromotionVisibilityForProduct:completionHandler:@
fetchStorePromotionVisibilityForProduct_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsSKProduct product_) => skProductStorePromotionController -> product_ -> Ptr () -> IO ()
fetchStorePromotionVisibilityForProduct_completionHandler skProductStorePromotionController product_ completionHandler =
  sendMessage skProductStorePromotionController fetchStorePromotionVisibilityForProduct_completionHandlerSelector (toSKProduct product_) completionHandler

-- | @- updateStorePromotionVisibility:forProduct:completionHandler:@
updateStorePromotionVisibility_forProduct_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsSKProduct product_) => skProductStorePromotionController -> SKProductStorePromotionVisibility -> product_ -> Ptr () -> IO ()
updateStorePromotionVisibility_forProduct_completionHandler skProductStorePromotionController promotionVisibility product_ completionHandler =
  sendMessage skProductStorePromotionController updateStorePromotionVisibility_forProduct_completionHandlerSelector promotionVisibility (toSKProduct product_) completionHandler

-- | @- updateStorePromotionOrder:completionHandler:@
updateStorePromotionOrder_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsNSArray promotionOrder) => skProductStorePromotionController -> promotionOrder -> Ptr () -> IO ()
updateStorePromotionOrder_completionHandler skProductStorePromotionController promotionOrder completionHandler =
  sendMessage skProductStorePromotionController updateStorePromotionOrder_completionHandlerSelector (toNSArray promotionOrder) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultController@
defaultControllerSelector :: Selector '[] (Id SKProductStorePromotionController)
defaultControllerSelector = mkSelector "defaultController"

-- | @Selector@ for @fetchStorePromotionVisibilityForProduct:completionHandler:@
fetchStorePromotionVisibilityForProduct_completionHandlerSelector :: Selector '[Id SKProduct, Ptr ()] ()
fetchStorePromotionVisibilityForProduct_completionHandlerSelector = mkSelector "fetchStorePromotionVisibilityForProduct:completionHandler:"

-- | @Selector@ for @updateStorePromotionVisibility:forProduct:completionHandler:@
updateStorePromotionVisibility_forProduct_completionHandlerSelector :: Selector '[SKProductStorePromotionVisibility, Id SKProduct, Ptr ()] ()
updateStorePromotionVisibility_forProduct_completionHandlerSelector = mkSelector "updateStorePromotionVisibility:forProduct:completionHandler:"

-- | @Selector@ for @updateStorePromotionOrder:completionHandler:@
updateStorePromotionOrder_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
updateStorePromotionOrder_completionHandlerSelector = mkSelector "updateStorePromotionOrder:completionHandler:"

