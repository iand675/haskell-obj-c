{-# LANGUAGE PatternSynonyms #-}
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
  , updateStorePromotionVisibility_forProduct_completionHandlerSelector
  , updateStorePromotionOrder_completionHandlerSelector

  -- * Enum types
  , SKProductStorePromotionVisibility(SKProductStorePromotionVisibility)
  , pattern SKProductStorePromotionVisibilityDefault
  , pattern SKProductStorePromotionVisibilityShow
  , pattern SKProductStorePromotionVisibilityHide

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
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultController@
defaultController :: IO (Id SKProductStorePromotionController)
defaultController  =
  do
    cls' <- getRequiredClass "SKProductStorePromotionController"
    sendClassMsg cls' (mkSelector "defaultController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchStorePromotionVisibilityForProduct:completionHandler:@
fetchStorePromotionVisibilityForProduct_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsSKProduct product_) => skProductStorePromotionController -> product_ -> Ptr () -> IO ()
fetchStorePromotionVisibilityForProduct_completionHandler skProductStorePromotionController  product_ completionHandler =
withObjCPtr product_ $ \raw_product_ ->
    sendMsg skProductStorePromotionController (mkSelector "fetchStorePromotionVisibilityForProduct:completionHandler:") retVoid [argPtr (castPtr raw_product_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateStorePromotionVisibility:forProduct:completionHandler:@
updateStorePromotionVisibility_forProduct_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsSKProduct product_) => skProductStorePromotionController -> SKProductStorePromotionVisibility -> product_ -> Ptr () -> IO ()
updateStorePromotionVisibility_forProduct_completionHandler skProductStorePromotionController  promotionVisibility product_ completionHandler =
withObjCPtr product_ $ \raw_product_ ->
    sendMsg skProductStorePromotionController (mkSelector "updateStorePromotionVisibility:forProduct:completionHandler:") retVoid [argCLong (coerce promotionVisibility), argPtr (castPtr raw_product_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateStorePromotionOrder:completionHandler:@
updateStorePromotionOrder_completionHandler :: (IsSKProductStorePromotionController skProductStorePromotionController, IsNSArray promotionOrder) => skProductStorePromotionController -> promotionOrder -> Ptr () -> IO ()
updateStorePromotionOrder_completionHandler skProductStorePromotionController  promotionOrder completionHandler =
withObjCPtr promotionOrder $ \raw_promotionOrder ->
    sendMsg skProductStorePromotionController (mkSelector "updateStorePromotionOrder:completionHandler:") retVoid [argPtr (castPtr raw_promotionOrder :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultController@
defaultControllerSelector :: Selector
defaultControllerSelector = mkSelector "defaultController"

-- | @Selector@ for @fetchStorePromotionVisibilityForProduct:completionHandler:@
fetchStorePromotionVisibilityForProduct_completionHandlerSelector :: Selector
fetchStorePromotionVisibilityForProduct_completionHandlerSelector = mkSelector "fetchStorePromotionVisibilityForProduct:completionHandler:"

-- | @Selector@ for @updateStorePromotionVisibility:forProduct:completionHandler:@
updateStorePromotionVisibility_forProduct_completionHandlerSelector :: Selector
updateStorePromotionVisibility_forProduct_completionHandlerSelector = mkSelector "updateStorePromotionVisibility:forProduct:completionHandler:"

-- | @Selector@ for @updateStorePromotionOrder:completionHandler:@
updateStorePromotionOrder_completionHandlerSelector :: Selector
updateStorePromotionOrder_completionHandlerSelector = mkSelector "updateStorePromotionOrder:completionHandler:"

