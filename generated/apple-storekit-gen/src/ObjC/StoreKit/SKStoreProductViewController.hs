{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKStoreProductViewController@.
module ObjC.StoreKit.SKStoreProductViewController
  ( SKStoreProductViewController
  , IsSKStoreProductViewController(..)
  , loadProductWithParameters_completionBlock
  , loadProductWithParameters_impression_completionBlock
  , delegate
  , setDelegate
  , delegateSelector
  , loadProductWithParameters_completionBlockSelector
  , loadProductWithParameters_impression_completionBlockSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadProductWithParameters:completionBlock:@
loadProductWithParameters_completionBlock :: (IsSKStoreProductViewController skStoreProductViewController, IsNSDictionary parameters) => skStoreProductViewController -> parameters -> Ptr () -> IO ()
loadProductWithParameters_completionBlock skStoreProductViewController parameters block =
  sendMessage skStoreProductViewController loadProductWithParameters_completionBlockSelector (toNSDictionary parameters) block

-- | @- loadProductWithParameters:impression:completionBlock:@
loadProductWithParameters_impression_completionBlock :: (IsSKStoreProductViewController skStoreProductViewController, IsNSDictionary parameters, IsSKAdImpression impression) => skStoreProductViewController -> parameters -> impression -> Ptr () -> IO ()
loadProductWithParameters_impression_completionBlock skStoreProductViewController parameters impression block =
  sendMessage skStoreProductViewController loadProductWithParameters_impression_completionBlockSelector (toNSDictionary parameters) (toSKAdImpression impression) block

-- | @- delegate@
delegate :: IsSKStoreProductViewController skStoreProductViewController => skStoreProductViewController -> IO RawId
delegate skStoreProductViewController =
  sendMessage skStoreProductViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSKStoreProductViewController skStoreProductViewController => skStoreProductViewController -> RawId -> IO ()
setDelegate skStoreProductViewController value =
  sendMessage skStoreProductViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadProductWithParameters:completionBlock:@
loadProductWithParameters_completionBlockSelector :: Selector '[Id NSDictionary, Ptr ()] ()
loadProductWithParameters_completionBlockSelector = mkSelector "loadProductWithParameters:completionBlock:"

-- | @Selector@ for @loadProductWithParameters:impression:completionBlock:@
loadProductWithParameters_impression_completionBlockSelector :: Selector '[Id NSDictionary, Id SKAdImpression, Ptr ()] ()
loadProductWithParameters_impression_completionBlockSelector = mkSelector "loadProductWithParameters:impression:completionBlock:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

