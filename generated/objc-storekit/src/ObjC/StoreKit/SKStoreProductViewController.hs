{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKStoreProductViewController@.
module ObjC.StoreKit.SKStoreProductViewController
  ( SKStoreProductViewController
  , IsSKStoreProductViewController(..)
  , loadProductWithParameters_completionBlock
  , loadProductWithParameters_impression_completionBlock
  , loadProductWithParameters_completionBlockSelector
  , loadProductWithParameters_impression_completionBlockSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadProductWithParameters:completionBlock:@
loadProductWithParameters_completionBlock :: (IsSKStoreProductViewController skStoreProductViewController, IsNSDictionary parameters) => skStoreProductViewController -> parameters -> Ptr () -> IO ()
loadProductWithParameters_completionBlock skStoreProductViewController  parameters block =
withObjCPtr parameters $ \raw_parameters ->
    sendMsg skStoreProductViewController (mkSelector "loadProductWithParameters:completionBlock:") retVoid [argPtr (castPtr raw_parameters :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- loadProductWithParameters:impression:completionBlock:@
loadProductWithParameters_impression_completionBlock :: (IsSKStoreProductViewController skStoreProductViewController, IsNSDictionary parameters, IsSKAdImpression impression) => skStoreProductViewController -> parameters -> impression -> Ptr () -> IO ()
loadProductWithParameters_impression_completionBlock skStoreProductViewController  parameters impression block =
withObjCPtr parameters $ \raw_parameters ->
  withObjCPtr impression $ \raw_impression ->
      sendMsg skStoreProductViewController (mkSelector "loadProductWithParameters:impression:completionBlock:") retVoid [argPtr (castPtr raw_parameters :: Ptr ()), argPtr (castPtr raw_impression :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadProductWithParameters:completionBlock:@
loadProductWithParameters_completionBlockSelector :: Selector
loadProductWithParameters_completionBlockSelector = mkSelector "loadProductWithParameters:completionBlock:"

-- | @Selector@ for @loadProductWithParameters:impression:completionBlock:@
loadProductWithParameters_impression_completionBlockSelector :: Selector
loadProductWithParameters_impression_completionBlockSelector = mkSelector "loadProductWithParameters:impression:completionBlock:"

