{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetResourceRequestOptions@.
module ObjC.Photos.PHAssetResourceRequestOptions
  ( PHAssetResourceRequestOptions
  , IsPHAssetResourceRequestOptions(..)
  , networkAccessAllowed
  , setNetworkAccessAllowed
  , progressHandler
  , setProgressHandler
  , networkAccessAllowedSelector
  , progressHandlerSelector
  , setNetworkAccessAllowedSelector
  , setProgressHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> IO Bool
networkAccessAllowed phAssetResourceRequestOptions =
  sendMessage phAssetResourceRequestOptions networkAccessAllowedSelector

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phAssetResourceRequestOptions value =
  sendMessage phAssetResourceRequestOptions setNetworkAccessAllowedSelector value

-- | @- progressHandler@
progressHandler :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> IO (Ptr ())
progressHandler phAssetResourceRequestOptions =
  sendMessage phAssetResourceRequestOptions progressHandlerSelector

-- | @- setProgressHandler:@
setProgressHandler :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> Ptr () -> IO ()
setProgressHandler phAssetResourceRequestOptions value =
  sendMessage phAssetResourceRequestOptions setProgressHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector '[] Bool
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector '[Bool] ()
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector '[] (Ptr ())
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector '[Ptr ()] ()
setProgressHandlerSelector = mkSelector "setProgressHandler:"

