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
  , setNetworkAccessAllowedSelector
  , progressHandlerSelector
  , setProgressHandlerSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> IO Bool
networkAccessAllowed phAssetResourceRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAssetResourceRequestOptions (mkSelector "networkAccessAllowed") retCULong []

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phAssetResourceRequestOptions  value =
  sendMsg phAssetResourceRequestOptions (mkSelector "setNetworkAccessAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- progressHandler@
progressHandler :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> IO (Ptr ())
progressHandler phAssetResourceRequestOptions  =
  fmap castPtr $ sendMsg phAssetResourceRequestOptions (mkSelector "progressHandler") (retPtr retVoid) []

-- | @- setProgressHandler:@
setProgressHandler :: IsPHAssetResourceRequestOptions phAssetResourceRequestOptions => phAssetResourceRequestOptions -> Ptr () -> IO ()
setProgressHandler phAssetResourceRequestOptions  value =
  sendMsg phAssetResourceRequestOptions (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector
setProgressHandlerSelector = mkSelector "setProgressHandler:"

