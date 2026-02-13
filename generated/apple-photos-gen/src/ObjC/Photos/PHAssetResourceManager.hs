{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetResourceManager@.
module ObjC.Photos.PHAssetResourceManager
  ( PHAssetResourceManager
  , IsPHAssetResourceManager(..)
  , defaultManager
  , requestDataForAssetResource_options_dataReceivedHandler_completionHandler
  , writeDataForAssetResource_toFile_options_completionHandler
  , cancelDataRequest
  , cancelDataRequestSelector
  , defaultManagerSelector
  , requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector
  , writeDataForAssetResource_toFile_options_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultManager@
defaultManager :: IO (Id PHAssetResourceManager)
defaultManager  =
  do
    cls' <- getRequiredClass "PHAssetResourceManager"
    sendClassMessage cls' defaultManagerSelector

-- | @- requestDataForAssetResource:options:dataReceivedHandler:completionHandler:@
requestDataForAssetResource_options_dataReceivedHandler_completionHandler :: (IsPHAssetResourceManager phAssetResourceManager, IsPHAssetResource resource, IsPHAssetResourceRequestOptions options) => phAssetResourceManager -> resource -> options -> Ptr () -> Ptr () -> IO CInt
requestDataForAssetResource_options_dataReceivedHandler_completionHandler phAssetResourceManager resource options handler completionHandler =
  sendMessage phAssetResourceManager requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector (toPHAssetResource resource) (toPHAssetResourceRequestOptions options) handler completionHandler

-- | @- writeDataForAssetResource:toFile:options:completionHandler:@
writeDataForAssetResource_toFile_options_completionHandler :: (IsPHAssetResourceManager phAssetResourceManager, IsPHAssetResource resource, IsNSURL fileURL, IsPHAssetResourceRequestOptions options) => phAssetResourceManager -> resource -> fileURL -> options -> Ptr () -> IO ()
writeDataForAssetResource_toFile_options_completionHandler phAssetResourceManager resource fileURL options completionHandler =
  sendMessage phAssetResourceManager writeDataForAssetResource_toFile_options_completionHandlerSelector (toPHAssetResource resource) (toNSURL fileURL) (toPHAssetResourceRequestOptions options) completionHandler

-- | @- cancelDataRequest:@
cancelDataRequest :: IsPHAssetResourceManager phAssetResourceManager => phAssetResourceManager -> CInt -> IO ()
cancelDataRequest phAssetResourceManager requestID =
  sendMessage phAssetResourceManager cancelDataRequestSelector requestID

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id PHAssetResourceManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @requestDataForAssetResource:options:dataReceivedHandler:completionHandler:@
requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector :: Selector '[Id PHAssetResource, Id PHAssetResourceRequestOptions, Ptr (), Ptr ()] CInt
requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector = mkSelector "requestDataForAssetResource:options:dataReceivedHandler:completionHandler:"

-- | @Selector@ for @writeDataForAssetResource:toFile:options:completionHandler:@
writeDataForAssetResource_toFile_options_completionHandlerSelector :: Selector '[Id PHAssetResource, Id NSURL, Id PHAssetResourceRequestOptions, Ptr ()] ()
writeDataForAssetResource_toFile_options_completionHandlerSelector = mkSelector "writeDataForAssetResource:toFile:options:completionHandler:"

-- | @Selector@ for @cancelDataRequest:@
cancelDataRequestSelector :: Selector '[CInt] ()
cancelDataRequestSelector = mkSelector "cancelDataRequest:"

