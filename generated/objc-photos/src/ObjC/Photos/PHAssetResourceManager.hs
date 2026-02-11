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
  , defaultManagerSelector
  , requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector
  , writeDataForAssetResource_toFile_options_completionHandlerSelector
  , cancelDataRequestSelector


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

-- | @+ defaultManager@
defaultManager :: IO (Id PHAssetResourceManager)
defaultManager  =
  do
    cls' <- getRequiredClass "PHAssetResourceManager"
    sendClassMsg cls' (mkSelector "defaultManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestDataForAssetResource:options:dataReceivedHandler:completionHandler:@
requestDataForAssetResource_options_dataReceivedHandler_completionHandler :: (IsPHAssetResourceManager phAssetResourceManager, IsPHAssetResource resource, IsPHAssetResourceRequestOptions options) => phAssetResourceManager -> resource -> options -> Ptr () -> Ptr () -> IO CInt
requestDataForAssetResource_options_dataReceivedHandler_completionHandler phAssetResourceManager  resource options handler completionHandler =
withObjCPtr resource $ \raw_resource ->
  withObjCPtr options $ \raw_options ->
      sendMsg phAssetResourceManager (mkSelector "requestDataForAssetResource:options:dataReceivedHandler:completionHandler:") retCInt [argPtr (castPtr raw_resource :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr handler :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeDataForAssetResource:toFile:options:completionHandler:@
writeDataForAssetResource_toFile_options_completionHandler :: (IsPHAssetResourceManager phAssetResourceManager, IsPHAssetResource resource, IsNSURL fileURL, IsPHAssetResourceRequestOptions options) => phAssetResourceManager -> resource -> fileURL -> options -> Ptr () -> IO ()
writeDataForAssetResource_toFile_options_completionHandler phAssetResourceManager  resource fileURL options completionHandler =
withObjCPtr resource $ \raw_resource ->
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr options $ \raw_options ->
        sendMsg phAssetResourceManager (mkSelector "writeDataForAssetResource:toFile:options:completionHandler:") retVoid [argPtr (castPtr raw_resource :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancelDataRequest:@
cancelDataRequest :: IsPHAssetResourceManager phAssetResourceManager => phAssetResourceManager -> CInt -> IO ()
cancelDataRequest phAssetResourceManager  requestID =
  sendMsg phAssetResourceManager (mkSelector "cancelDataRequest:") retVoid [argCInt (fromIntegral requestID)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @requestDataForAssetResource:options:dataReceivedHandler:completionHandler:@
requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector :: Selector
requestDataForAssetResource_options_dataReceivedHandler_completionHandlerSelector = mkSelector "requestDataForAssetResource:options:dataReceivedHandler:completionHandler:"

-- | @Selector@ for @writeDataForAssetResource:toFile:options:completionHandler:@
writeDataForAssetResource_toFile_options_completionHandlerSelector :: Selector
writeDataForAssetResource_toFile_options_completionHandlerSelector = mkSelector "writeDataForAssetResource:toFile:options:completionHandler:"

-- | @Selector@ for @cancelDataRequest:@
cancelDataRequestSelector :: Selector
cancelDataRequestSelector = mkSelector "cancelDataRequest:"

