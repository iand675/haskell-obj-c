{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetResourceLoadingRequest@.
module ObjC.AVFoundation.AVAssetResourceLoadingRequest
  ( AVAssetResourceLoadingRequest
  , IsAVAssetResourceLoadingRequest(..)
  , init_
  , new
  , finishLoading
  , finishLoadingWithError
  , finishLoadingWithResponse_data_redirect
  , streamingContentKeyRequestDataForApp_contentIdentifier_options_error
  , persistentContentKeyFromKeyVendorResponse_options_error
  , request
  , finished
  , cancelled
  , initSelector
  , newSelector
  , finishLoadingSelector
  , finishLoadingWithErrorSelector
  , finishLoadingWithResponse_data_redirectSelector
  , streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector
  , persistentContentKeyFromKeyVendorResponse_options_errorSelector
  , requestSelector
  , finishedSelector
  , cancelledSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id AVAssetResourceLoadingRequest)
init_ avAssetResourceLoadingRequest  =
  sendMsg avAssetResourceLoadingRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingRequest)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | finishLoading
--
-- Causes the receiver to treat the processing of the request as complete.
--
-- If a dataRequest is present, and the resource does not contain the full extent of the data that has been requested according to the values of the requestedOffset and requestedLength properties of the dataRequest, or if requestsAllDataToEndOfResource has a value of YES, -finishLoading may be invoked after providing as much of the requested data as the resource contains. If the contentInformationRequest property is not nil and specifies a non-empty allowedContentTypes array, the contentInformationRequest's contentType property must be set to a value within allowedContentTypes. Otherwise, this method will throw an exception.
--
-- ObjC selector: @- finishLoading@
finishLoading :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO ()
finishLoading avAssetResourceLoadingRequest  =
  sendMsg avAssetResourceLoadingRequest (mkSelector "finishLoading") retVoid []

-- | finishLoadingWithError:
--
-- Causes the receiver to treat the request as having failed.
--
-- @error@ — An instance of NSError indicating the reason for failure.
--
-- ObjC selector: @- finishLoadingWithError:@
finishLoadingWithError :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSError error_) => avAssetResourceLoadingRequest -> error_ -> IO ()
finishLoadingWithError avAssetResourceLoadingRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg avAssetResourceLoadingRequest (mkSelector "finishLoadingWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | finishLoadingWithResponse:data:redirect:
--
-- Causes the receiver to finish loading a resource that a delegate has previously assumed responsibility for loading by returning YES as the result of -resourceLoader:shouldWaitForLoadingOfRequestedResource:.
--
-- @response@ — The NSURLResponse for the NSURLRequest of the receiver. Should be nil if no response is required.
--
-- @data@ — An instance of NSData containing the data of the resource. Should be nil if no such data is available.
--
-- @redirect@ — An instance of NSURLRequest indicating a redirect of the loading request. Should be nil if no redirect is needed.
--
-- This method is deprecated. Use the following methods instead.					-[AVAssetResourceLoadingRequest setResponse:] to set the response property,					-[AVAssetResourceLoadingRequest setRedirect:] to set the redirect property,					-[AVAssetResourceLoadingDataRequest respondWithData:] to provide data, and					-[AVAssetResourceLoadingRequest finishLoading] to indicate that loading is finished.
--
-- ObjC selector: @- finishLoadingWithResponse:data:redirect:@
finishLoadingWithResponse_data_redirect :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSURLResponse response, IsNSData data_, IsNSURLRequest redirect) => avAssetResourceLoadingRequest -> response -> data_ -> redirect -> IO ()
finishLoadingWithResponse_data_redirect avAssetResourceLoadingRequest  response data_ redirect =
withObjCPtr response $ \raw_response ->
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr redirect $ \raw_redirect ->
        sendMsg avAssetResourceLoadingRequest (mkSelector "finishLoadingWithResponse:data:redirect:") retVoid [argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_redirect :: Ptr ())]

-- | streamingContentKeyRequestDataForApp:contentIdentifier:options:error:
--
-- Obtains a streaming content key request for a specific combination of application and content.
--
-- @appIdentifier@ — An opaque identifier for the application. The value of this identifier depends on the particular system used to provide the decryption key.
--
-- @contentIdentifier@ — An opaque identifier for the content. The value of this identifier depends on the particular system used to provide the decryption key.
--
-- @options@ — Additional information necessary to obtain the key, or nil if none.
--
-- @outError@ — If obtaining the streaming content key request fails, will be set to an instance of NSError describing the failure.
--
-- Returns: The key request data that must be transmitted to the key vendor to obtain the content key.
--
-- ObjC selector: @- streamingContentKeyRequestDataForApp:contentIdentifier:options:error:@
streamingContentKeyRequestDataForApp_contentIdentifier_options_error :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSData appIdentifier, IsNSData contentIdentifier, IsNSDictionary options, IsNSError outError) => avAssetResourceLoadingRequest -> appIdentifier -> contentIdentifier -> options -> outError -> IO (Id NSData)
streamingContentKeyRequestDataForApp_contentIdentifier_options_error avAssetResourceLoadingRequest  appIdentifier contentIdentifier options outError =
withObjCPtr appIdentifier $ \raw_appIdentifier ->
  withObjCPtr contentIdentifier $ \raw_contentIdentifier ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg avAssetResourceLoadingRequest (mkSelector "streamingContentKeyRequestDataForApp:contentIdentifier:options:error:") (retPtr retVoid) [argPtr (castPtr raw_appIdentifier :: Ptr ()), argPtr (castPtr raw_contentIdentifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | persistentContentKeyFromKeyVendorResponse:options:error:
--
-- Obtains a persistable content key from a context.
--
-- @keyVendorResponse@ — The response returned from the key vendor as a result of a request generated from streamingContentKeyRequestDataForApp:contentIdentifier:options:error:.
--
-- @options@ — Additional information necessary to obtain the persistable content key, or nil if none.
--
-- @outError@ — If obtaining the persistable content key fails, will be set to an instance of NSError describing the failure.
--
-- Returns: The persistable content key data that may be stored offline to answer future loading requests of the same content key.
--
-- The data returned from this method may be used to immediately satisfy an AVAssetResourceLoadingDataRequest, as well as any subsequent requests for the same key url. The value of AVAssetResourceLoadingContentInformationRequest.contentType must be set to AVStreamingKeyDeliveryPersistentContentKeyType when responding with data created with this method.
--
-- ObjC selector: @- persistentContentKeyFromKeyVendorResponse:options:error:@
persistentContentKeyFromKeyVendorResponse_options_error :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSData keyVendorResponse, IsNSDictionary options, IsNSError outError) => avAssetResourceLoadingRequest -> keyVendorResponse -> options -> outError -> IO (Id NSData)
persistentContentKeyFromKeyVendorResponse_options_error avAssetResourceLoadingRequest  keyVendorResponse options outError =
withObjCPtr keyVendorResponse $ \raw_keyVendorResponse ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAssetResourceLoadingRequest (mkSelector "persistentContentKeyFromKeyVendorResponse:options:error:") (retPtr retVoid) [argPtr (castPtr raw_keyVendorResponse :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | request
--
-- An NSURLRequest for the requested resource.
--
-- ObjC selector: @- request@
request :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id NSURLRequest)
request avAssetResourceLoadingRequest  =
  sendMsg avAssetResourceLoadingRequest (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | finished
--
-- Indicates whether loading of the resource has been finished.
--
-- The value of this property becomes YES only in response to an invocation of either -finishLoading or -finishLoadingWithError:.
--
-- ObjC selector: @- finished@
finished :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO Bool
finished avAssetResourceLoadingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoadingRequest (mkSelector "finished") retCULong []

-- | cancelled
--
-- Indicates whether the request has been cancelled.
--
-- The value of this property becomes YES when the resource loader cancels the loading of a request, just prior to sending the message -resourceLoader:didCancelLoadingRequest: to its delegate.
--
-- ObjC selector: @- cancelled@
cancelled :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO Bool
cancelled avAssetResourceLoadingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoadingRequest (mkSelector "cancelled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @finishLoading@
finishLoadingSelector :: Selector
finishLoadingSelector = mkSelector "finishLoading"

-- | @Selector@ for @finishLoadingWithError:@
finishLoadingWithErrorSelector :: Selector
finishLoadingWithErrorSelector = mkSelector "finishLoadingWithError:"

-- | @Selector@ for @finishLoadingWithResponse:data:redirect:@
finishLoadingWithResponse_data_redirectSelector :: Selector
finishLoadingWithResponse_data_redirectSelector = mkSelector "finishLoadingWithResponse:data:redirect:"

-- | @Selector@ for @streamingContentKeyRequestDataForApp:contentIdentifier:options:error:@
streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector :: Selector
streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector = mkSelector "streamingContentKeyRequestDataForApp:contentIdentifier:options:error:"

-- | @Selector@ for @persistentContentKeyFromKeyVendorResponse:options:error:@
persistentContentKeyFromKeyVendorResponse_options_errorSelector :: Selector
persistentContentKeyFromKeyVendorResponse_options_errorSelector = mkSelector "persistentContentKeyFromKeyVendorResponse:options:error:"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @finished@
finishedSelector :: Selector
finishedSelector = mkSelector "finished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

