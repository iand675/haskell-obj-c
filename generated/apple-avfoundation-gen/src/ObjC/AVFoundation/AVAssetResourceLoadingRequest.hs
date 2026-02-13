{-# LANGUAGE DataKinds #-}
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
  , contentInformationRequest
  , dataRequest
  , response
  , setResponse
  , redirect
  , setRedirect
  , requestor
  , cancelledSelector
  , contentInformationRequestSelector
  , dataRequestSelector
  , finishLoadingSelector
  , finishLoadingWithErrorSelector
  , finishLoadingWithResponse_data_redirectSelector
  , finishedSelector
  , initSelector
  , newSelector
  , persistentContentKeyFromKeyVendorResponse_options_errorSelector
  , redirectSelector
  , requestSelector
  , requestorSelector
  , responseSelector
  , setRedirectSelector
  , setResponseSelector
  , streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id AVAssetResourceLoadingRequest)
init_ avAssetResourceLoadingRequest =
  sendOwnedMessage avAssetResourceLoadingRequest initSelector

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingRequest)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingRequest"
    sendOwnedClassMessage cls' newSelector

-- | finishLoading
--
-- Causes the receiver to treat the processing of the request as complete.
--
-- If a dataRequest is present, and the resource does not contain the full extent of the data that has been requested according to the values of the requestedOffset and requestedLength properties of the dataRequest, or if requestsAllDataToEndOfResource has a value of YES, -finishLoading may be invoked after providing as much of the requested data as the resource contains. If the contentInformationRequest property is not nil and specifies a non-empty allowedContentTypes array, the contentInformationRequest's contentType property must be set to a value within allowedContentTypes. Otherwise, this method will throw an exception.
--
-- ObjC selector: @- finishLoading@
finishLoading :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO ()
finishLoading avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest finishLoadingSelector

-- | finishLoadingWithError:
--
-- Causes the receiver to treat the request as having failed.
--
-- @error@ — An instance of NSError indicating the reason for failure.
--
-- ObjC selector: @- finishLoadingWithError:@
finishLoadingWithError :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSError error_) => avAssetResourceLoadingRequest -> error_ -> IO ()
finishLoadingWithError avAssetResourceLoadingRequest error_ =
  sendMessage avAssetResourceLoadingRequest finishLoadingWithErrorSelector (toNSError error_)

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
finishLoadingWithResponse_data_redirect avAssetResourceLoadingRequest response data_ redirect =
  sendMessage avAssetResourceLoadingRequest finishLoadingWithResponse_data_redirectSelector (toNSURLResponse response) (toNSData data_) (toNSURLRequest redirect)

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
streamingContentKeyRequestDataForApp_contentIdentifier_options_error avAssetResourceLoadingRequest appIdentifier contentIdentifier options outError =
  sendMessage avAssetResourceLoadingRequest streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector (toNSData appIdentifier) (toNSData contentIdentifier) (toNSDictionary options) (toNSError outError)

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
persistentContentKeyFromKeyVendorResponse_options_error avAssetResourceLoadingRequest keyVendorResponse options outError =
  sendMessage avAssetResourceLoadingRequest persistentContentKeyFromKeyVendorResponse_options_errorSelector (toNSData keyVendorResponse) (toNSDictionary options) (toNSError outError)

-- | request
--
-- An NSURLRequest for the requested resource.
--
-- ObjC selector: @- request@
request :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id NSURLRequest)
request avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest requestSelector

-- | finished
--
-- Indicates whether loading of the resource has been finished.
--
-- The value of this property becomes YES only in response to an invocation of either -finishLoading or -finishLoadingWithError:.
--
-- ObjC selector: @- finished@
finished :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO Bool
finished avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest finishedSelector

-- | cancelled
--
-- Indicates whether the request has been cancelled.
--
-- The value of this property becomes YES when the resource loader cancels the loading of a request, just prior to sending the message -resourceLoader:didCancelLoadingRequest: to its delegate.
--
-- ObjC selector: @- cancelled@
cancelled :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO Bool
cancelled avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest cancelledSelector

-- | contentInformationRequest
--
-- An instance of AVAssetResourceLoadingContentInformationRequest that you must populate with information about the resource before responding to any AVAssetResourceLoadingDataRequests for the resource.  The value of this property will be nil if no such information is being requested.
--
-- ObjC selector: @- contentInformationRequest@
contentInformationRequest :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id AVAssetResourceLoadingContentInformationRequest)
contentInformationRequest avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest contentInformationRequestSelector

-- | dataRequest
--
-- An instance of AVAssetResourceLoadingDataRequest that indicates the range of resource data that's being requested.  If an AVAssetResourceLoadingContentInformationRequest has been provided, you must set its properties appropriately before responding to any AVAssetResourceLoadingDataRequests.  The value of this property will be nil if no data is being requested.
--
-- ObjC selector: @- dataRequest@
dataRequest :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id AVAssetResourceLoadingDataRequest)
dataRequest avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest dataRequestSelector

-- | response
--
-- Set the value of this property to an instance of NSURLResponse indicating a response to the loading request. If no response is needed, leave the value of this property set to nil.
--
-- ObjC selector: @- response@
response :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id NSURLResponse)
response avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest responseSelector

-- | response
--
-- Set the value of this property to an instance of NSURLResponse indicating a response to the loading request. If no response is needed, leave the value of this property set to nil.
--
-- ObjC selector: @- setResponse:@
setResponse :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSURLResponse value) => avAssetResourceLoadingRequest -> value -> IO ()
setResponse avAssetResourceLoadingRequest value =
  sendMessage avAssetResourceLoadingRequest setResponseSelector (toNSURLResponse value)

-- | redirect
--
-- Set the value of this property to an instance of NSURLRequest indicating a redirection of the loading request to another URL. If no redirection is needed, leave the value of this property set to nil.
--
-- AVAssetResourceLoader supports redirects to HTTP URLs only. Redirects to other URLs will result in a loading failure.
--
-- ObjC selector: @- redirect@
redirect :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id NSURLRequest)
redirect avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest redirectSelector

-- | redirect
--
-- Set the value of this property to an instance of NSURLRequest indicating a redirection of the loading request to another URL. If no redirection is needed, leave the value of this property set to nil.
--
-- AVAssetResourceLoader supports redirects to HTTP URLs only. Redirects to other URLs will result in a loading failure.
--
-- ObjC selector: @- setRedirect:@
setRedirect :: (IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest, IsNSURLRequest value) => avAssetResourceLoadingRequest -> value -> IO ()
setRedirect avAssetResourceLoadingRequest value =
  sendMessage avAssetResourceLoadingRequest setRedirectSelector (toNSURLRequest value)

-- | requestor
--
-- The AVAssetResourceLoadingRequestor that made this request
--
-- ObjC selector: @- requestor@
requestor :: IsAVAssetResourceLoadingRequest avAssetResourceLoadingRequest => avAssetResourceLoadingRequest -> IO (Id AVAssetResourceLoadingRequestor)
requestor avAssetResourceLoadingRequest =
  sendMessage avAssetResourceLoadingRequest requestorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetResourceLoadingRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetResourceLoadingRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @finishLoading@
finishLoadingSelector :: Selector '[] ()
finishLoadingSelector = mkSelector "finishLoading"

-- | @Selector@ for @finishLoadingWithError:@
finishLoadingWithErrorSelector :: Selector '[Id NSError] ()
finishLoadingWithErrorSelector = mkSelector "finishLoadingWithError:"

-- | @Selector@ for @finishLoadingWithResponse:data:redirect:@
finishLoadingWithResponse_data_redirectSelector :: Selector '[Id NSURLResponse, Id NSData, Id NSURLRequest] ()
finishLoadingWithResponse_data_redirectSelector = mkSelector "finishLoadingWithResponse:data:redirect:"

-- | @Selector@ for @streamingContentKeyRequestDataForApp:contentIdentifier:options:error:@
streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector :: Selector '[Id NSData, Id NSData, Id NSDictionary, Id NSError] (Id NSData)
streamingContentKeyRequestDataForApp_contentIdentifier_options_errorSelector = mkSelector "streamingContentKeyRequestDataForApp:contentIdentifier:options:error:"

-- | @Selector@ for @persistentContentKeyFromKeyVendorResponse:options:error:@
persistentContentKeyFromKeyVendorResponse_options_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] (Id NSData)
persistentContentKeyFromKeyVendorResponse_options_errorSelector = mkSelector "persistentContentKeyFromKeyVendorResponse:options:error:"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id NSURLRequest)
requestSelector = mkSelector "request"

-- | @Selector@ for @finished@
finishedSelector :: Selector '[] Bool
finishedSelector = mkSelector "finished"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @contentInformationRequest@
contentInformationRequestSelector :: Selector '[] (Id AVAssetResourceLoadingContentInformationRequest)
contentInformationRequestSelector = mkSelector "contentInformationRequest"

-- | @Selector@ for @dataRequest@
dataRequestSelector :: Selector '[] (Id AVAssetResourceLoadingDataRequest)
dataRequestSelector = mkSelector "dataRequest"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSURLResponse)
responseSelector = mkSelector "response"

-- | @Selector@ for @setResponse:@
setResponseSelector :: Selector '[Id NSURLResponse] ()
setResponseSelector = mkSelector "setResponse:"

-- | @Selector@ for @redirect@
redirectSelector :: Selector '[] (Id NSURLRequest)
redirectSelector = mkSelector "redirect"

-- | @Selector@ for @setRedirect:@
setRedirectSelector :: Selector '[Id NSURLRequest] ()
setRedirectSelector = mkSelector "setRedirect:"

-- | @Selector@ for @requestor@
requestorSelector :: Selector '[] (Id AVAssetResourceLoadingRequestor)
requestorSelector = mkSelector "requestor"

