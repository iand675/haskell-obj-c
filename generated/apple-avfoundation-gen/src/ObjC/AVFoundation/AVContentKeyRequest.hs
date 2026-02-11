{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVContentKeyRequest@.
module ObjC.AVFoundation.AVContentKeyRequest
  ( AVContentKeyRequest
  , IsAVContentKeyRequest(..)
  , makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandler
  , processContentKeyResponse
  , processContentKeyResponseError
  , respondByRequestingPersistableContentKeyRequest
  , respondByRequestingPersistableContentKeyRequestAndReturnError
  , status
  , error_
  , identifier
  , initializationData
  , options
  , canProvidePersistableContentKey
  , contentKeySpecifier
  , contentKey
  , originatingRecipient
  , renewsExpiringResponseData
  , makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandlerSelector
  , processContentKeyResponseSelector
  , processContentKeyResponseErrorSelector
  , respondByRequestingPersistableContentKeyRequestSelector
  , respondByRequestingPersistableContentKeyRequestAndReturnErrorSelector
  , statusSelector
  , errorSelector
  , identifierSelector
  , initializationDataSelector
  , optionsSelector
  , canProvidePersistableContentKeySelector
  , contentKeySpecifierSelector
  , contentKeySelector
  , originatingRecipientSelector
  , renewsExpiringResponseDataSelector

  -- * Enum types
  , AVContentKeyRequestStatus(AVContentKeyRequestStatus)
  , pattern AVContentKeyRequestStatusRequestingResponse
  , pattern AVContentKeyRequestStatusReceivedResponse
  , pattern AVContentKeyRequestStatusRenewed
  , pattern AVContentKeyRequestStatusRetried
  , pattern AVContentKeyRequestStatusCancelled
  , pattern AVContentKeyRequestStatusFailed

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Obtains a content key request data for a specific combination of application and content.
--
-- If option AVContentKeyRequestProtocolVersionsKey is not specified the default protocol version of 1 is assumed.
--
-- - Parameter appIdentifier: An opaque identifier for the application. The value of this identifier depends on the particular system used to provide the content key. - Parameter contentIdentifier: An optional opaque identifier for the content. The value of this identifier depends on the particular system used to provide the content key. - Parameter options: Additional information necessary to obtain the key, or nil if none. See AVContentKeyRequest*Key below. - Parameter handler: Once the streaming content key request is prepared, this block will be called with the request data or an error describing the failure.
--
-- ObjC selector: @- makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:@
makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandler :: (IsAVContentKeyRequest avContentKeyRequest, IsNSData appIdentifier, IsNSData contentIdentifier, IsNSDictionary options) => avContentKeyRequest -> appIdentifier -> contentIdentifier -> options -> Ptr () -> IO ()
makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandler avContentKeyRequest  appIdentifier contentIdentifier options handler =
  withObjCPtr appIdentifier $ \raw_appIdentifier ->
    withObjCPtr contentIdentifier $ \raw_contentIdentifier ->
      withObjCPtr options $ \raw_options ->
          sendMsg avContentKeyRequest (mkSelector "makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:") retVoid [argPtr (castPtr raw_appIdentifier :: Ptr ()), argPtr (castPtr raw_contentIdentifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Informs the receiver to process the specified content key response.
--
-- After you receive an AVContentKeyRequest via -contentKeySession:didProvideContentKeyRequest: and after you invoke -[AVContentKeyRequest makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:] on that request, you must obtain a response to the request in accordance with the protocol in use by the entity that controls the use of the media data. This is the method you use to provide the content key response to make protected content available for processing. If obtaining the content key response fails, use -processContentKeyResponseError:.
--
-- - Parameter keyResponse: An instance of AVContentKeyResponse carrying a response to a content key request.
--
-- ObjC selector: @- processContentKeyResponse:@
processContentKeyResponse :: (IsAVContentKeyRequest avContentKeyRequest, IsAVContentKeyResponse keyResponse) => avContentKeyRequest -> keyResponse -> IO ()
processContentKeyResponse avContentKeyRequest  keyResponse =
  withObjCPtr keyResponse $ \raw_keyResponse ->
      sendMsg avContentKeyRequest (mkSelector "processContentKeyResponse:") retVoid [argPtr (castPtr raw_keyResponse :: Ptr ())]

-- | Informs the receiver that obtaining a content key response has failed, resulting in failure handling.
--
-- - Parameter error: An instance of NSError that describes the specific failure that occurred.
--
-- ObjC selector: @- processContentKeyResponseError:@
processContentKeyResponseError :: (IsAVContentKeyRequest avContentKeyRequest, IsNSError error_) => avContentKeyRequest -> error_ -> IO ()
processContentKeyResponseError avContentKeyRequest  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg avContentKeyRequest (mkSelector "processContentKeyResponseError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Informs the receiver to process a persistable content key request.
--
-- When you receive an AVContentKeyRequest via -contentKeySession:didProvideContentKeyRequest: and you want the resulting key response to produce a key that can persist across multiple playback sessions, you must invoke -respondByRequestingPersistableContentKeyRequest on that AVContentKeyRequest in order to signal that you want to process an AVPersistableContentKeyRequest instead. If the underlying protocol supports persistable content keys, in response your delegate will receive an AVPersistableContentKeyRequest via -contentKeySession:didProvidePersistableContentKeyRequest:. NSInternalInconsistencyException will be raised, if you are attempting to create and use a persistable key but your AVContentKeySession delegate does not respond to contentKeySession:didProvidePersistableContentKeyRequest:.
--
-- ObjC selector: @- respondByRequestingPersistableContentKeyRequest@
respondByRequestingPersistableContentKeyRequest :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO ()
respondByRequestingPersistableContentKeyRequest avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "respondByRequestingPersistableContentKeyRequest") retVoid []

-- | Informs the receiver to process a persistable content key request.
--
-- When you receive an AVContentKeyRequest via -contentKeySession:didProvideContentKeyRequest: and you want the resulting key response to produce a key that can persist across multiple playback sessions, you must invoke -respondByRequestingPersistableContentKeyRequest on that AVContentKeyRequest in order to signal that you want to process an AVPersistableContentKeyRequest instead. If the underlying protocol supports persistable content keys, in response your delegate will receive an AVPersistableContentKeyRequest via -contentKeySession:didProvidePersistableContentKeyRequest:. NSInternalInconsistencyException will be raised, if you are attempting to create and use a persistable key but your AVContentKeySession delegate does not respond to contentKeySession:didProvidePersistableContentKeyRequest:.
--
-- - Parameter outError: The error returned if a persistable content key request cannot be requested.
--
-- - Returns: YES if sucessful. If NO, this request should be responded to via processContentKeyResponse: or processContentKeyResponseError:.
--
-- ObjC selector: @- respondByRequestingPersistableContentKeyRequestAndReturnError:@
respondByRequestingPersistableContentKeyRequestAndReturnError :: (IsAVContentKeyRequest avContentKeyRequest, IsNSError outError) => avContentKeyRequest -> outError -> IO Bool
respondByRequestingPersistableContentKeyRequestAndReturnError avContentKeyRequest  outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avContentKeyRequest (mkSelector "respondByRequestingPersistableContentKeyRequestAndReturnError:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | This describes the state of the AVContentKeyRequest, value is one of AVContentKeyRequestStatus.
--
-- ObjC selector: @- status@
status :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO AVContentKeyRequestStatus
status avContentKeyRequest  =
    fmap (coerce :: CLong -> AVContentKeyRequestStatus) $ sendMsg avContentKeyRequest (mkSelector "status") retCLong []

-- | If the receiver's status is AVContentKeyRequestStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the content key request to fail. If the receiver's status is not AVContentKeyRequestStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO (Id NSError)
error_ avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Container- and protocol-specific identifier for the content key.
--
-- In order to use a key with an HTTP Live Streaming AVURLAsset, the identifier must be an NSURL that matches a key URI in the Media Playlist.
--
-- ObjC selector: @- identifier@
identifier :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO RawId
identifier avContentKeyRequest  =
    fmap (RawId . castPtr) $ sendMsg avContentKeyRequest (mkSelector "identifier") (retPtr retVoid) []

-- | @- initializationData@
initializationData :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO (Id NSData)
initializationData avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "initializationData") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Additional information specified while initiaing key loading using -processContentKeyRequestWithIdentifier:initializationData:options:.
--
-- ObjC selector: @- options@
options :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO (Id NSDictionary)
options avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | When the value of this property is YES, you can use the method -persistableContentKeyFromKeyVendorResponse:options:error: to create a persistable content key from the content key response.
--
-- The value of this property will be YES only when the receiver is provided to your AVContentKeySession delegate via the method -contentKeySession:didProvidePersistableContentKeyRequest:. If you have an AVContentKeyRequest for which the value of canProvidePersistableContentKey is NO, but you wish to obtain a persistable content key, send the AVContentKeyRequest the message -respondByRequestingPersistableContentKeyRequest.
--
-- ObjC selector: @- canProvidePersistableContentKey@
canProvidePersistableContentKey :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO Bool
canProvidePersistableContentKey avContentKeyRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avContentKeyRequest (mkSelector "canProvidePersistableContentKey") retCULong []

-- | Specifies the requested content key.
--
-- ObjC selector: @- contentKeySpecifier@
contentKeySpecifier :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO (Id AVContentKeySpecifier)
contentKeySpecifier avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "contentKeySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents an AVContentKey that results from an invocation of -processContentKeyResponse:.
--
-- Before the receiver achieves the status AVContentKeyRequestReceivedResponse, the value of this property will be nil. Once that status has been achieved, the value of this property becomes a non-nil AVContentKey that can be provided to content key recipients that apply content keys manually to objects that require them, such as CMSampleBuffers, or to initiate renewal. A non-nil value does not indicate that the content key is valid; authorization failures may yet be possible.
--
-- ObjC selector: @- contentKey@
contentKey :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO (Id AVContentKey)
contentKey avContentKeyRequest  =
    sendMsg avContentKeyRequest (mkSelector "contentKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The AVContentKeyRecipient which initiated this request, if any.
--
-- The originatingRecipient is an AVFoundation object responsible for initiating an AVContentKeyRequest. For example, an AVURLAsset used for playback can trigger an AVContentKeyRequest.
--
-- If an application triggers key loading directly, for example with -[AVContentKeySession processContentKeyRequestWithIdentifier:initializationData:options:], the value of originatingRecipient will be nil.
--
-- The originatingRecipient of key requests from HLS interstitials will always be the corresponding interstitial AVURLAsset. To receive key requests for DRM-protected interstitial content, applications must ensure their AVContentKeySession is attached to these interstitial AVURLAssets.
--
-- These interstitial AVURLAssets may be retrieved from the primary AVURLAsset via AVPlayerInterstitialEventMonitor.
--
-- ObjC selector: @- originatingRecipient@
originatingRecipient :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO RawId
originatingRecipient avContentKeyRequest  =
    fmap (RawId . castPtr) $ sendMsg avContentKeyRequest (mkSelector "originatingRecipient") (retPtr retVoid) []

-- | Indicates whether the receiver represents a request to renew previously provided response data that is expiring or has expired.
--
-- ObjC selector: @- renewsExpiringResponseData@
renewsExpiringResponseData :: IsAVContentKeyRequest avContentKeyRequest => avContentKeyRequest -> IO Bool
renewsExpiringResponseData avContentKeyRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avContentKeyRequest (mkSelector "renewsExpiringResponseData") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:@
makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandlerSelector :: Selector
makeStreamingContentKeyRequestDataForApp_contentIdentifier_options_completionHandlerSelector = mkSelector "makeStreamingContentKeyRequestDataForApp:contentIdentifier:options:completionHandler:"

-- | @Selector@ for @processContentKeyResponse:@
processContentKeyResponseSelector :: Selector
processContentKeyResponseSelector = mkSelector "processContentKeyResponse:"

-- | @Selector@ for @processContentKeyResponseError:@
processContentKeyResponseErrorSelector :: Selector
processContentKeyResponseErrorSelector = mkSelector "processContentKeyResponseError:"

-- | @Selector@ for @respondByRequestingPersistableContentKeyRequest@
respondByRequestingPersistableContentKeyRequestSelector :: Selector
respondByRequestingPersistableContentKeyRequestSelector = mkSelector "respondByRequestingPersistableContentKeyRequest"

-- | @Selector@ for @respondByRequestingPersistableContentKeyRequestAndReturnError:@
respondByRequestingPersistableContentKeyRequestAndReturnErrorSelector :: Selector
respondByRequestingPersistableContentKeyRequestAndReturnErrorSelector = mkSelector "respondByRequestingPersistableContentKeyRequestAndReturnError:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @initializationData@
initializationDataSelector :: Selector
initializationDataSelector = mkSelector "initializationData"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @canProvidePersistableContentKey@
canProvidePersistableContentKeySelector :: Selector
canProvidePersistableContentKeySelector = mkSelector "canProvidePersistableContentKey"

-- | @Selector@ for @contentKeySpecifier@
contentKeySpecifierSelector :: Selector
contentKeySpecifierSelector = mkSelector "contentKeySpecifier"

-- | @Selector@ for @contentKey@
contentKeySelector :: Selector
contentKeySelector = mkSelector "contentKey"

-- | @Selector@ for @originatingRecipient@
originatingRecipientSelector :: Selector
originatingRecipientSelector = mkSelector "originatingRecipient"

-- | @Selector@ for @renewsExpiringResponseData@
renewsExpiringResponseDataSelector :: Selector
renewsExpiringResponseDataSelector = mkSelector "renewsExpiringResponseData"

