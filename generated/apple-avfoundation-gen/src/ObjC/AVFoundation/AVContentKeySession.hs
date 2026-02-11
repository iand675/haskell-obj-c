{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVContentKeySession is used to create and track decryption keys for media data. Objects conforming to the AVContentKeyRecipient protocol, such as AVURLAssets, can be added to an AVContentKeySession to employ the services of the AVContentKeySession in handling new key requests and to obtain access to the session's already existing keys.
--
-- Its secondary purpose is to provide a report of expired sessions to assist a controlling entity that wishes to track the set of sessions that are still active. If initialized with a location at which to store them, AVContentKeySession maintains a global collection of pending "expired session reports", each associated with an identifier for the app that created the session. The contents of this identifier are specified by the controlling entity that provides media data or that grants permission for its use.
--
-- Expired sessions are tracked as follows: a stream processing session is considered to be started after an instance of AVContentKeySession is created and the first object conforming to the AVContentKeyRecipient protocol is added to it. If an instance of AVContentKeySession that has reached this state does not receive an expire message before it's deallocated or the process in which it's running is terminated, an "expired session report" will subsequently be added to the pending list of expired session reports that indicates that the session expired abnormally. In contrast, for AVContentKeySessions that reach the state of having at least one object conforming to the AVContentKeyRecipient protocol added to them and later receive an expire message, "expired session reports" will be generated that indicate that the session expired normally.
--
-- To obtain the collection of pending expired session reports in order to provide them to the controlling entity associated with a specific app identifier, use +pendingExpiredSessionReportsWithAppIdentifier:.
--
-- After pending expired session reports have been sent to the controlling entity and their receipt has been acknowledged, they can be removed from the collection of pending expired session reports maintained by AVContentKeySession by using +removePendingExpiredSessionReports:withAppIdentifier:.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVContentKeySession@.
module ObjC.AVFoundation.AVContentKeySession
  ( AVContentKeySession
  , IsAVContentKeySession(..)
  , init_
  , new
  , contentKeySessionWithKeySystem
  , contentKeySessionWithKeySystem_storageDirectoryAtURL
  , setDelegate_queue
  , expire
  , processContentKeyRequestWithIdentifier_initializationData_options
  , renewExpiringResponseDataForContentKeyRequest
  , makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandler
  , invalidatePersistableContentKey_options_completionHandler
  , invalidateAllPersistableContentKeysForApp_options_completionHandler
  , pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURL
  , removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURL
  , addContentKeyRecipient
  , removeContentKeyRecipient
  , delegate
  , delegateQueue
  , storageURL
  , keySystem
  , contentProtectionSessionIdentifier
  , contentKeyRecipients
  , initSelector
  , newSelector
  , contentKeySessionWithKeySystemSelector
  , contentKeySessionWithKeySystem_storageDirectoryAtURLSelector
  , setDelegate_queueSelector
  , expireSelector
  , processContentKeyRequestWithIdentifier_initializationData_optionsSelector
  , renewExpiringResponseDataForContentKeyRequestSelector
  , makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandlerSelector
  , invalidatePersistableContentKey_options_completionHandlerSelector
  , invalidateAllPersistableContentKeysForApp_options_completionHandlerSelector
  , pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURLSelector
  , removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURLSelector
  , addContentKeyRecipientSelector
  , removeContentKeyRecipientSelector
  , delegateSelector
  , delegateQueueSelector
  , storageURLSelector
  , keySystemSelector
  , contentProtectionSessionIdentifierSelector
  , contentKeyRecipientsSelector


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
init_ :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id AVContentKeySession)
init_ avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVContentKeySession)
new  =
  do
    cls' <- getRequiredClass "AVContentKeySession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new instance of AVContentKeySession to manage a collection of media content keys.
--
-- This method returns an AVContentKeySession instance that is capable of managing collection of media content keys corresponding to the input keySystem. An NSInvalidArgumentException will be raised if the value of keySystem is unsupported.
--
-- - Parameter keySystem: A valid key system for retrieving keys.
--
-- - Returns: A new AVContentKeySession.
--
-- ObjC selector: @+ contentKeySessionWithKeySystem:@
contentKeySessionWithKeySystem :: IsNSString keySystem => keySystem -> IO (Id AVContentKeySession)
contentKeySessionWithKeySystem keySystem =
  do
    cls' <- getRequiredClass "AVContentKeySession"
    withObjCPtr keySystem $ \raw_keySystem ->
      sendClassMsg cls' (mkSelector "contentKeySessionWithKeySystem:") (retPtr retVoid) [argPtr (castPtr raw_keySystem :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new instance of AVContentKeySession to manage a collection of media content keys.
--
-- This method returns an AVContentKeySession instance that is capable of managing collection of media content keys corresponding to the input keySystem. An NSInvalidArgumentException will be raised if the value of keySystem is unsupported.
--
-- - Parameter keySystem: A valid key system for retrieving keys. - Parameter storageURL: URL to a writable directory that the session will use to facilitate expired session reports after abnormal session termination.
--
-- - Returns: A new AVContentKeySession.
--
-- ObjC selector: @+ contentKeySessionWithKeySystem:storageDirectoryAtURL:@
contentKeySessionWithKeySystem_storageDirectoryAtURL :: (IsNSString keySystem, IsNSURL storageURL) => keySystem -> storageURL -> IO (Id AVContentKeySession)
contentKeySessionWithKeySystem_storageDirectoryAtURL keySystem storageURL =
  do
    cls' <- getRequiredClass "AVContentKeySession"
    withObjCPtr keySystem $ \raw_keySystem ->
      withObjCPtr storageURL $ \raw_storageURL ->
        sendClassMsg cls' (mkSelector "contentKeySessionWithKeySystem:storageDirectoryAtURL:") (retPtr retVoid) [argPtr (castPtr raw_keySystem :: Ptr ()), argPtr (castPtr raw_storageURL :: Ptr ())] >>= retainedObject . castPtr

-- | Sets the receiver's delegate. A delegate is required to handle content key initialization.
--
-- - Parameter delegate: An object conforming to the AVContentKeySessionDelegate protocol. - Parameter delegateQueue: A dispatch queue on which delegate methods will be invoked whenever processes requiring content keys are executed asynchronously. Passing a value of nil for the delegateQueue parameter along with a non-nil value for the delegate parameter will result in an invalid argument exception.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVContentKeySession avContentKeySession, IsNSObject delegateQueue) => avContentKeySession -> RawId -> delegateQueue -> IO ()
setDelegate_queue avContentKeySession  delegate delegateQueue =
  withObjCPtr delegateQueue $ \raw_delegateQueue ->
      sendMsg avContentKeySession (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

-- | Tells the receiver to treat the session as having been intentionally and normally expired.
--
-- When an instance of AVContentKeySession receives an expire message, all of its associated objects conforming to the AVContentKeyRecipient protocol will become inoperable. Send this message only after you have finished operating on the media data.
--
-- ObjC selector: @- expire@
expire :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO ()
expire avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "expire") retVoid []

-- | Informs the receiver that it should attempt to instantiate a content decryption key using the specified initialization data.
--
-- May be used to generate an AVContentKeyRequest from request initialization data already in hand, without awaiting such data during the processing of media data of an associated recipient.
--
-- - Parameter identifier: Container- and protocol-specific identifier to be used to obtain a key response. Either identifier or initializationData must be non-nil. Both can be non-nil, if the content protection protocol requires both. - Parameter initializationData: Container- and protocol-specific data to be used to obtain a key response. Either identifier or initializationData must be non-nil. Both can be non-nil, if the content protection protocol requires both. - Parameter options: Additional information necessary to obtain the key, or nil if none. See AVContentKeyRequest*Key below.
--
-- ObjC selector: @- processContentKeyRequestWithIdentifier:initializationData:options:@
processContentKeyRequestWithIdentifier_initializationData_options :: (IsAVContentKeySession avContentKeySession, IsNSData initializationData, IsNSDictionary options) => avContentKeySession -> RawId -> initializationData -> options -> IO ()
processContentKeyRequestWithIdentifier_initializationData_options avContentKeySession  identifier initializationData options =
  withObjCPtr initializationData $ \raw_initializationData ->
    withObjCPtr options $ \raw_options ->
        sendMsg avContentKeySession (mkSelector "processContentKeyRequestWithIdentifier:initializationData:options:") retVoid [argPtr (castPtr (unRawId identifier) :: Ptr ()), argPtr (castPtr raw_initializationData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | Informs the receiver that the already provided response data for an earlier AVContentKeyRequest will imminently expire.
--
-- In response the receiver will invoke your delegate with a new content key request entreating it to renew the expiring response data, via -contentKeySession:didProvideRenewingContentKeyRequest:.
--
-- ObjC selector: @- renewExpiringResponseDataForContentKeyRequest:@
renewExpiringResponseDataForContentKeyRequest :: (IsAVContentKeySession avContentKeySession, IsAVContentKeyRequest contentKeyRequest) => avContentKeySession -> contentKeyRequest -> IO ()
renewExpiringResponseDataForContentKeyRequest avContentKeySession  contentKeyRequest =
  withObjCPtr contentKeyRequest $ \raw_contentKeyRequest ->
      sendMsg avContentKeySession (mkSelector "renewExpiringResponseDataForContentKeyRequest:") retVoid [argPtr (castPtr raw_contentKeyRequest :: Ptr ())]

-- | Creates a secure server playback context (SPC) that the client could send to the key server to obtain an expiration date for the provided persistable content key data.
--
-- - Parameter persistableContentKeyData: Persistable content key data that was previously created using -[AVContentKeyRequest persistableContentKeyFromKeyVendorResponse:options:error:] or obtained via AVContentKeySessionDelegate callback -contentKeySession:didUpdatePersistableContentKey:forContentKeyIdentifier:. - Parameter handler: Once the secure token is ready, this block will be called with the token or an error describing the failure.
--
-- ObjC selector: @- makeSecureTokenForExpirationDateOfPersistableContentKey:completionHandler:@
makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandler :: (IsAVContentKeySession avContentKeySession, IsNSData persistableContentKeyData) => avContentKeySession -> persistableContentKeyData -> Ptr () -> IO ()
makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandler avContentKeySession  persistableContentKeyData handler =
  withObjCPtr persistableContentKeyData $ \raw_persistableContentKeyData ->
      sendMsg avContentKeySession (mkSelector "makeSecureTokenForExpirationDateOfPersistableContentKey:completionHandler:") retVoid [argPtr (castPtr raw_persistableContentKeyData :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Invalidates the persistable content key and creates a secure server playback context (SPC) that the client could send to the key server to verify the outcome of invalidation request.
--
-- Once invalidated, a persistable content key cannot be used to answer key requests during later playback sessions.
--
-- - Parameter persistableContentKeyData: Persistable content key data that was previously created using -[AVContentKeyRequest persistableContentKeyFromKeyVendorResponse:options:error:] or obtained via AVContentKeySessionDelegate callback -contentKeySession:didUpdatePersistableContentKey:forContentKeyIdentifier:. - Parameter options: Additional information necessary to generate the server playback context, or nil if none. See AVContentKeySessionServerPlaybackContextOption for supported options. - Parameter handler: Once the server playback context is ready, this block will be called with the data or an error describing the failure.
--
-- ObjC selector: @- invalidatePersistableContentKey:options:completionHandler:@
invalidatePersistableContentKey_options_completionHandler :: (IsAVContentKeySession avContentKeySession, IsNSData persistableContentKeyData, IsNSDictionary options) => avContentKeySession -> persistableContentKeyData -> options -> Ptr () -> IO ()
invalidatePersistableContentKey_options_completionHandler avContentKeySession  persistableContentKeyData options handler =
  withObjCPtr persistableContentKeyData $ \raw_persistableContentKeyData ->
    withObjCPtr options $ \raw_options ->
        sendMsg avContentKeySession (mkSelector "invalidatePersistableContentKey:options:completionHandler:") retVoid [argPtr (castPtr raw_persistableContentKeyData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Invalidates all persistable content keys associated with the application and creates a secure server playback context (SPC) that the client could send to the key server to verify the outcome of invalidation request.
--
-- Once invalidated, persistable content keys cannot be used to answer key requests during later playback sessions.
--
-- - Parameter appIdentifier: An opaque identifier for the application. The contents of this identifier depend on the particular protocol in use by the entity that controls the use of the media data. - Parameter options: Additional information necessary to generate the server playback context, or nil if none. See AVContentKeySessionServerPlaybackContextOption for supported options. - Parameter handler: Once the server playback context is ready, this block will be called with the data or an error describing the failure.
--
-- ObjC selector: @- invalidateAllPersistableContentKeysForApp:options:completionHandler:@
invalidateAllPersistableContentKeysForApp_options_completionHandler :: (IsAVContentKeySession avContentKeySession, IsNSData appIdentifier, IsNSDictionary options) => avContentKeySession -> appIdentifier -> options -> Ptr () -> IO ()
invalidateAllPersistableContentKeysForApp_options_completionHandler avContentKeySession  appIdentifier options handler =
  withObjCPtr appIdentifier $ \raw_appIdentifier ->
    withObjCPtr options $ \raw_options ->
        sendMsg avContentKeySession (mkSelector "invalidateAllPersistableContentKeysForApp:options:completionHandler:") retVoid [argPtr (castPtr raw_appIdentifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Provides "expired session reports" for prior AVContentKeySessions created with the specified app identifier that have expired either normally or abnormally.
--
-- Note that no reports for sessions still in progress will be included.
--
-- - Parameter appIdentifier: An opaque identifier for the application. The contents of this identifier depend on the particular protocol in use by the entity that controls the use of the media data. - Parameter storageURL: URL to a directory previously used with one or more instances of AVContentKeySession for the storage of expired session reports.
--
-- - Returns: An NSArray containing instances of NSData, each containing a pending expired session report as a property-list serialization of an NSDictionary object. The contents of expired session reports depend on the particular protocol in use by the entity that controls the use of the media data.
--
-- ObjC selector: @+ pendingExpiredSessionReportsWithAppIdentifier:storageDirectoryAtURL:@
pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURL :: (IsNSData appIdentifier, IsNSURL storageURL) => appIdentifier -> storageURL -> IO (Id NSArray)
pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURL appIdentifier storageURL =
  do
    cls' <- getRequiredClass "AVContentKeySession"
    withObjCPtr appIdentifier $ \raw_appIdentifier ->
      withObjCPtr storageURL $ \raw_storageURL ->
        sendClassMsg cls' (mkSelector "pendingExpiredSessionReportsWithAppIdentifier:storageDirectoryAtURL:") (retPtr retVoid) [argPtr (castPtr raw_appIdentifier :: Ptr ()), argPtr (castPtr raw_storageURL :: Ptr ())] >>= retainedObject . castPtr

-- | Removes expired session reports for prior AVContentKeySessions from storage. Once they have been removed, they will no longer be available via subsequent invocations of +pendingExpiredSessionReportsWithAppIdentifier:.
--
-- This method is most suitable for use only after the specified expired session reports have been sent to the entity that controls the use of the media data and the entity has acknowledged their receipt.
--
-- - Parameter expiredSessionReports: An array of expired session reports to be discarded. - Parameter appIdentifier: An opaque identifier for the application. The contents of this identifier depend on the particular protocol in use by the entity that controls the use of the media data. - Parameter storageURL: URL to a writable folder.
--
-- ObjC selector: @+ removePendingExpiredSessionReports:withAppIdentifier:storageDirectoryAtURL:@
removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURL :: (IsNSArray expiredSessionReports, IsNSData appIdentifier, IsNSURL storageURL) => expiredSessionReports -> appIdentifier -> storageURL -> IO ()
removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURL expiredSessionReports appIdentifier storageURL =
  do
    cls' <- getRequiredClass "AVContentKeySession"
    withObjCPtr expiredSessionReports $ \raw_expiredSessionReports ->
      withObjCPtr appIdentifier $ \raw_appIdentifier ->
        withObjCPtr storageURL $ \raw_storageURL ->
          sendClassMsg cls' (mkSelector "removePendingExpiredSessionReports:withAppIdentifier:storageDirectoryAtURL:") retVoid [argPtr (castPtr raw_expiredSessionReports :: Ptr ()), argPtr (castPtr raw_appIdentifier :: Ptr ()), argPtr (castPtr raw_storageURL :: Ptr ())]

-- | Informs the receiver that the specified recipient will be used for the session.
--
-- It is an error to add recipient to sessions that have received an expire message. It is also an error to add recipients after they have already begun to process media data (e.g. after an AVURLAsset has loaded the values of any of its keys). Such errors will result in NSInternalInconsistencyExceptions. Sending this message to an AVContentKeySession is atomic.
--
-- ObjC selector: @- addContentKeyRecipient:@
addContentKeyRecipient :: IsAVContentKeySession avContentKeySession => avContentKeySession -> RawId -> IO ()
addContentKeyRecipient avContentKeySession  recipient =
    sendMsg avContentKeySession (mkSelector "addContentKeyRecipient:") retVoid [argPtr (castPtr (unRawId recipient) :: Ptr ())]

-- | Informs the receiver that the specified recipient will no longer be used.
--
-- After the specified recipient is removed from the receiver it will become inoperable. Remove the recipient only after you have finished operating on the media data associated with it. Sending this message to an AVContentKeySession is atomic.
--
-- ObjC selector: @- removeContentKeyRecipient:@
removeContentKeyRecipient :: IsAVContentKeySession avContentKeySession => avContentKeySession -> RawId -> IO ()
removeContentKeyRecipient avContentKeySession  recipient =
    sendMsg avContentKeySession (mkSelector "removeContentKeyRecipient:") retVoid [argPtr (castPtr (unRawId recipient) :: Ptr ())]

-- | The receiver's delegate.
--
-- The value of this property is an object conforming to the AVContentKeySessionDelegate protocol. The delegate is set using the setDelegate:queue: method.
--
-- ObjC selector: @- delegate@
delegate :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO RawId
delegate avContentKeySession  =
    fmap (RawId . castPtr) $ sendMsg avContentKeySession (mkSelector "delegate") (retPtr retVoid) []

-- | The dispatch queue on which all delegate methods will be invoked whenever processes requiring content keys are executed asynchronously.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setDelegate:queue: method.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id NSObject)
delegateQueue avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The storage URL provided when the AVContentKeySession was created. May be nil.
--
-- URL to a writable directory; may be nil. The session will use this to facilitate expired session reports after abnormal session termination.
--
-- ObjC selector: @- storageURL@
storageURL :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id NSURL)
storageURL avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "storageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key system used for retrieving keys
--
-- ObjC selector: @- keySystem@
keySystem :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id NSString)
keySystem avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "keySystem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An opaque identifier for the current content protection session.
--
-- May be nil. Will call the delegate's contentKeySessionContentProtectionSessionIdentifierDidChange: when the identifier changes. The protection session ID is a unique string identifier generated by the AVContentKeySession that can be used by the application to identify content key session objects.
--
-- ObjC selector: @- contentProtectionSessionIdentifier@
contentProtectionSessionIdentifier :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id NSData)
contentProtectionSessionIdentifier avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "contentProtectionSessionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The array of recipients of content keys currently associated with the AVContentKeySession.
--
-- ObjC selector: @- contentKeyRecipients@
contentKeyRecipients :: IsAVContentKeySession avContentKeySession => avContentKeySession -> IO (Id NSArray)
contentKeyRecipients avContentKeySession  =
    sendMsg avContentKeySession (mkSelector "contentKeyRecipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentKeySessionWithKeySystem:@
contentKeySessionWithKeySystemSelector :: Selector
contentKeySessionWithKeySystemSelector = mkSelector "contentKeySessionWithKeySystem:"

-- | @Selector@ for @contentKeySessionWithKeySystem:storageDirectoryAtURL:@
contentKeySessionWithKeySystem_storageDirectoryAtURLSelector :: Selector
contentKeySessionWithKeySystem_storageDirectoryAtURLSelector = mkSelector "contentKeySessionWithKeySystem:storageDirectoryAtURL:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @expire@
expireSelector :: Selector
expireSelector = mkSelector "expire"

-- | @Selector@ for @processContentKeyRequestWithIdentifier:initializationData:options:@
processContentKeyRequestWithIdentifier_initializationData_optionsSelector :: Selector
processContentKeyRequestWithIdentifier_initializationData_optionsSelector = mkSelector "processContentKeyRequestWithIdentifier:initializationData:options:"

-- | @Selector@ for @renewExpiringResponseDataForContentKeyRequest:@
renewExpiringResponseDataForContentKeyRequestSelector :: Selector
renewExpiringResponseDataForContentKeyRequestSelector = mkSelector "renewExpiringResponseDataForContentKeyRequest:"

-- | @Selector@ for @makeSecureTokenForExpirationDateOfPersistableContentKey:completionHandler:@
makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandlerSelector :: Selector
makeSecureTokenForExpirationDateOfPersistableContentKey_completionHandlerSelector = mkSelector "makeSecureTokenForExpirationDateOfPersistableContentKey:completionHandler:"

-- | @Selector@ for @invalidatePersistableContentKey:options:completionHandler:@
invalidatePersistableContentKey_options_completionHandlerSelector :: Selector
invalidatePersistableContentKey_options_completionHandlerSelector = mkSelector "invalidatePersistableContentKey:options:completionHandler:"

-- | @Selector@ for @invalidateAllPersistableContentKeysForApp:options:completionHandler:@
invalidateAllPersistableContentKeysForApp_options_completionHandlerSelector :: Selector
invalidateAllPersistableContentKeysForApp_options_completionHandlerSelector = mkSelector "invalidateAllPersistableContentKeysForApp:options:completionHandler:"

-- | @Selector@ for @pendingExpiredSessionReportsWithAppIdentifier:storageDirectoryAtURL:@
pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURLSelector :: Selector
pendingExpiredSessionReportsWithAppIdentifier_storageDirectoryAtURLSelector = mkSelector "pendingExpiredSessionReportsWithAppIdentifier:storageDirectoryAtURL:"

-- | @Selector@ for @removePendingExpiredSessionReports:withAppIdentifier:storageDirectoryAtURL:@
removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURLSelector :: Selector
removePendingExpiredSessionReports_withAppIdentifier_storageDirectoryAtURLSelector = mkSelector "removePendingExpiredSessionReports:withAppIdentifier:storageDirectoryAtURL:"

-- | @Selector@ for @addContentKeyRecipient:@
addContentKeyRecipientSelector :: Selector
addContentKeyRecipientSelector = mkSelector "addContentKeyRecipient:"

-- | @Selector@ for @removeContentKeyRecipient:@
removeContentKeyRecipientSelector :: Selector
removeContentKeyRecipientSelector = mkSelector "removeContentKeyRecipient:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @storageURL@
storageURLSelector :: Selector
storageURLSelector = mkSelector "storageURL"

-- | @Selector@ for @keySystem@
keySystemSelector :: Selector
keySystemSelector = mkSelector "keySystem"

-- | @Selector@ for @contentProtectionSessionIdentifier@
contentProtectionSessionIdentifierSelector :: Selector
contentProtectionSessionIdentifierSelector = mkSelector "contentProtectionSessionIdentifier"

-- | @Selector@ for @contentKeyRecipients@
contentKeyRecipientsSelector :: Selector
contentKeyRecipientsSelector = mkSelector "contentKeyRecipients"

