{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that retrieves metadata for a URL.
--
-- Use ``LPMetadataProvider`` to fetch metadata for a URL, including its title, icon, and image or video links. All properties on the resulting ``LPLinkMetadata`` instance are optional.
--
-- - Note: To enable macOS clients to fetch metadata for remote URLs, add the <doc://com.apple.documentation/documentation/bundleresources/entitlements/com_apple_security_network_client> entitlement.
--
-- ## Fetch link metadata from a URL
--
-- For each metadata request, create an instance of ``LPMetadataProvider`` and call ``LPMetadataProvider/startFetchingMetadataForURL:completionHandler:``.
--
-- In the completion handler, check the error. If your user doesn’t have a network connection, the fetch can fail. If the server doesn’t respond or is too slow, the fetch can time out. Alternatively, the app may cancel the request, or an unknown error may occur.
--
-- Otherwise, use the metadata however you want, for example, to populate the title for a table view cell.
--
-- ```swift let metadataProvider = LPMetadataProvider() let url = URL(string: "https://www.apple.com/ipad")!
--
-- metadataProvider.startFetchingMetadata(for: url) { metadata, error in     if error != nil {         // The fetch failed; handle the error.         return     }
--
-- // Make use of fetched metadata. } ```
--
-- For more information about handling errors, see ``LinkPresentation/LPError``.
--
-- Generated bindings for @LPMetadataProvider@.
module ObjC.LinkPresentation.LPMetadataProvider
  ( LPMetadataProvider
  , IsLPMetadataProvider(..)
  , startFetchingMetadataForURL_completionHandler
  , startFetchingMetadataForRequest_completionHandler
  , cancel
  , shouldFetchSubresources
  , setShouldFetchSubresources
  , timeout
  , setTimeout
  , cancelSelector
  , setShouldFetchSubresourcesSelector
  , setTimeoutSelector
  , shouldFetchSubresourcesSelector
  , startFetchingMetadataForRequest_completionHandlerSelector
  , startFetchingMetadataForURL_completionHandlerSelector
  , timeoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LinkPresentation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Fetches metadata for the given URL.
--
-- Call this method once per ``LPMetadataProvider`` instance. If you attempt to fetch metadata multiple times on a single ``LPMetadataProvider`` instance, it throws an error.
--
-- The completion handler executes on a background queue. Dispatch any necessary UI updates back to the main queue. When the completion handler returns, it deletes any file URLs returned in the resulting ``LPLinkMetadata``.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift >  func startFetchingMetadata(for url: URL) async throws -> LPLinkMetadata > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- startFetchingMetadataForURL:completionHandler:@
startFetchingMetadataForURL_completionHandler :: (IsLPMetadataProvider lpMetadataProvider, IsNSURL url) => lpMetadataProvider -> url -> Ptr () -> IO ()
startFetchingMetadataForURL_completionHandler lpMetadataProvider url completionHandler =
  sendMessage lpMetadataProvider startFetchingMetadataForURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Fetches metadata for the given ``NSURLRequest``.
--
-- Call this method once per ``LPMetadataProvider`` instance. If you attempt to fetch metadata multiple times on a single ``LPMetadataProvider`` instance, it throws an error.
--
-- The completion handler executes on a background queue. Dispatch any necessary UI updates back to the main queue. When the completion handler returns, it deletes any file URLs returned in the resulting ``LPLinkMetadata``.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift >  func startFetchingMetadata(for request: URLRequest) async throws -> LPLinkMetadata > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- startFetchingMetadataForRequest:completionHandler:@
startFetchingMetadataForRequest_completionHandler :: (IsLPMetadataProvider lpMetadataProvider, IsNSURLRequest request) => lpMetadataProvider -> request -> Ptr () -> IO ()
startFetchingMetadataForRequest_completionHandler lpMetadataProvider request completionHandler =
  sendMessage lpMetadataProvider startFetchingMetadataForRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | Cancels a metadata request.
--
-- This method invokes the completion handler with the error code ``LPErrorCode/LPErrorMetadataFetchCancelled`` if the request hasn’t already completed.
--
-- ObjC selector: @- cancel@
cancel :: IsLPMetadataProvider lpMetadataProvider => lpMetadataProvider -> IO ()
cancel lpMetadataProvider =
  sendMessage lpMetadataProvider cancelSelector

-- | A Boolean value indicating whether to download subresources specified by the metadata.
--
-- Subresources include the icon, image, or video. When set to @false@, the returned ``LPLinkMetadata`` object consists only of metadata retrieved from the main resource identified by the url passed to ``LPMetadataProvider/startFetchingMetadataForURL:completionHandler:``.
--
-- The default value is @true@.
--
-- ObjC selector: @- shouldFetchSubresources@
shouldFetchSubresources :: IsLPMetadataProvider lpMetadataProvider => lpMetadataProvider -> IO Bool
shouldFetchSubresources lpMetadataProvider =
  sendMessage lpMetadataProvider shouldFetchSubresourcesSelector

-- | A Boolean value indicating whether to download subresources specified by the metadata.
--
-- Subresources include the icon, image, or video. When set to @false@, the returned ``LPLinkMetadata`` object consists only of metadata retrieved from the main resource identified by the url passed to ``LPMetadataProvider/startFetchingMetadataForURL:completionHandler:``.
--
-- The default value is @true@.
--
-- ObjC selector: @- setShouldFetchSubresources:@
setShouldFetchSubresources :: IsLPMetadataProvider lpMetadataProvider => lpMetadataProvider -> Bool -> IO ()
setShouldFetchSubresources lpMetadataProvider value =
  sendMessage lpMetadataProvider setShouldFetchSubresourcesSelector value

-- | The time interval after which the request automatically fails if it hasn’t already completed.
--
-- The default timeout interval is 30 seconds. If a metadata fetch takes longer than the timeout interval, the completion handler is called with the error code ``LPErrorCode/LPErrorMetadataFetchTimedOut``.
--
-- ObjC selector: @- timeout@
timeout :: IsLPMetadataProvider lpMetadataProvider => lpMetadataProvider -> IO CDouble
timeout lpMetadataProvider =
  sendMessage lpMetadataProvider timeoutSelector

-- | The time interval after which the request automatically fails if it hasn’t already completed.
--
-- The default timeout interval is 30 seconds. If a metadata fetch takes longer than the timeout interval, the completion handler is called with the error code ``LPErrorCode/LPErrorMetadataFetchTimedOut``.
--
-- ObjC selector: @- setTimeout:@
setTimeout :: IsLPMetadataProvider lpMetadataProvider => lpMetadataProvider -> CDouble -> IO ()
setTimeout lpMetadataProvider value =
  sendMessage lpMetadataProvider setTimeoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startFetchingMetadataForURL:completionHandler:@
startFetchingMetadataForURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
startFetchingMetadataForURL_completionHandlerSelector = mkSelector "startFetchingMetadataForURL:completionHandler:"

-- | @Selector@ for @startFetchingMetadataForRequest:completionHandler:@
startFetchingMetadataForRequest_completionHandlerSelector :: Selector '[Id NSURLRequest, Ptr ()] ()
startFetchingMetadataForRequest_completionHandlerSelector = mkSelector "startFetchingMetadataForRequest:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @shouldFetchSubresources@
shouldFetchSubresourcesSelector :: Selector '[] Bool
shouldFetchSubresourcesSelector = mkSelector "shouldFetchSubresources"

-- | @Selector@ for @setShouldFetchSubresources:@
setShouldFetchSubresourcesSelector :: Selector '[Bool] ()
setShouldFetchSubresourcesSelector = mkSelector "setShouldFetchSubresources:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector '[] CDouble
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector '[CDouble] ()
setTimeoutSelector = mkSelector "setTimeout:"

