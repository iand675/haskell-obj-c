{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains metadata about a URL.
--
-- Use ``LPLinkMetadata`` to store the metadata about a URL, including its title, icon, images and video.
--
-- Fetch metadata using ``LPMetadataProvider``. For remote URLs, cache the metadata locally to avoid the data and performance cost of fetching it from the internet every time you present it. ``LPLinkMetadata`` is serializable with <doc://com.apple.documentation/documentation/foundation/nssecurecoding>.
--
-- For local file URLs, the <doc://com.apple.documentation/documentation/quicklookthumbnailing> API retrieves a representative thumbnail for the file, if possible.
--
-- ## Provide custom metadata
--
-- Say your app already has a database of links, with titles and images that weren’t fetched by ``LPMetadataProvider``. You don’t have to fetch new metadata from the internet in order to accelerate the share sheet or to present a rich link. Instead, you can fill in the fields of ``LPLinkMetadata`` yourself.
--
-- Create an ``LPLinkMetadata`` object, and fill in at least the ``LPLinkMetadata/originalURL`` and ``LPLinkMetadata/URL`` fields, plus whatever additional information you have.
--
-- ```swift func activityViewControllerLinkMetadata(_: UIActivityViewController) -> LPLinkMetadata? {     let metadata = LPLinkMetadata()     metadata.originalURL = URL(string: "https://www.example.com/apple-pie")     metadata.url = metadata.originalURL     metadata.title = "The Greatest Apple Pie In The World"     metadata.imageProvider = NSItemProvider.init(contentsOf:         Bundle.main.url(forResource: "apple-pie", withExtension: "jpg"))     return metadata } ```
--
-- ## Accelerate the share sheet preview
--
-- For existing apps that share URLs, the share sheet automatically presents a preview of the link. The preview first shows a placeholder link icon alongside the base URL while fetching the link’s metadata over the network. The preview updates once the link’s icon and title become available.
--
-- If you already have an ``LPLinkMetadata`` object for a URL, pass it to the share sheet to present the preview instantly, without fetching data over the network. In your implementation of <doc://com.apple.documentation/documentation/uikit/uiactivityitemsource/3144571-activityviewcontrollerlinkmetada>, return the metadata object.
--
-- ```swift func activityViewControllerLinkMetadata(_: UIActivityViewController) -> LPLinkMetadata? {     return self.metadata } ```
--
-- If the user chooses to share to Messages, the same metadata passes directly through, providing a smooth and seamless experience with no unnecessary loading.
--
-- Generated bindings for @LPLinkMetadata@.
module ObjC.LinkPresentation.LPLinkMetadata
  ( LPLinkMetadata
  , IsLPLinkMetadata(..)
  , originalURL
  , setOriginalURL
  , url
  , setURL
  , title
  , setTitle
  , iconProvider
  , setIconProvider
  , imageProvider
  , setImageProvider
  , videoProvider
  , setVideoProvider
  , remoteVideoURL
  , setRemoteVideoURL
  , iconProviderSelector
  , imageProviderSelector
  , originalURLSelector
  , remoteVideoURLSelector
  , setIconProviderSelector
  , setImageProviderSelector
  , setOriginalURLSelector
  , setRemoteVideoURLSelector
  , setTitleSelector
  , setURLSelector
  , setVideoProviderSelector
  , titleSelector
  , urlSelector
  , videoProviderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LinkPresentation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The original URL of the metadata request.
--
-- ObjC selector: @- originalURL@
originalURL :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSURL)
originalURL lpLinkMetadata =
  sendMessage lpLinkMetadata originalURLSelector

-- | The original URL of the metadata request.
--
-- ObjC selector: @- setOriginalURL:@
setOriginalURL :: (IsLPLinkMetadata lpLinkMetadata, IsNSURL value) => lpLinkMetadata -> value -> IO ()
setOriginalURL lpLinkMetadata value =
  sendMessage lpLinkMetadata setOriginalURLSelector (toNSURL value)

-- | The URL that returned the metadata, taking server-side redirects into account.
--
-- The URL that returns the metadata may differ from the ``LPLinkMetadata/originalURL`` to which you sent the metadata request. This can happen if the server redirects the request, for example, when a resource has moved, or when the original URL is a domain alias.
--
-- ObjC selector: @- URL@
url :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSURL)
url lpLinkMetadata =
  sendMessage lpLinkMetadata urlSelector

-- | The URL that returned the metadata, taking server-side redirects into account.
--
-- The URL that returns the metadata may differ from the ``LPLinkMetadata/originalURL`` to which you sent the metadata request. This can happen if the server redirects the request, for example, when a resource has moved, or when the original URL is a domain alias.
--
-- ObjC selector: @- setURL:@
setURL :: (IsLPLinkMetadata lpLinkMetadata, IsNSURL value) => lpLinkMetadata -> value -> IO ()
setURL lpLinkMetadata value =
  sendMessage lpLinkMetadata setURLSelector (toNSURL value)

-- | A representative title for the URL.
--
-- ObjC selector: @- title@
title :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSString)
title lpLinkMetadata =
  sendMessage lpLinkMetadata titleSelector

-- | A representative title for the URL.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsLPLinkMetadata lpLinkMetadata, IsNSString value) => lpLinkMetadata -> value -> IO ()
setTitle lpLinkMetadata value =
  sendMessage lpLinkMetadata setTitleSelector (toNSString value)

-- | An object that retrieves data corresponding to a representative icon for the URL.
--
-- ObjC selector: @- iconProvider@
iconProvider :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSItemProvider)
iconProvider lpLinkMetadata =
  sendMessage lpLinkMetadata iconProviderSelector

-- | An object that retrieves data corresponding to a representative icon for the URL.
--
-- ObjC selector: @- setIconProvider:@
setIconProvider :: (IsLPLinkMetadata lpLinkMetadata, IsNSItemProvider value) => lpLinkMetadata -> value -> IO ()
setIconProvider lpLinkMetadata value =
  sendMessage lpLinkMetadata setIconProviderSelector (toNSItemProvider value)

-- | An object that retrieves data corresponding to a representative image for the URL.
--
-- ObjC selector: @- imageProvider@
imageProvider :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSItemProvider)
imageProvider lpLinkMetadata =
  sendMessage lpLinkMetadata imageProviderSelector

-- | An object that retrieves data corresponding to a representative image for the URL.
--
-- ObjC selector: @- setImageProvider:@
setImageProvider :: (IsLPLinkMetadata lpLinkMetadata, IsNSItemProvider value) => lpLinkMetadata -> value -> IO ()
setImageProvider lpLinkMetadata value =
  sendMessage lpLinkMetadata setImageProviderSelector (toNSItemProvider value)

-- | An object that retrieves data corresponding to a representative video for the URL.
--
-- The item provider returns a video that <doc://com.apple.documentation/documentation/avfoundation> can play.
--
-- ObjC selector: @- videoProvider@
videoProvider :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSItemProvider)
videoProvider lpLinkMetadata =
  sendMessage lpLinkMetadata videoProviderSelector

-- | An object that retrieves data corresponding to a representative video for the URL.
--
-- The item provider returns a video that <doc://com.apple.documentation/documentation/avfoundation> can play.
--
-- ObjC selector: @- setVideoProvider:@
setVideoProvider :: (IsLPLinkMetadata lpLinkMetadata, IsNSItemProvider value) => lpLinkMetadata -> value -> IO ()
setVideoProvider lpLinkMetadata value =
  sendMessage lpLinkMetadata setVideoProviderSelector (toNSItemProvider value)

-- | A remote URL corresponding to a representative video for the URL.
--
-- This may reference a remote video file that <doc://com.apple.documentation/documentation/avfoundation> can stream, or a YouTube video URL.
--
-- ObjC selector: @- remoteVideoURL@
remoteVideoURL :: IsLPLinkMetadata lpLinkMetadata => lpLinkMetadata -> IO (Id NSURL)
remoteVideoURL lpLinkMetadata =
  sendMessage lpLinkMetadata remoteVideoURLSelector

-- | A remote URL corresponding to a representative video for the URL.
--
-- This may reference a remote video file that <doc://com.apple.documentation/documentation/avfoundation> can stream, or a YouTube video URL.
--
-- ObjC selector: @- setRemoteVideoURL:@
setRemoteVideoURL :: (IsLPLinkMetadata lpLinkMetadata, IsNSURL value) => lpLinkMetadata -> value -> IO ()
setRemoteVideoURL lpLinkMetadata value =
  sendMessage lpLinkMetadata setRemoteVideoURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @originalURL@
originalURLSelector :: Selector '[] (Id NSURL)
originalURLSelector = mkSelector "originalURL"

-- | @Selector@ for @setOriginalURL:@
setOriginalURLSelector :: Selector '[Id NSURL] ()
setOriginalURLSelector = mkSelector "setOriginalURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @iconProvider@
iconProviderSelector :: Selector '[] (Id NSItemProvider)
iconProviderSelector = mkSelector "iconProvider"

-- | @Selector@ for @setIconProvider:@
setIconProviderSelector :: Selector '[Id NSItemProvider] ()
setIconProviderSelector = mkSelector "setIconProvider:"

-- | @Selector@ for @imageProvider@
imageProviderSelector :: Selector '[] (Id NSItemProvider)
imageProviderSelector = mkSelector "imageProvider"

-- | @Selector@ for @setImageProvider:@
setImageProviderSelector :: Selector '[Id NSItemProvider] ()
setImageProviderSelector = mkSelector "setImageProvider:"

-- | @Selector@ for @videoProvider@
videoProviderSelector :: Selector '[] (Id NSItemProvider)
videoProviderSelector = mkSelector "videoProvider"

-- | @Selector@ for @setVideoProvider:@
setVideoProviderSelector :: Selector '[Id NSItemProvider] ()
setVideoProviderSelector = mkSelector "setVideoProvider:"

-- | @Selector@ for @remoteVideoURL@
remoteVideoURLSelector :: Selector '[] (Id NSURL)
remoteVideoURLSelector = mkSelector "remoteVideoURL"

-- | @Selector@ for @setRemoteVideoURL:@
setRemoteVideoURLSelector :: Selector '[Id NSURL] ()
setRemoteVideoURLSelector = mkSelector "setRemoteVideoURL:"

