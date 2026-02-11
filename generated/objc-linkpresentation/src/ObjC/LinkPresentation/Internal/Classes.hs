{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.LinkPresentation.Internal.Classes (
    module ObjC.LinkPresentation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- LPLinkMetadata ----------

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
-- Phantom type for @LPLinkMetadata@.
data LPLinkMetadata

instance IsObjCObject (Id LPLinkMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LPLinkMetadata"

class IsNSObject a => IsLPLinkMetadata a where
  toLPLinkMetadata :: a -> Id LPLinkMetadata

instance IsLPLinkMetadata (Id LPLinkMetadata) where
  toLPLinkMetadata = unsafeCastId

instance IsNSObject (Id LPLinkMetadata) where
  toNSObject = unsafeCastId

-- ---------- LPMetadataProvider ----------

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
-- Phantom type for @LPMetadataProvider@.
data LPMetadataProvider

instance IsObjCObject (Id LPMetadataProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LPMetadataProvider"

class IsNSObject a => IsLPMetadataProvider a where
  toLPMetadataProvider :: a -> Id LPMetadataProvider

instance IsLPMetadataProvider (Id LPMetadataProvider) where
  toLPMetadataProvider = unsafeCastId

instance IsNSObject (Id LPMetadataProvider) where
  toNSObject = unsafeCastId

-- ---------- LPLinkView ----------

-- | A rich visual representation of a link.
--
-- ``LPLinkView`` presents a link based on its available metadata. Use it to show a link’s title and icon, associated images, inline audio, video playback, and maps in a familiar and consistent style.
--
-- ## Present a rich link
--
-- To present a rich link in your app, create an ``LPLinkView``, passing an ``LPLinkMetadata`` instance into its initializer. Then add the ``LPLinkView`` to your view.
--
-- For example, to present links in a table view, add an ``LPLinkView`` instance as a subview when populating each cell.
--
-- ```swift let linkView = LPLinkView(metadata: metadata) cell.contentView.addSubview(linkView) linkView.sizeToFit() ```
--
-- ``LPLinkView`` has an intrinsic size, but it also responds to <doc://com.apple.documentation/documentation/uikit/uiview/1622630-sizetofit> to present a layout at any size.
-- 
-- Phantom type for @LPLinkView@.
data LPLinkView

instance IsObjCObject (Id LPLinkView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LPLinkView"

class IsNSView a => IsLPLinkView a where
  toLPLinkView :: a -> Id LPLinkView

instance IsLPLinkView (Id LPLinkView) where
  toLPLinkView = unsafeCastId

instance IsNSObject (Id LPLinkView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id LPLinkView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id LPLinkView) where
  toNSView = unsafeCastId
