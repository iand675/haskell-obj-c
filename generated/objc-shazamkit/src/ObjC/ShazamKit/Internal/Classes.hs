{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ShazamKit.Internal.Classes (
    module ObjC.ShazamKit.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SHCatalog ----------

-- | An abstract base class for storing reference signatures and their associated metadata.
--
-- This is the base class of your custom catalog.
-- 
-- Phantom type for @SHCatalog@.
data SHCatalog

instance IsObjCObject (Id SHCatalog) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHCatalog"

class IsNSObject a => IsSHCatalog a where
  toSHCatalog :: a -> Id SHCatalog

instance IsSHCatalog (Id SHCatalog) where
  toSHCatalog = unsafeCastId

instance IsNSObject (Id SHCatalog) where
  toNSObject = unsafeCastId

-- ---------- SHMatch ----------

-- | An object that represents the catalog media items that match a query.
--
-- A single query signature may match more than one reference signature. In addition, one reference signature may map to many media items.
-- 
-- Phantom type for @SHMatch@.
data SHMatch

instance IsObjCObject (Id SHMatch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHMatch"

class IsNSObject a => IsSHMatch a where
  toSHMatch :: a -> Id SHMatch

instance IsSHMatch (Id SHMatch) where
  toSHMatch = unsafeCastId

instance IsNSObject (Id SHMatch) where
  toNSObject = unsafeCastId

-- ---------- SHMediaItem ----------

-- | An object that represents the metadata for a reference signature.
--
-- This class uses subscripting for the data elements of a custom media item that an existing property doesn't already represent.
--
-- Add a readable custom property by extending ``SHMediaItemProperty-struct``  with a key for that property, and by extending this class with a property that uses the key. The following code shows the extensions for an episode number:
--
-- ```swift // Add an episode number to the list of properties. extension SHMediaItemProperty {     static let episode = SHMediaItemProperty("Episode") }
--
-- // Add a property for returning the episode number using a subscript. extension SHMediaItem {     var episode: Int? {         return self[.episode] as? Int     } } ```
--
-- Add your custom property when you create the media item as the following code shows:
--
-- ```swift // Create a new media item and set the title, subtitle, and episode properties. let mediaItem = SHMediaItem(properties: [.episode: 42,                                          .title: "Question",                                          .subtitle: "The Answer"]) ```
--
-- > Note: > The class of the object that represents a custom object must be one of: @Dictionary@, @Array@, @URL@, @Number@, @String@, @Date@, or @Data@.
-- 
-- Phantom type for @SHMediaItem@.
data SHMediaItem

instance IsObjCObject (Id SHMediaItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHMediaItem"

class IsNSObject a => IsSHMediaItem a where
  toSHMediaItem :: a -> Id SHMediaItem

instance IsSHMediaItem (Id SHMediaItem) where
  toSHMediaItem = unsafeCastId

instance IsNSObject (Id SHMediaItem) where
  toNSObject = unsafeCastId

-- ---------- SHMediaLibrary ----------

-- | An object that represents the user's Shazam library.
--
-- Use @SHMediaLibrary@ to add matched songs from the Shazam catalog to the user's Shazam library.
--
-- > Note: > There's no system permission necessary to write to the user's Shazam library. Consider requesting permission from the user before adding songs to the library.
-- 
-- Phantom type for @SHMediaLibrary@.
data SHMediaLibrary

instance IsObjCObject (Id SHMediaLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHMediaLibrary"

class IsNSObject a => IsSHMediaLibrary a where
  toSHMediaLibrary :: a -> Id SHMediaLibrary

instance IsSHMediaLibrary (Id SHMediaLibrary) where
  toSHMediaLibrary = unsafeCastId

instance IsNSObject (Id SHMediaLibrary) where
  toNSObject = unsafeCastId

-- ---------- SHRange ----------

-- | A half-open interval from a lower bound up to, but not including, an upper bound.
-- 
-- Phantom type for @SHRange@.
data SHRange

instance IsObjCObject (Id SHRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHRange"

class IsNSObject a => IsSHRange a where
  toSHRange :: a -> Id SHRange

instance IsSHRange (Id SHRange) where
  toSHRange = unsafeCastId

instance IsNSObject (Id SHRange) where
  toNSObject = unsafeCastId

-- ---------- SHSession ----------

-- | An object that matches a specific audio recording when a segment of that recording is part of captured sound in the Shazam catalog or your custom catalog.
--
-- Prepare to make matches by:
--
-- - Creating a session for the catalog that contains the reference signatures - Adding your delegate that receives the match results
--
-- Search for a match in one of two ways:
--
-- - Generate a signature for the captured audio and call ``match(_:)`` - Call ``matchStreamingBuffer(_:at:)`` with a streaming audio buffer, and ShazamKit generates the signature for you
--
-- Searching the catalog is asynchronous. The session calls your delegate methods with the result.
--
-- Matching songs in Shazam music requires enabling your app to access the catalog. For more information on enabling your app, see [Enable ShazamKit for an App ID](https://developer.apple.com/help/account/configure-app-services/shazamkit).
--
-- The code below shows searching for a match in the Shazam catalog using an existing audio buffer:
--
-- ```swift// Set up the session.let session = SHSession()
--
-- // Create a signature from the captured audio buffer.let signatureGenerator = SHSignatureGenerator()try signatureGenerator.append(buffer, at: audioTime)let signature = signatureGenerator.signature()
--
-- // Check for a match.let result = await session.result(from: signature)
--
-- // Use the result.switch result {  case .match(let match):       // Match found.  case .noMatch(let signature):       // No match found.  case .error(let error, let signature):       // An error occurred.} ```
-- 
-- Phantom type for @SHSession@.
data SHSession

instance IsObjCObject (Id SHSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHSession"

class IsNSObject a => IsSHSession a where
  toSHSession :: a -> Id SHSession

instance IsSHSession (Id SHSession) where
  toSHSession = unsafeCastId

instance IsNSObject (Id SHSession) where
  toNSObject = unsafeCastId

-- ---------- SHSignature ----------

-- | An object that contains the opaque data and other information for a signature.
--
-- Save your signature to a file and share it with others by writing the data to a file. You can use the saved signatures of reference recordings to populate a custom catalog.
--
-- Check whether your captured query signature is long enough to search for a match by comparing ``duration`` to the ``SHCatalog/minimumQuerySignatureDuration`` and ``SHCatalog/maximumQuerySignatureDuration`` of a catalog.
-- 
-- Phantom type for @SHSignature@.
data SHSignature

instance IsObjCObject (Id SHSignature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHSignature"

class IsNSObject a => IsSHSignature a where
  toSHSignature :: a -> Id SHSignature

instance IsSHSignature (Id SHSignature) where
  toSHSignature = unsafeCastId

instance IsNSObject (Id SHSignature) where
  toNSObject = unsafeCastId

-- ---------- SHSignatureGenerator ----------

-- | An object for converting audio data into a signature.
--
-- Create both reference and query signatures using this class.
-- 
-- Phantom type for @SHSignatureGenerator@.
data SHSignatureGenerator

instance IsObjCObject (Id SHSignatureGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHSignatureGenerator"

class IsNSObject a => IsSHSignatureGenerator a where
  toSHSignatureGenerator :: a -> Id SHSignatureGenerator

instance IsSHSignatureGenerator (Id SHSignatureGenerator) where
  toSHSignatureGenerator = unsafeCastId

instance IsNSObject (Id SHSignatureGenerator) where
  toNSObject = unsafeCastId

-- ---------- SHCustomCatalog ----------

-- | An object for storing the reference signatures for custom audio recordings and their associated metadata.
--
-- Create a custom catalog by adding reference signatures that you generate from audio that you provide. You also add the associated metadata for each signature. Save your custom catalog and share it with others. You can also load a saved catalog.
-- 
-- Phantom type for @SHCustomCatalog@.
data SHCustomCatalog

instance IsObjCObject (Id SHCustomCatalog) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHCustomCatalog"

class IsSHCatalog a => IsSHCustomCatalog a where
  toSHCustomCatalog :: a -> Id SHCustomCatalog

instance IsSHCustomCatalog (Id SHCustomCatalog) where
  toSHCustomCatalog = unsafeCastId

instance IsNSObject (Id SHCustomCatalog) where
  toNSObject = unsafeCastId

instance IsSHCatalog (Id SHCustomCatalog) where
  toSHCatalog = unsafeCastId

-- ---------- SHMatchedMediaItem ----------

-- | An object that represents the metadata for a matched reference signature.
--
-- To access properties for custom media items, use subscripting. For more information, see ``SHMediaItem``.
-- 
-- Phantom type for @SHMatchedMediaItem@.
data SHMatchedMediaItem

instance IsObjCObject (Id SHMatchedMediaItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SHMatchedMediaItem"

class IsSHMediaItem a => IsSHMatchedMediaItem a where
  toSHMatchedMediaItem :: a -> Id SHMatchedMediaItem

instance IsSHMatchedMediaItem (Id SHMatchedMediaItem) where
  toSHMatchedMediaItem = unsafeCastId

instance IsNSObject (Id SHMatchedMediaItem) where
  toNSObject = unsafeCastId

instance IsSHMediaItem (Id SHMatchedMediaItem) where
  toSHMediaItem = unsafeCastId
